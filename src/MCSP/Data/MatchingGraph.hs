-- | Working with edges of a matching graph.
module MCSP.Data.MatchingGraph (
    -- * Finding Edges
    Edge,
    edgeSet,

    -- * Building Solutions
    Solution,
    solution,
    solutions,

    -- * Restoring Partitions
    mergeness,
    blockCount,
    toPartitions,
) where

import Data.Bool (Bool, not, (&&))
import Data.Eq (Eq (..))
import Data.Foldable (length, null)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.IntMap.Strict qualified as IntMap (IntMap, insert, size, toDescList)
import Data.Interval (Interval, (<=..<))
import Data.IntervalSet (IntervalSet, insert, intersection)
import Data.IntervalSet qualified as IntervalSet (null, singleton)
import Data.List (concatMap, map, takeWhile)
import Data.List.NonEmpty (NonEmpty (..), unfoldr)
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid (..), mappend, mempty)
import Data.Ord (Ord (..))
import Data.Vector.Generic qualified as Vector (foldl', length, snoc)
import Data.Vector.Unboxed (Vector)
import GHC.IsList (IsList (..))
import GHC.Num (fromInteger, (+), (-))
import GHC.Real (toInteger)
import Text.Show (Show)

import MCSP.Data.Pair (Pair, both, cartesian, left, liftP, right, ($:), (&&&))
import MCSP.Data.String (String (..), slice, unsafeSlice)
import MCSP.Data.String.Extra (Partition, chars)

-- --------------------- --
-- Edge Set Construction --
-- --------------------- --

-- | Represents a position of a block or a character.
type Index = Int

-- | Represents the length of a block.
type Length = Int

-- | A single edge in the matching graph for a pair of strings.
--
-- An edge @((s, p), k)@ represents a common subtring @S[s .. s + k - 1] = P[p .. p + k - 1]@ of
-- length @k@ that can be used as a block for partitions.
type Edge = (Pair Index, Length)

-- Note: `Edge` is implemented as a tuple so we don't need to derive anything, but we should use
-- the pattern below.

{-# COMPLETE Edge #-}

-- | A single edge in the matching graph for a pair of strings.
--
-- An edge @`Edge` {`start` = (left, right), `blockLen`}@ represents a common subtring
-- @S[left .. left + blockLen - 1] = P[right .. right + blockLen - 1]@ of length `blockLen` that
-- can be used as a block for partitions.
pattern Edge :: Pair Index -> Length -> Edge
pattern Edge {start, blockLen} = (start, blockLen)
{-# INLINE CONLIKE Edge #-}

-- | /O(n)/ List edges starting from a position pair @(s,p)@.
--
-- >>> edgesFrom ("abab", "abba") (0,0)
-- [((0,0),2)]
edgesFrom :: Eq a => Pair (String a) -> Pair Index -> [Edge]
edgesFrom strs start = takeWhile (isCommonBlock strs) $ map (start,) [2 ..]
  where
    isCommonBlock (l, r) Edge {start = (s, p), blockLen = k} =
        s + k <= length l && p + k <= length r && unsafeSlice s k l == unsafeSlice p k r
{-# INLINEABLE edgesFrom #-}

-- | /O(n^3)/ List all edges of the matching graph for a pair of strings.
--
-- >>> edgeSet ("abab", "abba")
-- [((0,0),2),((1,2),2),((2,0),2)]
edgeSet :: Eq a => Pair (String a) -> Vector Edge
edgeSet (l, r) = toVector $ concatMap (edgesFrom (l, r)) start
  where
    start = cartesian [0 .. length l - 1] [0 .. length r - 1]
    toVector = fromList

-- ------------------ --
-- Building Solutions --
-- ------------------ --

-- | Indices for a subtring to be used as a block.
type Block = Interval Index

-- | Creates an interval representing a substring @S[i .. i + n - 1]@.
--
-- >>> blockInterval 1 4
-- Finite 1 <=..< Finite 5
blockInterval :: Index -> Length -> Block
blockInterval lo len = extend lo <=..< extend (lo + len)
  where
    extend = fromInteger . toInteger

-- | Extract the interval for the associated blocks in each string from an edge.
--
-- >>> toBlocks Edge {start=(0, 2), blockLen=5}
-- (Finite 0 <=..< Finite 5,Finite 2 <=..< Finite 7)
toBlocks :: Edge -> Pair Block
toBlocks Edge {start = (s, p), blockLen = k} = (blockInterval s k, blockInterval p k)

-- | The set of all matched regions of string.
--
-- Used to avoid overlapping blocks in the final partition.
type MatchedSet = IntervalSet Index

-- | Checks if a block was already matched in the current solution.
--
-- >>> let edge = Edge {start=(0,2), blockLen=5}
-- >>> blockInterval 0 5 `overlaps` mempty
-- False
-- >>> blockInterval 0 5 `overlaps` IntervalSet.singleton (blockInterval 5 10)
-- False
-- >>> blockInterval 0 5 `overlaps` IntervalSet.singleton (blockInterval 4 5)
-- True
overlaps :: Block -> MatchedSet -> Bool
interval `overlaps` set = not $ IntervalSet.null (IntervalSet.singleton interval `intersection` set)

-- | Turn an edge into a non-overlapping interval pair.
--
-- Returns `Nothing` if any of the interval would overlap.
--
-- >>> let edge = Edge {start=(0,2), blockLen=5}
-- >>> nonOverlappingBlock edge mempty
-- Just (Finite 0 <=..< Finite 5,Finite 2 <=..< Finite 7)
-- >>> nonOverlappingBlock edge (IntervalSet.singleton `both` (toBlocks edge))
-- Nothing
nonOverlappingBlock :: Edge -> Pair MatchedSet -> Maybe (Pair Block)
nonOverlappingBlock edge matched =
    if not (left block `overlaps` left matched) && not (right block `overlaps` right matched)
        then Just block
        else Nothing
  where
    block = toBlocks edge

-- | A partial solution to the strings partitioning.
--
-- Each @(k,v)@ pair in the map represents a block @S[k ... k + v]@ that should be used in the final
-- solution.
type IndexedPartition = IntMap.IntMap Length

-- | Insert an edge to the final solution, without checking for overlapping blocks.
insertInPartition :: Edge -> Pair IndexedPartition -> Pair IndexedPartition
insertInPartition edge = liftP (`IntMap.insert` blockLen edge) (start edge)

-- | A collection of data used for constructing solutions from an edge set.
data MatchingInfo = Info
    { -- | Collection of blocks from non-overlapping edges, used as a solution.
      partition :: Pair IndexedPartition,
      -- | Set of ranges matched in each string.
      matchedSet :: Pair MatchedSet,
      -- | List of edges not used in the solution.
      unused :: Vector Edge
    }
    deriving stock (Show)

-- | Representation of a trivial solution.
--
-- >>> empty
-- Info {partition = (fromList [],fromList []), matchedSet = (fromList [],fromList []), unused = []}
empty :: MatchingInfo
empty = Info {partition = mempty, matchedSet = mempty, unused = []}

-- | Constructs a partial solution from a list of edges, collecting the `unused` edges.
--
-- >>> resolve [Edge {start=(0,2), blockLen=5}]
-- Info {partition = (fromList [(0,5)],fromList [(2,5)]), matchedSet = (fromList [Finite 0 <=..< Finite 5],fromList [Finite 2 <=..< Finite 7]), unused = []}
resolve :: Vector Edge -> MatchingInfo
resolve = Vector.foldl' addEdge empty
  where
    addEdge Info {..} edge = case nonOverlappingBlock edge matchedSet of
        -- non overlapping edge, add to solution
        Just blocks ->
            Info
                { partition = insertInPartition edge partition,
                  matchedSet = liftP insert blocks matchedSet,
                  unused
                }
        -- edge overlaps, add to unused
        Nothing ->
            Info
                { partition,
                  matchedSet,
                  unused = Vector.snoc unused edge
                }

-- | Construct another solution with the `unused` edges of the previous solution.
--
-- Return `Nothing` for any trivial solution.
--
-- >>> nextSolution (resolve [Edge {start=(0,2), blockLen=5}])
-- Just (Info {partition = (fromList [],fromList []), matchedSet = (fromList [],fromList []), unused = []})
-- >>> nextSolution empty
-- Nothing
nextSolution :: MatchingInfo -> Maybe MatchingInfo
nextSolution Info {..} =
    if null (left partition) && null (right partition)
        then Nothing
        else Just (resolve unused)

-- | A complete solution to the strings partitioning.
--
-- Each @(k,v)@ pair in the solution represents a block @S[k ... k + v]@.
type Solution = Pair (Vector (Index, Length))

-- | Extract the solution from the `MatchingInfo`.
toSolution :: MatchingInfo -> Solution
toSolution Info {..} = toSolutionVector `both` partition
  where
    toSolutionVector part = fromListN (IntMap.size part) (IntMap.toDescList part)

-- | The solution represented by the edge list.
--
-- Apply each edge in the same order they are listed, ignoring edges that overlaps with the partial
-- solution.
--
-- >>> solution $ edgeSet ("abab", "abba")
-- ([(0,2)],[(0,2)])
-- >>> solution $ edgeSet ("abab", "abab")
-- ([(2,2),(0,2)],[(2,2),(0,2)])
solution :: Vector Edge -> Solution
solution = toSolution . resolve

-- | List of all solutions represented with an ordering of the edge set.
--
-- This is done by reapeatedly constructing `solution`s with the unused edges from the previous
-- `solution`.
--
-- >>> solutions $ edgeSet ("abab", "abba")
-- ([(0,2)],[(0,2)]) :| [([(1,2)],[(2,2)]),([(2,2)],[(0,2)]),([],[])]
-- >>> solutions $ edgeSet ("abab", "abab")
-- ([(2,2),(0,2)],[(2,2),(0,2)]) :| [([(0,3)],[(0,3)]),([(0,4)],[(0,4)]),([(2,2),(0,2)],[(2,2),(0,2)]),([(1,2)],[(1,2)]),([(1,3)],[(1,3)]),([],[])]
solutions :: Vector Edge -> NonEmpty Solution
solutions edges = unfoldr (toSolution &&& nextSolution) (resolve edges)

-- -------------------- --
-- Restoring Partitions --
-- -------------------- --

-- | Calculates how much the solution merges the characters of the string.
--
-- The mergeness is given by the string length minus the number of blocks.
--
-- >>> mergeness mempty
-- 0
-- >>> mergeness (solution $ edgeSet ("abab", "abba"))
-- 1
mergeness :: Solution -> Length
mergeness sol = min $: blocks `both` sol
  where
    blocks part = Vector.foldl' sum 0 part - Vector.length part
    sum total (_, len) = total + len

-- | Calculates the number of blocks the solution will create.
--
-- >>> blockCount "abab" mempty
-- 4
-- >>> blockCount "abab" (solution $ edgeSet ("abab", "abba"))
-- 3
blockCount :: String a -> Solution -> Length
blockCount str sol = length str - mergeness sol

-- | Extract the partition from a block slice map.
--
-- >>> toPartition "abcd" (fromList [(1, 2)])
-- [a,bc,d]
-- >>> toPartition "abcd" (fromList [(2, 2)])
-- [a,b,cd]
-- >>> toPartition "abcd" (fromList [(0, 2)])
-- [ab,c,d]
-- >>> toPartition "abcd" (fromList [(0, 4)])
-- [abcd]
-- >>> toPartition "abcd" (fromList [])
-- [a,b,c,d]
toPartition :: String a -> Vector (Index, Length) -> Partition a
toPartition s = concatChars 0 . Vector.foldl' insertBlock (length s, [])
  where
    insertBlock (f, p) (i, n) = (i, slice i n s : concatChars (i + n) (f, p))
    concatChars i (f, p) = chars (slice i (f - i) s) `mappend` p

-- | Construct the partitions for a solution.
--
-- >>> toPartitions ("abab", "abba") mempty
-- ([a,b,a,b],[a,b,b,a])
-- >>> toPartitions ("abab", "abba") (solution $ edgeSet ("abab", "abba"))
-- ([ab,a,b],[ab,b,a])
-- >>> toPartitions ("abba", "abab") (solution $ edgeSet ("abba", "abab"))
-- ([ab,b,a],[ab,a,b])
toPartitions :: Pair (String a) -> Solution -> Pair (Partition a)
toPartitions = liftP toPartition
