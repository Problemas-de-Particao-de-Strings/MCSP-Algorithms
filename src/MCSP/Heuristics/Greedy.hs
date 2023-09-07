-- | Greedy Heuristic for solving the MCSP problem.
module MCSP.Heuristics.Greedy (
    greedy,
) where

import Control.Applicative (pure)
import Data.Bool (otherwise)
import Data.Eq (Eq (..))
import Data.Function (on, (.))
import Data.Int (Int)
import Data.List (map)
import Data.Maybe (Maybe (..), maybe)
import Data.Ord (Ord (..))
import Data.Tuple.Extra (both, first, snd, uncurry)
import GHC.Err (errorWithoutStackTrace)
import GHC.Num ((+))
import Text.Show (Show)

import Data.IntMap.Strict (
    IntMap,
    delete,
    empty,
    foldlWithKey',
    insert,
    singleton,
    toAscList,
    union,
 )

import MCSP.Data.String (String (..), length)
import MCSP.Data.String.Extra (PartitionPair, longestCommonSubstring, stripInfix)

-- | The pair @(idx, substr)@ where @idx@ is the index where @substr@ was taken from in the
-- original string.
type IndexedString a = (Int, String a)

-- | A collection of subtrings of the same original string, indexed by their original position.
--
-- Represents a partition of the original strings, but their relative order is maintained with the
-- indexes, not by their position a list.
type IndexedPartition a = IntMap (String a)

-- | Same as `PartitionPair`, but using @IndexedPartition@s.
type IndexedPartitionPair a = (IndexedPartition a, IndexedPartition a)

-- --------------------------------------- --
-- Longest Common Substring for Partitions --

-- | The result of @lcsPair@, holding the longest common subtring of a @IndexedPartitionPair@ and
-- the pair where such substring was found.
data LCSResult a = Result
    { -- | The string from the left partition from which the LCS was taken, and its index.
      left :: {-# UNPACK #-} !(Int, String a),
      -- | The longest common subtring of the partitions. Also, the LCS for @left@ and @right@.
      lcs :: {-# UNPACK #-} !(String a),
      -- | The string from the right partition from which the LCS was taken, and its index.
      right :: {-# UNPACK #-} !(Int, String a)
    }
    deriving stock (Show)

-- | The comparison key for ordering @LCSResults@.
--
-- For two candidate results, we always prefer the one with the longest @lcs@. If that is equal for
-- both, we take the one with the shortest original strings @left@ and @right@, hoping that it will
-- leave less partitions after removing @lcs@. Otherwise, we take the result with smallest indices.
cmpKey :: LCSResult a -> (Int, Int, Int)
cmpKey Result {left = (ln, ls), lcs = common, right = (rn, rs)} =
    ( length common,
      -(length ls + length rs),
      -(ln + rn)
    )

instance Eq (LCSResult a) where
    (==) = (==) `on` cmpKey
    (/=) = (/=) `on` cmpKey

instance Ord (LCSResult a) where
    compare = compare `on` cmpKey
    (<=) = (<=) `on` cmpKey
    (<) = (<) `on` cmpKey
    (>) = (>) `on` cmpKey
    (>=) = (>=) `on` cmpKey

-- | Returns the longest common subtring of a @IndexedPartitionPair@ and the pair where such
-- substring was found. Returns `Nothing` if no common substring can be found.
lcsPair :: Ord a => IndexedPartitionPair a -> Maybe (LCSResult a)
lcsPair (xs, ys) = foldlWithKey' (lcsPairWith ys) Nothing xs
  where
    lcsPairWith rhs res n x
        | longerResult res x = res
        | otherwise = foldlWithKey' (maxLCS (n, x)) res rhs
    maxLCS l res n y
        | longerResult res y = res
        | otherwise = max res (withLCS l (n, y))
    longerResult res s = maybe 0 (length . lcs) res > length s
    withLCS l@(_, x) r@(_, y) = do
        sub <- longestCommonSubstring x y
        pure (Result {left = l, lcs = sub, right = r})

-- | Break the indexed string from a partition removing the @lcs@ from it.
--
-- Replace the string with the results from @`stripInfix` lcs s@, returning the @IndexedString@ for
-- the @lcs@, which should be collected in another @IndexedPartition@.
breakAt ::
    Eq a =>
    IndexedString a
    -> String a
    -> IndexedPartition a
    -> (IndexedString a, IndexedPartition a)
breakAt (n, v) s m = case stripInfix s v of
    Just (prefix, suffix) -> insertItems prefix suffix (n, delete n m)
    Nothing -> errorWithoutStackTrace "greedy: given LCS was not part of the input string."
  where
    -- insert each item, updating the indices if needed
    insertItems s1 s2 = first (,s) . insert2 s2 . insert1 s1
    insert1 Null pp = pp
    insert1 s1 (i, p) = (i + length s1, insert i s1 p)
    insert2 Null pp = pp
    insert2 s2 (i, p) = (i, insert (i + length s) s2 p)

-- | Find the longest common substring, remove it from the partitions and returns it with the
-- indices where to reinsert it for each partition.
extractLCS ::
    Ord a =>
    IndexedPartitionPair a
    -> Maybe (IndexedPartitionPair a, IndexedString a, IndexedString a)
extractLCS (xs, ys) = do
    Result x common y <- lcsPair (xs, ys)
    let (x', xs') = breakAt x common xs
    let (y', ys') = breakAt y common ys
    pure ((xs', ys'), x', y')

-- | Recursively run the greedy algorithm by finding the longest common substring, breaking the
-- matched subtrings and collecting the results in a two new partitions. When no common substring
-- is found, the algorithm is finished, and the result partition is merged with the remaining
-- unbroken strings.
indexedGreedy :: Ord a => IndexedPartitionPair a -> IndexedPartitionPair a
indexedGreedy = go empty empty
  where
    go rx ry (px, py) = case extractLCS (px, py) of
        Just (pp, x, y) -> go (add x rx) (add y ry) pp
        Nothing -> (rx `union` px, ry `union` py)
    add = uncurry insert

-- | MCSP greedy algorithm.
--
-- Tries to solve the MCSP by repeatedly finding the longest common substring (LCS), breaking the
-- strings with it, and inserting the LCS in the resulting partition, until no common substring is
-- left.
greedy :: Ord a => String a -> String a -> PartitionPair a
greedy s1 s2 = both sort (indexedGreedy (singleton 0 s1, singleton 0 s2))
  where
    sort = map snd . toAscList
