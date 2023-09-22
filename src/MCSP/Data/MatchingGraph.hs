-- | Working with edges of a matching graph.
module MCSP.Data.MatchingGraph (
    -- * Finding Edges
    Edge,
    edgeSet,
) where

import Control.Applicative (liftA2, pure)
import Data.Foldable (foldMap', foldr, length)
import Data.Function ((.))
import Data.Functor (fmap)
import Data.Int (Int)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Map qualified as Map (Map, alter, empty, intersectionWith)
import Data.Maybe (maybe)
import Data.Ord (Ord)
import Data.Vector.Unboxed (Vector)
import GHC.IsList (IsList (..))
import GHC.Num ((-))

import MCSP.Data.Pair (Pair)
import MCSP.Data.String (String, unsafeSlice)

-- --------------------- --
-- Edge Set Construction --
-- --------------------- --

-- | A mapping where each key has multiple values.
type MultiMap k v = Map.Map k (NonEmpty v)

-- | /O(n log n)/ Construct a multi-map collecting repeated values in a `NonEmpty` list.
--
-- >>> multiMap [('a', 1), ('b', 2), ('a', 3)]
-- fromList [('a',1 :| [3]),('b',2 :| [])]
multiMap :: Ord k => [(k, v)] -> MultiMap k v
multiMap = foldr prependAt Map.empty
  where
    prependAt (key, val) = Map.alter (prepend val) key
    prepend x xs = pure (maybe (x :| []) (x <|) xs)

-- | Represents a position of a block or a character.
type Index = Int

-- | Represents the length of a block.
type Length = Int

-- | The set of all possible blocks in a single string.
--
-- Keys are the subtrings associated with the block, and the values are @(start, length)@ of the
-- block in the original string.
type BlockMap a = MultiMap (String a) (Index, Length)

-- | /O(n^2 log n)/ Constructs the `BlockMap` of a string.
--
-- Blocks of length 1 are ignored.
--
-- >>> blockMap "abab"
-- fromList [(ab,(0,2) :| [(2,2)]),(aba,(0,3) :| []),(abab,(0,4) :| []),(ba,(1,2) :| []),(bab,(1,3) :| [])]
blockMap :: Ord a => String a -> BlockMap a
blockMap str =
    let n = length str
        indices = [(s, k) | s <- [0 .. n - 1], k <- [2 .. n - s]]
     in multiMap (fmap withSlice indices)
  where
    withSlice (s, k) = (unsafeSlice s k str, (s, k))

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

-- | The set of all possible common blocks in a pair of strings.
--
-- Keys are the subtrings associated with the block, and the values are the edges in the matching
-- graph.
type CommonBlockMap a = MultiMap (String a) Edge

-- | /O(n^3)/ Constructs the `CommonBlockMap` of a string.
--
-- Blocks of length 1 are ignored.
--
-- >>> commonBlockMap ("abab", "abba")
-- fromList [(ab,((0,0),2) :| [((2,0),2)]),(ba,((1,2),2) :| [])]
commonBlockMap :: Ord a => Pair (String a) -> CommonBlockMap a
commonBlockMap (s1, s2) = Map.intersectionWith (liftA2 createEdge) (blockMap s1) (blockMap s2)
  where
    createEdge (s, k) (p, _k) = Edge {start = (s, p), blockLen = k}

-- | /O(n^3 log n)/ List all edges of the matching graph for a pait of strings.
--
-- >>> edgeSet ("abab", "abba")
-- [((0,0),2),((2,0),2),((1,2),2)]
edgeSet :: Ord a => Pair (String a) -> Vector Edge
edgeSet = foldMap' asList . commonBlockMap
  where
    asList es = fromListN (length es) (toList es)
