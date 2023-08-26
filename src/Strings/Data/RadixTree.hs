{-# LANGUAGE NoImplicitPrelude #-}

-- | A compressed trie of string radices.
module Strings.Data.RadixTree (
    -- * Data Types
    RadixTree,
    Map.RadixTreeMap,

    -- * Construction
    empty,
    construct,

    -- * Query
    member,
    findMin,
    findMax,

    -- * Modification
    insert,
    union,
    delete,
) where

import Control.Arrow ((&&&))
import Data.Bool (Bool)
import Data.Function (id, (.))
import Data.List (map)
import Data.Maybe (Maybe)
import Data.Ord (Ord)

import Strings.Data.RadixTree.Map qualified as Map
import Strings.Data.String (String (..))

-- | A set of `String`s represented by a [radix tree](https://en.wikipedia.org/wiki/Radix_tree).
--
-- This tree is implemented a `Map.RadixTreeMap` where each key is associated with itself, enabling key construction in
-- constant time (no need to concatenate radices) and some useful instances (like `Data.Foldable`).
type RadixTree a = Map.RadixTreeMap a (String a)

-- | /O(1)/ The empty tree.
empty :: RadixTree a
empty = Map.empty

-- | /O(?)/ Build a radix tree from a list of strings.
construct :: Ord a => [String a] -> RadixTree a
construct = Map.construct . map (id &&& id)

-- | /O(n log r)/ Check if string is present in the tree.
member :: Ord a => String a -> RadixTree a -> Bool
member = Map.member

-- | /O(n log r)/ Extract the minimal string in the tree.
findMin :: RadixTree a -> Maybe (String a)
findMin = Map.lookupMin

-- | /O(n log r)/ Extract the minimal string in the tree.
findMax :: RadixTree a -> Maybe (String a)
findMax = Map.lookupMax

-- | /O(?)/ Insert a string in a tree.
--
-- If the tree already contains a string equal to the given value, it is replaced with the new value.
insert :: Ord a => String a -> RadixTree a -> RadixTree a
insert s = Map.insert s s

-- | /O(?)/ The union of two sets, preferring the first set when equal strings are encountered.
union :: Ord a => RadixTree a -> RadixTree a -> RadixTree a
union = Map.union

-- | /O(?)/  Delete a string from a set.
delete :: Ord a => String a -> RadixTree a -> RadixTree a
delete = Map.delete
