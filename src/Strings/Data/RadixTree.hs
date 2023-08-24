{-# LANGUAGE NoImplicitPrelude #-}

-- | A compressed trie of string radices.
module Strings.Data.RadixTree (
    -- * Data Types
    RadixTree,
    Map.RadixTreeMap,

    -- * Construction
    empty,

    -- * Query
    member,
) where

import Data.Bool (Bool)
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

-- | /O(n log r)/ Check if key is present in the tree.
member :: Ord a => String a -> RadixTree a -> Bool
member = Map.member
