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
--
-- >>> import Prelude (Char)
-- >>> empty :: RadixTree Char
-- Tree []
empty :: RadixTree a
empty = Map.empty

-- | /O(?)/ Build a radix tree from a list of strings.
--
-- >>> construct ["abc", "def", "abb"]
-- Tree [ab :~> Tree [b :~> Tree (abb) [],c :~> Tree (abc) []],def :~> Tree (def) []]
construct :: Ord a => [String a] -> RadixTree a
construct = Map.construct . map (id &&& id)

-- | /O(n log r)/ Check if string is present in the tree.
--
-- >>> member "ab" (construct ["abc", "def", "abb"])
-- False
-- >>> member "abc" (construct ["abc", "def", "abb"])
-- True
member :: Ord a => String a -> RadixTree a -> Bool
member = Map.member

-- | /O(n log r)/ Extract the minimal string in the tree.
--
-- >>> findMin (construct ["abc", "def", "abb"])
-- Just abb
-- >>> import Prelude (Char)
-- >>> findMin (empty :: RadixTree Char)
-- Nothing
findMin :: RadixTree a -> Maybe (String a)
findMin = Map.lookupMin

-- | /O(n log r)/ Extract the minimal string in the tree.
--
-- >>> findMax (construct ["abc", "def", "abb"])
-- Just def
-- >>> import Prelude (Char)
-- >>> findMax (empty :: RadixTree Char)
-- Nothing
findMax :: RadixTree a -> Maybe (String a)
findMax = Map.lookupMax

-- | /O(?)/ Insert a string in a tree.
--
-- If the tree already contains a string equal to the given value, it is replaced with the new value.
--
-- >>> insert "xyz" (construct ["abc"])
-- Tree [abc :~> Tree (abc) [],xyz :~> Tree (xyz) []]
-- >>> insert "xyz" (construct ["abc", "xyz"])
-- Tree [abc :~> Tree (abc) [],xyz :~> Tree (xyz) []]
insert :: Ord a => String a -> RadixTree a -> RadixTree a
insert s = Map.insert s s

-- | /O(?)/ The union of two sets, preferring the first set when equal strings are encountered.
--
-- >>> construct ["abc"] `union` construct ["def"]
-- Tree [abc :~> Tree (abc) [],def :~> Tree (def) []]
-- >>> construct ["abc"] `union` construct []
-- Tree [abc :~> Tree (abc) []]
-- >>> construct [] `union` construct ["abc"]
-- Tree [abc :~> Tree (abc) []]
-- >>> construct ["abc"] `union` construct ["abc"]
-- Tree [abc :~> Tree (abc) []]
union :: Ord a => RadixTree a -> RadixTree a -> RadixTree a
union = Map.union

-- | /O(?)/  Delete a string from the tree set.
--
-- >>> delete "abc" (construct ["abc", "def"])
-- Tree [def :~> Tree (def) []]
-- >>> delete "abc" (construct ["def"])
-- Tree [def :~> Tree (def) []]
delete :: Ord a => String a -> RadixTree a -> RadixTree a
delete = Map.delete
