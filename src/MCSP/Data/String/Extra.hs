-- | Custom operations for `String`.
module MCSP.Data.String.Extra (
    -- ** Partition operations
    Partition,
    PartitionPair,
    chars,

    -- ** Character set analysis
    frequency,
    singletons,
    hasOneOf,

    -- ** Substring analysis
    module MCSP.Data.String.Extra.Prefix,
    suffixes,
    longestCommonSubstring,
) where

import Data.Bool (Bool)
import Data.Foldable (Foldable (foldr'), any)
import Data.Function (id)
import Data.Int (Int)
import Data.Map.Strict (Map, alter, foldrWithKey')
import Data.Maybe (Maybe (Just), fromMaybe)
import Data.Monoid (mempty)
import Data.Ord (Ord)
import Data.Set (Set, insert, member)
import GHC.Num ((+))

import MCSP.Data.RadixTree.Suffix (construct, findMax, suffixes)
import MCSP.Data.String (String (..))
import MCSP.Data.String.Extra.Prefix

-- ------------------------ --
-- Operations on partitions --

-- | A collection of substrings of the same string.
type Partition a = [String a]

-- | A pair of partitions.
type PartitionPair a = (Partition a, Partition a)

-- | /O(n)/ Split the string in substrings of 1 char each.
--
-- >>> chars "abcd"
-- [a,b,c,d]
chars :: String a -> Partition a
chars = go []
  where
    go p (rest :>: ch) = go (ch : p) rest
    go !p Null = p

-- ---------------------- --
-- Character set analysis --

-- | /O(n lg n)/ Extracts the frequency count of each character in a string.
--
-- >>> frequency "aabacabd"
-- fromList [('a',4),('b',2),('c',1),('d',1)]
frequency :: Ord a => String a -> Map a Int
frequency = foldr' (alter increment) mempty
  where
    increment x = Just (1 + fromMaybe 0 x)

-- | /O(n lg n)/ Extracts the set of singleton characters in a string.
--
-- >>> singletons "aabacabd"
-- fromList "cd"
singletons :: Ord a => String a -> Set a
singletons str = foldrWithKey' insertSingleton mempty (frequency str)
  where
    insertSingleton k 1 = insert k
    insertSingleton _ _ = id

-- | /O(n lg m)/ Check if at least one of the character of string is present in the given set.
--
-- >>> import Data.Set (fromList)
-- >>> hasOneOf "abca" (fromList "bdf")
-- True
-- >>> import Data.Set (fromList)
-- >>> hasOneOf "xxx" (fromList "bdf")
-- False
hasOneOf :: Ord a => String a -> Set a -> Bool
hasOneOf str ls = any hasLetter str
  where
    hasLetter ch = member ch ls

-- ------------------ --
-- Substring analysis --

-- | /O(?)/ Extracts the longest string that is a substring of both strings.
--
-- Returns `Just` the lexicographically largest of the maximal subtrings, or `Data.Maybe.Nothing`
-- if strings are disjoint.
--
-- >>> longestCommonSubstring "ABABC" "ABCBA"
-- Just ABC
longestCommonSubstring :: Ord a => String a -> String a -> Maybe (String a)
longestCommonSubstring s1 s2 = findMax (construct s1 s2)
{-# INLINEABLE longestCommonSubstring #-}
