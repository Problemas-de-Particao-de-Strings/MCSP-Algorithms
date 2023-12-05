-- | Custom operations for `String`.
module MCSP.Data.String.Extra (
    -- ** Partition operations
    Partition,
    chars,

    -- ** Character set analysis
    alphabet,
    occurrences,
    singletons,
    repeated,
    hasOneOf,

    -- ** Substring analysis
    module MCSP.Data.String.Extra.Radix,
    longestCommonSubstring,
) where

import Control.Monad ((>>=))
import Data.Bool (Bool)
import Data.Foldable (any, foldl')
import Data.Function (flip, id, ($))
import Data.Int (Int)
import Data.Map.Strict (Map, alter, foldrWithKey')
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Data.Monoid (mempty)
import Data.Ord (Ord (..))
import Data.Set (Set, insert, member)
import GHC.Num ((+))

import MCSP.Data.RadixTree.Suffix (construct, findMax)
import MCSP.Data.String (String (..))
import MCSP.Data.String.Extra.Radix

-- ------------------------ --
-- Operations on partitions --

-- | A collection of substrings of the same string.
type Partition a = [String a]

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

-- | /O(n lg n)/ The set of all characters in a string.
--
-- >>> alphabet "aabacabd"
-- fromList "abcd"
alphabet :: Ord a => String a -> Set a
alphabet = foldl' (flip insert) mempty

-- | /O(n lg n)/ The frequency count of each character in a string.
--
-- >>> occurrences "aabacabd"
-- fromList [('a',4),('b',2),('c',1),('d',1)]
occurrences :: Ord a => String a -> Map a Int
occurrences = foldl' (flip $ alter increment) mempty
  where
    increment x = Just (1 + fromMaybe 0 x)
{-# INLINEABLE occurrences #-}

-- | /O(n lg n)/ The set of singleton characters in a string.
--
-- >>> singletons "aabacabd"
-- fromList "cd"
singletons :: Ord a => String a -> Set a
singletons str = foldrWithKey' insertSingleton mempty (occurrences str)
  where
    insertSingleton k 1 = insert k
    insertSingleton _ _ = id
{-# INLINEABLE singletons #-}

-- | /O(n lg n)/ The set of repeated characters in a string.
--
-- >>> repeated "aabacabd"
-- fromList "ab"
repeated :: Ord a => String a -> Set a
repeated str = foldrWithKey' insertRepeated mempty (occurrences str)
  where
    insertRepeated _ 1 = id
    insertRepeated k _ = insert k

-- | /O(n lg m)/ Check if at least one of the character of string is present in the given set.
--
-- >>> import Data.Set (fromList)
-- >>> hasOneOf "abca" (fromList "bdf")
-- True
--
-- >>> import Data.Set (fromList)
-- >>> hasOneOf "xxx" (fromList "bdf")
-- False
hasOneOf :: Ord a => String a -> Set a -> Bool
hasOneOf str ls = any (`member` ls) str

-- ------------------ --
-- Substring analysis --

-- | /O(?)/ Extracts the longest string that is a substring of both strings.
--
-- Returns `Just` the lexicographically largest of the maximal subtrings, or `Data.Maybe.Nothing`
-- if strings are disjoint.
--
-- >>> longestCommonSubstring "ABABC" "ABCBA"
-- Just ABC
--
-- >>> longestCommonSubstring "13" "1400"
-- Just 1
longestCommonSubstring :: Ord a => String a -> String a -> Maybe (String a)
longestCommonSubstring s1 s2 = findMax (construct s1 s2) >>= nonEmpty
  where
    nonEmpty (NonNull s) = Just s
    nonEmpty Null = Nothing
{-# INLINEABLE longestCommonSubstring #-}
