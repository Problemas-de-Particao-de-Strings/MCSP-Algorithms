{-# LANGUAGE NoImplicitPrelude #-}

module Strings.Data.String.Analysis (
    frequency,
    singletons,
    hasOneOf,
) where

import Data.Bool (Bool)
import Data.Foldable (Foldable (foldr'), any)
import Data.Function (id, ($), (.))
import Data.Int (Int)
import Data.Map.Strict (Map, alter, foldrWithKey')
import Data.Maybe (Maybe (Just), maybe)
import Data.Monoid (mempty)
import Data.Ord (Ord)
import Data.Set (Set, insert, member)
import GHC.Num ((+))

import Strings.Data.String (String)

-- ----------------------------- --
-- String information extraction --
-- ----------------------------- --

-- | /O(n lg n)/ Extracts the frequency count of each character in a string.
--
-- >>> frequency "aabacabd"
-- fromList [('a',4),('b',2),('c',1),('d',1)]
frequency :: Ord a => String a -> Map a Int
frequency = foldr' (alter $ Just . maybe 1 (+ 1)) mempty

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
-- >>> hasOneOf "abca" (Data.Set.fromList "bdf")
-- True
-- >>> import Data.Set (fromList)
-- >>> hasOneOf "xxx" (Data.Set.fromList "bdf")
-- False
hasOneOf :: Ord a => String a -> Set a -> Bool
hasOneOf str ls = any hasLetter str
  where
    hasLetter ch = member ch ls
