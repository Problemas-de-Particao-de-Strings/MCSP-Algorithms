{-# LANGUAGE NoImplicitPrelude #-}

-- | Prefix operations on `String`.
module Strings.Data.String.Prefix (
    stripPrefix,
    isPrefixOf,
    commonPrefix,
    splitCommonPrefix,
) where

import Data.Bool (Bool (..), otherwise)
import Data.Eq (Eq, (/=), (==))
import Data.Int (Int)
import Data.List (find)
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Data.Ord (min)
import Data.Tuple.Extra (fst3)
import GHC.Num ((-))

import Strings.Data.String (String, length, splitAt, take, unsafeIndex, unsafeSlice)

-- ---------------------- --
-- String prefix analysis --

-- | /O(min(m,n))/ Removes the given prefix from a string.
--
-- Returns `Nothing` if the string does not start with the prefix given, or `Just` the string after the prefix.
--
-- >>> stripPrefix "foo" "foobar"
-- Just bar
-- >>> stripPrefix "foo" "foo"
-- Just
-- >>> stripPrefix "foo" "barfoo"
-- Nothing
stripPrefix :: Eq a => String a -> String a -> Maybe (String a)
stripPrefix p str
    | s0 == p = Just s1
    | otherwise = Nothing
  where
    (s0, s1) = splitAt (length p) str
{-# INLINEABLE stripPrefix #-}

-- | /O(min(m,n))/ Returns `True` iff the first string is a prefix of the second.
--
-- >>> "Hello" `isPrefixOf` "Hello World!"
-- True
-- >>> "Hello" `isPrefixOf` "Wello Horld!"
-- False
isPrefixOf :: Eq a => String a -> String a -> Bool
isPrefixOf p str = take (length p) str == p
{-# INLINEABLE isPrefixOf #-}

-- | /O(min(m,n))/ The length of maximum common prefix of two string.
--
-- >>> commonPrefixLength "abc" "abd"
-- 2
commonPrefixLength :: Eq a => String a -> String a -> Int
commonPrefixLength lhs rhs = fromMaybe n (find notMatching indices)
  where
    n = min (length lhs) (length rhs)
    indices = [0 .. n - 1] :: [Int]
    -- SAFETY: index i is in range [0, n = min(lhs, rhs)) and is guaranteed to be in bounds for both strings
    notMatching i = unsafeIndex lhs i /= unsafeIndex rhs i

-- | /O(min(m,n))/ The maximum common prefix of two strings.
--
-- >>> commonPrefix "abc" "abd"
-- ab
-- >>> commonPrefix "xyz" "xyz"
-- xyz
-- >>> commonPrefix "def" "ghi"
-- <BLANKLINE>
commonPrefix :: Eq a => String a -> String a -> String a
commonPrefix lhs rhs = fst3 (splitCommonPrefix lhs rhs)

-- | /O(min(m,n))/ Returns the maximum common prefix of two strings and the remainder of each string.
--
-- Note that `splitCommonPrefix a b` is equivalent to `let p = commonPrefix a b in (p, fromJust (splitPrefix p a),
-- fromJust (splitPrefix p b))`, but slightly more efficient.
--
-- >>> splitCommonPrefix "abc" "abd"
-- (ab,c,d)
-- >>> splitCommonPrefix "xyz" "xyz"
-- (xyz,,)
-- >>> splitCommonPrefix "def" "ghi"
-- (,def,ghi)
splitCommonPrefix :: Eq a => String a -> String a -> (String a, String a, String a)
splitCommonPrefix lhs rhs =
    -- SAFETY: commonPrefixLength is guaranteed to resolve to a length not bigger than any of the strings
    ( unsafeSlice 0 prefix lhs
    , -- SAFETY: again, '0 <= prefix <= length lhs' and '0 <= prefix <= length rhs'
      unsafeSlice prefix (length lhs - prefix) lhs
    , unsafeSlice prefix (length rhs - prefix) rhs
    )
  where
    prefix = commonPrefixLength lhs rhs
