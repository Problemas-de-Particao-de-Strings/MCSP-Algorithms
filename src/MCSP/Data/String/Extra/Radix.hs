-- | Radix operations on `String`.
module MCSP.Data.String.Extra.Radix (
    -- * Prefix
    stripPrefix,
    isPrefixOf,
    commonPrefix,
    splitCommonPrefix,

    -- * Infix
    stripInfix,
    isInfixOf,

    -- * Suffix
    stripSuffix,
    isSuffixOf,
    suffixes,
) where

import Data.Bool (Bool (..), otherwise)
import Data.Eq (Eq, (/=), (==))
import Data.Function ((.))
import Data.Int (Int)
import Data.List.Extra (find, firstJust)
import Data.Maybe (Maybe (Just, Nothing), fromMaybe, isJust)
import Data.Ord (min)
import Data.Tuple.Extra (fst3)
import GHC.Num ((+), (-))

import MCSP.Data.String (String (..), drop, length, splitAt, take, unsafeIndex, unsafeSlice)

-- ---------------------- --
-- String prefix analysis --

-- | /O(min(m,n))/ Removes the given prefix from a string.
--
-- Returns `Nothing` if the string does not start with the prefix given, or `Just` the string after
-- the prefix.
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
p `isPrefixOf` str = take (length p) str == p

-- | /O(min(m,n))/ The length of maximum common prefix of two string.
--
-- >>> commonPrefixLength "abc" "abd"
-- 2
commonPrefixLength :: Eq a => String a -> String a -> Int
commonPrefixLength lhs rhs = fromMaybe n (find notMatching indices)
  where
    n = min (length lhs) (length rhs)
    indices = [0 .. n - 1] :: [Int]
    -- SAFETY: index i is in range [0, n = min(lhs, rhs)) and is guaranteed to be in bounds for
    -- both strings
    notMatching i = unsafeIndex lhs i /= unsafeIndex rhs i
{-# INLINEABLE commonPrefixLength #-}

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
{-# INLINE commonPrefix #-}

-- | /O(min(m,n))/ Returns the maximum common prefix of two strings and the remainder of each string.
--
-- Note that @splitCommonPrefix a b@ is equivalent to
-- @let p = commonPrefix a b in (p, fromJust (splitPrefix p a), fromJust (splitPrefix p b))@, but
-- slightly more efficient and cannot fail.
--
-- >>> splitCommonPrefix "abc" "abd"
-- (ab,c,d)
-- >>> splitCommonPrefix "xyz" "xyz"
-- (xyz,,)
-- >>> splitCommonPrefix "def" "ghi"
-- (,def,ghi)
splitCommonPrefix :: Eq a => String a -> String a -> (String a, String a, String a)
splitCommonPrefix lhs rhs =
    -- SAFETY: commonPrefixLength is guaranteed to resolve to a length not bigger than
    -- any of the strings
    ( unsafeSlice 0 prefix lhs,
      -- SAFETY: again, '0 <= prefix <= length lhs' and '0 <= prefix <= length rhs'
      unsafeSlice prefix (length lhs - prefix) lhs,
      unsafeSlice prefix (length rhs - prefix) rhs
    )
  where
    prefix = commonPrefixLength lhs rhs
{-# INLINE splitCommonPrefix #-}

-- --------------------- --
-- String infix analysis --

-- | /O(n m)/ Return the the string before (prefix) and after (suffix) the search string, or
-- `Nothing` if the search string is not present.
--
-- A result of @`Just` (s1, s2) = `stripInfix` s r@ means that
-- @s1 `Strings.Data.String.++` r `Strings.Data.String.++` s2 `==` s@.
--
-- >>> stripInfix "el" "Hello"
-- Just (H,lo)
-- >>> stripInfix "World!" "Hello"
-- Nothing
-- >>> stripInfix "si" "mississipi"
-- Just (mis,ssipi)
-- >>> stripInfix "chat" "hat"
-- Nothing
-- >>> stripInfix "word" "word"
-- Just (,)
stripInfix :: Eq a => String a -> String a -> Maybe (String a, String a)
stripInfix needle haystack = firstJust (matchingNeedle . split) [0 .. n]
  where
    r = length needle
    n = length haystack - r
    matchingNeedle (a, candidate, c)
        | candidate == needle = Just (a, c)
        | otherwise = Nothing
    -- SAFETY: given '0 <= i <= n` and `n = length str - length radix`, we'll have that always
    -- `0 <= i + r <= length str` and `n - i = length str - r - i`, both being inbounds
    split i =
        ( unsafeSlice 0 i haystack, -- part before infix (prefix)
          unsafeSlice i r haystack, -- candidate for infix
          unsafeSlice (i + r) (n - i) haystack -- after matched infix (suffix)
        )

-- | /O(n m)/  Check if the first string is contained anywhere within the second string.
--
-- >>> isInfixOf "Haskell" "I really like Haskell."
-- True
-- >>> isInfixOf "Ial" "I really like Haskell."
-- False
isInfixOf :: Eq a => String a -> String a -> Bool
r `isInfixOf` str = isJust (stripInfix r str)
{-# INLINEABLE isInfixOf #-}

-- ---------------------- --
-- String suffix analysis --

-- | /O(min(m,n))/ Returns the prefix of the second string if its suffix matches the first string.
--
-- Returns `Nothing` if the string does not end with the suffix given, or `Just` the string before
-- the suffix.
--
-- >>> stripSuffix "bar" "foobar"
-- Just foo
-- >>> stripSuffix "" "baz"
-- Just baz
-- >>> stripSuffix "foo" "quux"
-- Nothing
stripSuffix :: Eq a => String a -> String a -> Maybe (String a)
stripSuffix s str
    | s1 == s = Just s0
    | otherwise = Nothing
  where
    (s0, s1) = splitAt (length str - length s) str
{-# INLINEABLE stripSuffix #-}

-- | /O(min(m,n))/ Returns `True` iff the first string is a prefix of the second.
--
-- >>> "ld!" `isSuffixOf` "Hello World!"
-- True
-- >>> "World" `isSuffixOf` "Hello World!"
-- False
isSuffixOf :: Eq a => String a -> String a -> Bool
s `isSuffixOf` str = drop (length str - length s) str == s

-- | /O(n)/ Extract all non-empty suffixes of a string.
--
-- >>> suffixes "Hello"
-- [Hello,ello,llo,lo,o]
suffixes :: String a -> [String a]
suffixes s@(_ :< rest) = s : suffixes rest
suffixes Null = []
{-# INLINE suffixes #-}
