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

    -- ** QuickCheck instances
    BalancedStrings (BalancedStrings, getBalancedStrings),
) where

import Control.Monad ((>>=))
import Data.Bits (FiniteBits, countLeadingZeros, finiteBitSize, shift)
import Data.Bool (Bool, (&&))
import Data.Eq (Eq)
import Data.Foldable (any, foldl')
import Data.Function (flip, id, ($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Map.Strict (Map, alter, foldrWithKey')
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Data.Monoid (mempty)
import Data.Ord (Ord (..))
import Data.Set (Set, insert, member)
import GHC.Num ((*), (+), (-))
import GHC.Real (Integral, div)
import Test.QuickCheck.Arbitrary (Arbitrary (..), CoArbitrary (..))
import Test.QuickCheck.Function (Function (..), functionMap)
import Test.QuickCheck.Gen (scale)
import Text.Show (Show)

import MCSP.Data.Pair (Pair, ShuffledPair, both, getPair)
import MCSP.Data.RadixTree.Suffix (construct, findMax)
import MCSP.Data.String (String (..), Unbox, concat)
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
-- >>> longestCommonSubstring "13" "1400"
-- Just 1
longestCommonSubstring :: Ord a => String a -> String a -> Maybe (String a)
longestCommonSubstring s1 s2 = findMax (construct s1 s2) >>= nonEmpty
  where
    nonEmpty (NonNull s) = Just s
    nonEmpty Null = Nothing
{-# INLINEABLE longestCommonSubstring #-}

-- -------------------- --
-- QuickCheck instances --

-- | A [QuickCheck Modifier](https://hackage.haskell.org/package/QuickCheck/docs/Test-QuickCheck-Modifiers.html)
-- that generates a pair of balanced strings from common partitions. See `getBalancedStrings`.
newtype BalancedStrings a = CommonPartitions {partitions :: ShuffledPair (String a)}
    deriving newtype (Eq, Ord, Show)

-- | Extracts the permuted pair from a `BalancedStrings`.
extractStrings :: Unbox a => BalancedStrings a -> Pair (String a)
extractStrings CommonPartitions {..} = concat `both` getPair partitions

{-# COMPLETE BalancedStrings #-}

-- | A [QuickCheck Modifier](https://hackage.haskell.org/package/QuickCheck/docs/Test-QuickCheck-Modifiers.html)
-- that generates a pair of balanced strings from common partitions.
pattern BalancedStrings :: Unbox a => Pair (String a) -> BalancedStrings a
pattern BalancedStrings {getBalancedStrings} <- (extractStrings -> getBalancedStrings)

-- | Integer square root.
--
-- Returns a number @r@ such that @r^2 <= n < (r+1)^2@.
--
-- From <https://wiki.haskell.org/Generic_number_type#squareRoot>.
--
-- >>> squareRoot 10 :: Int
-- 3
-- >>> squareRoot 15 :: Int
-- 3
-- >>> squareRoot 16 :: Int
-- 4
squareRoot :: (Integral a, FiniteBits a) => a -> a
squareRoot n | n <= 0 = 0
squareRoot 1 = 1
squareRoot n = go (squareRoot (n `div` nextPow2) * lowerRoot)
  where
    lastBitSet = finiteBitSize n - 1 - countLeadingZeros n
    nextPow2 = shift 1 lastBitSet
    lowerRoot = shift nextPow2 (-1)

    go r =
        if r * r <= n && n < (r + 1) * (r + 1)
            then r
            else go ((r + n `div` r) `div` 2)

instance (Unbox a, Arbitrary a) => Arbitrary (BalancedStrings a) where
    -- we need to resize the generator, because balanced string are generated in the with size n^2
    arbitrary = CommonPartitions <$> scale squareRoot arbitrary
    shrink (CommonPartitions partitions) = CommonPartitions <$> shrink partitions

instance CoArbitrary a => CoArbitrary (BalancedStrings a) where
    coarbitrary = coarbitrary . partitions

instance (Unbox a, Function a) => Function (BalancedStrings a) where
    function = functionMap partitions CommonPartitions
