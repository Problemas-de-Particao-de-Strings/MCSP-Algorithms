module MCSP.QuickCheck.Modifiers.String (
    BalancedStrings (BalancedStrings, getBalancedStrings),
) where

import Data.Bits (countLeadingZeros, finiteBitSize, shift)
import Data.Bool ((&&))
import Data.Eq (Eq (..))
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Ord (Ord (..))
import GHC.Num ((*), (+), (-))
import GHC.Real (div)
import Test.QuickCheck.Arbitrary (Arbitrary (..), CoArbitrary (..))
import Test.QuickCheck.Function (Function (..), functionMap)
import Test.QuickCheck.Gen (scale)
import Text.Show (Show)

import MCSP.Data.Pair (Pair, both)
import MCSP.Data.String (String (..), Unbox, concat)
import MCSP.QuickCheck.Modifiers.Pair (ShuffledPair (getPair))

-- | A QuickCheck Modifier that generates a pair of balanced strings from common partitions.
-- See `getBalancedStrings`.
newtype BalancedStrings a = CommonPartitions {partitions :: ShuffledPair (String a)}
    deriving newtype (Eq, Ord, Show)

-- | Extracts the permuted pair from a `BalancedStrings`.
extractStrings :: Unbox a => BalancedStrings a -> Pair (String a)
extractStrings CommonPartitions {..} = concat `both` getPair partitions

{-# COMPLETE BalancedStrings #-}

-- | A QuickCheck Modifier that generates a pair of balanced `String` from common partitions.
pattern BalancedStrings :: Unbox a => Pair (String a) -> BalancedStrings a
pattern BalancedStrings {getBalancedStrings} <- (extractStrings -> getBalancedStrings)

-- | Integer square root.
--
-- Returns a number @r@ such that @r^2 <= n < (r+1)^2@.
--
-- From <https://wiki.haskell.org/Generic_number_type#squareRoot>.
--
-- >>> squareRoot 10
-- 3
-- >>> squareRoot 15
-- 3
-- >>> squareRoot 16
-- 4
squareRoot :: Int -> Int
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
