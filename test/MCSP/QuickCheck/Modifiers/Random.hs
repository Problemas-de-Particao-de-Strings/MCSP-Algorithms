module MCSP.QuickCheck.Modifiers.Random (
    Randomized (Randomized, getRandom),
    (=~=),
) where

import Data.Eq (Eq)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List (map)
import GHC.Enum (maxBound)
import Text.Show (Show)

import Test.Tasty.QuickCheck (
    Arbitrary (..),
    CoArbitrary (..),
    Fun,
    Property,
    chooseBoundedIntegral,
    property,
    (===),
    pattern Fn,
 )

import MCSP.System.Random (Random, Seed, generateWith, uniform)

infix 4 =~=

-- | Like `===`, but compares two randomly generated values.
(=~=) :: (Eq a, Show a) => Random a -> Random a -> Property
genA =~= genB = property $ \seed ->
    generateWith seed genA === generateWith seed genB

-- | A QuickCheck Modifier that generates a `Random` value. See `getRandom`.
newtype Randomized a = Generator (Fun Seed a)
    deriving newtype (Show)

extractMonad :: Randomized a -> Random a
extractMonad (Generator (Fn generate)) = generate <$> uniform

{-# COMPLETE Randomized #-}

-- | A QuickCheck Modifier that generates a `Random` value.
pattern Randomized :: Random a -> Randomized a
pattern Randomized {getRandom} <- (extractMonad -> getRandom)

instance Arbitrary a => Arbitrary (Randomized a) where
    arbitrary = Generator <$> arbitrary
    shrink (Generator fn) = map Generator $ shrink fn

instance CoArbitrary a => CoArbitrary (Randomized a) where
    coarbitrary (Generator (Fn f)) gen = do
        s1 <- chooseBoundedIntegral (0, maxBound)
        s2 <- chooseBoundedIntegral (0, maxBound)
        coarbitrary (f (s1, s2)) gen
