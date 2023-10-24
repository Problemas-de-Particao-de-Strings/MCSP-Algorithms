module MCSP.QuickCheck.Modifiers.Pair (
    ShuffledPair (ShuffledPair, getPair),
) where

import Control.Applicative (pure)
import Data.Eq (Eq (..))
import Data.Foldable (length)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.List (map, sortOn, zip, zipWith)
import Data.Ord (Ord (..))
import Test.QuickCheck.Arbitrary (Arbitrary (..), Arbitrary1 (..), CoArbitrary (..))
import Test.QuickCheck.Function (Function (..), functionMap)
import Test.QuickCheck.Gen (shuffle)
import Text.Show (Show)

import MCSP.Data.Pair (Pair, both, fst, snd)

-- | A QuickCheck Modifier that generates a pair of lists such that one is a permutation of the
-- other. See `getPair`.
newtype ShuffledPair a = IndexedList {indexed :: [(Int, a)]}
    deriving newtype (Eq, Ord, Show)

-- | Extracts the permuted pair from a `ShuffledPair`.
extractPair :: ShuffledPair a -> Pair [a]
extractPair IndexedList {..} = map snd `both` (indexed, sortOn fst indexed)

{-# COMPLETE ShuffledPair #-}

-- | A QuickCheck Modifier that generates a pair of lists such that one is a permutation of the other.
pattern ShuffledPair :: Pair [a] -> ShuffledPair a
pattern ShuffledPair {getPair} <- (extractPair -> getPair)

instance Arbitrary1 ShuffledPair where
    liftArbitrary gen = do
        elems <- liftArbitrary gen
        idx <- shuffle [1 .. length elems]
        pure (IndexedList (zip idx elems))
    liftShrink shr IndexedList {..} =
        map IndexedList (liftShrink shrinkWithIndex indexed)
      where
        shrinkWithIndex (i, x) = map (i,) (shr x)

instance Arbitrary a => Arbitrary (ShuffledPair a) where
    arbitrary = liftArbitrary arbitrary
    shrink = liftShrink shrink

instance CoArbitrary a => CoArbitrary (ShuffledPair a) where
    coarbitrary = coarbitrary . indexed

instance Function a => Function (ShuffledPair a) where
    function = functionMap indexed (IndexedList . fixIndices)
      where
        replaceIdx i (k, (_, x)) = (k, (i, x))
        fixIndices elems =
            let n = [1 .. length elems]
             in map snd $ sortOn fst $ zipWith replaceIdx n $ sortOn (fst . snd) $ zip n elems
