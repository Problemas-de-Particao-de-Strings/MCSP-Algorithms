module MCSP.QuickCheck.Modifiers.IsList (
    ViaList (..),
    arbitraryList,
    shrinkList,
    coarbitraryList,
    functionMapList,
) where

import Data.Eq (Eq (..))
import Data.Function (id, ($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (foldl, map)
import Data.Ord (Ord (..))
import GHC.Exts (IsList (..))
import GHC.Num ((+))
import Test.QuickCheck.Arbitrary (Arbitrary (..), Arbitrary1 (..), CoArbitrary (..))
import Test.QuickCheck.Function (Function (..), functionMap, (:->))
import Test.QuickCheck.Gen (Gen, variant)
import Text.Show (Show)

-- | A QuickCheck Modifier that generates `IsList` instances by converting to lists.
newtype ViaList l = ViaList {getViaList :: l}
    deriving newtype (Eq, Ord, Show)

arbitraryList :: (IsList l, Arbitrary (Item l)) => Gen l
arbitraryList = fromList <$> liftArbitrary arbitrary

shrinkList :: (IsList l, Arbitrary (Item l)) => l -> [l]
shrinkList (toList -> xs) = map fromList $ liftShrink shrink xs

coarbitraryList :: (IsList l, CoArbitrary (Item l)) => l -> Gen b -> Gen b
coarbitraryList (toList -> xs) =
    let (len, elems) = foldl perturb (0, id) xs
     in variant @Int len . elems
  where
    perturb (idx, elems) val = (idx + 1, elems . variant idx . coarbitrary val)

functionMapList :: (IsList l, Function (Item l)) => (a -> l) -> (l -> a) -> (a -> c) -> a :-> c
functionMapList g h = functionMap (toList . g) (h . fromList)

instance (IsList l, Arbitrary (Item l)) => Arbitrary (ViaList l) where
    arbitrary = ViaList <$> arbitraryList
    shrink = map ViaList . shrinkList . getViaList

instance (IsList l, CoArbitrary (Item l)) => CoArbitrary (ViaList l) where
    coarbitrary = coarbitraryList . getViaList

instance (IsList l, Function (Item l)) => Function (ViaList l) where
    function = functionMapList getViaList ViaList
