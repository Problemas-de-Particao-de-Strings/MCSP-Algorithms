module MCSP.QuickCheck.Modifiers.RadixTree (
    ArbitraryTree (..),
) where

import Control.Applicative (pure)
import Data.Eq (Eq (..))
import Data.Foldable (foldr, length, toList)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List (concat, map, (++))
import Data.List.Extra (nubSort)
import Data.Maybe (Maybe (..))
import Data.Ord (Ord (..))
import GHC.Num ((-))
import Test.QuickCheck.Arbitrary (Arbitrary (..), CoArbitrary (..))
import Test.QuickCheck.Function (Function (..), functionMap)
import Test.QuickCheck.Gen (Gen, chooseInt, sized, vectorOf)
import Text.Show (Show)

import MCSP.Data.RadixTree (RadixTree, construct, delete, findMax, insert)
import MCSP.Data.String (String (..), Unbox)

-- | A QuickCheck Modifier that generates `RadixTree` instances.
newtype ArbitraryTree a = ArbitraryTree {getTree :: RadixTree a}
    deriving newtype (Eq, Ord, Show)

arbitraryTree :: Ord a => Gen (String a) -> Gen (RadixTree a)
arbitraryTree gen = sized $ \maxN -> do
    n <- chooseInt (0, maxN)
    strs <- unique n []
    pure $ construct strs
  where
    unique n xs =
        if n >= length xs
            then pure xs
            else do
                x <- vectorOf (n - length xs) gen
                unique n (nubSort (xs ++ x))

shrinkTree :: Ord a => (String a -> [String a]) -> RadixTree a -> [RadixTree a]
shrinkTree shrinkKey tree = case findMax tree of
    Just maxKey ->
        let rest = delete maxKey tree
         in rest : concat [shr maxKey rest | shr <- [shrinkRest, shrinkMax]]
    Nothing -> []
  where
    shrinkRest key subtree = map (insert key) (shrinkTree shrinkKey subtree)
    shrinkMax key subtree = map (`insert` subtree) (shrinkKey key)

instance (Unbox a, Ord a, Arbitrary a) => Arbitrary (ArbitraryTree a) where
    arbitrary = ArbitraryTree <$> arbitraryTree arbitrary
    shrink (ArbitraryTree tree) = ArbitraryTree <$> shrinkTree shrink tree

instance CoArbitrary a => CoArbitrary (ArbitraryTree a) where
    coarbitrary (ArbitraryTree tree) gen = foldr coarbitrary gen tree

instance (Unbox a, Ord a, Function a) => Function (ArbitraryTree a) where
    function = functionMap (toList . getTree) (ArbitraryTree . construct)
