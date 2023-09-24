module MCSP.Tests.RadixTree (radixTreeTests) where

import Data.Bool (Bool, not)
import Data.Char (Char)
import Data.Eq ((/=), (==))
import Data.Foldable (Foldable (..))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List.Extra (nubSort, snoc)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord (max, min)
import Data.Semigroup (Max (..), Min (..))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, Testable, classify, testProperty)

import MCSP.Data.RadixTree (RadixTree, construct, findMax, findMin, union)
import MCSP.Data.String (String, concat)

radixTreeTests :: TestTree
radixTreeTests = testGroup "RadixTree" [radixTreeFoldsAreSorted, treeStructureHolds]

radixTreeFoldsAreSorted :: TestTree
radixTreeFoldsAreSorted =
    testGroup
        "folds are sorted and deduplicated"
        [ testProperty "toList . construct == nubSort" $
            markDeduplicated $
                \list -> toList (construct list) == nubSort list,
          testProperty "foldMap (: []) . construct == nubSort" $
            markDeduplicated $
                \list -> foldMap (: []) (construct list) == nubSort list,
          testProperty "foldMap' (: []) . construct == nubSort" $
            markDeduplicated $
                \list -> foldMap' (: []) (construct list) == nubSort list,
          testProperty "foldr (:) [] . construct == nubSort" $
            markDeduplicated $
                \list -> foldr (:) [] (construct list) == nubSort list,
          testProperty "foldr' (:) [] . construct == nubSort" $
            markDeduplicated $
                \list -> foldr' (:) [] (construct list) == nubSort list,
          testProperty "foldl snoc [] . construct == nubSort" $
            markDeduplicated $
                \list -> foldl snoc [] (construct list) == nubSort list,
          testProperty "foldl' snoc [] . construct == nubSort" $
            markDeduplicated $
                \list -> foldl' snoc [] (construct list) == nubSort list,
          testProperty "fold . construct == concat . nubSort" $
            markDeduplicated $
                \list -> fold (construct list) == concat (nubSort list)
        ]

markDeduplicated :: ([String Char] -> Bool) -> [String Char] -> Property
markDeduplicated prop xs =
    classify (length xs /= length (nubSort xs)) "deduplicated" (prop xs)

treeStructureHolds :: TestTree
treeStructureHolds =
    testGroup
        "tree structure holds"
        [ testProperty "findMin == getMin <$> foldMap (Just . Min)" $
            markNonEmpty $
                \tree -> findMin tree == (getMin <$> foldMap' (Just . Min) tree),
          testProperty "findMax == getMax <$> foldMap (Just . Max)" $
            markNonEmpty $
                \tree -> findMax tree == (getMax <$> foldMap' (Just . Max) tree),
          testProperty "findMin (x `union` y) == findMin x `min` findMin y" $
            markNonEmpty $
                \x -> markNonEmpty $
                    \y -> findMin (x `union` y) == findMin x `minJust` findMin y,
          testProperty "findMax (x `union` y) == findMax x `max` findMax y" $
            markNonEmpty $
                \x -> markNonEmpty $
                    \y -> findMax (x `union` y) == findMax x `max` findMax y
        ]
  where
    minJust (Just x) (Just y) = Just (x `min` y)
    minJust mx Nothing = mx
    minJust Nothing my = my

markNonEmpty :: Testable prop => (RadixTree Char -> prop) -> RadixTree Char -> Property
markNonEmpty prop tree =
    classify (not (null tree)) "non-empty" (prop tree)
