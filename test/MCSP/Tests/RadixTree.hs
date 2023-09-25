module MCSP.Tests.RadixTree (radixTreeTests) where

import Data.Bool (not)
import Data.Char (Char)
import Data.Eq ((/=))
import Data.Foldable (Foldable (..))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List.Extra (nubSort, snoc)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord (max, min)
import Data.Semigroup (Max (..), Min (..))

import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, Testable, classify, testProperty, (===), (==>))

import MCSP.Data.RadixTree (
    RadixTree,
    SameKeyVal (..),
    construct,
    delete,
    findMax,
    findMin,
    insert,
    member,
    union,
 )
import MCSP.Data.String (String, concat)

radixTreeTests :: TestTree
radixTreeTests = testGroup "RadixTree" [radixTreeFoldsAreSorted, treeStructureHolds]

radixTreeFoldsAreSorted :: TestTree
radixTreeFoldsAreSorted =
    testGroup
        "folds are sorted and deduplicated"
        [ testFromList "toList . construct == nubSort" $ \list ->
            toList (construct list) === nubSort list,
          testFromList "foldMap (: []) . construct == nubSort" $ \list ->
            foldMap (: []) (construct list) === nubSort list,
          testFromList "foldMap' (: []) . construct == nubSort" $ \list ->
            foldMap' (: []) (construct list) === nubSort list,
          testFromList "foldr (:) [] . construct == nubSort" $ \list ->
            foldr (:) [] (construct list) === nubSort list,
          testFromList "foldr' (:) [] . construct == nubSort" $ \list ->
            foldr' (:) [] (construct list) === nubSort list,
          testFromList "foldl snoc [] . construct == nubSort" $ \list ->
            foldl snoc [] (construct list) === nubSort list,
          testFromList "foldl' snoc [] . construct == nubSort" $ \list ->
            foldl' snoc [] (construct list) === nubSort list,
          testFromList "fold . construct == concat . nubSort" $ \list ->
            fold (construct list) === concat (nubSort list)
        ]
  where
    testFromList :: Testable prop => TestName -> ([String Char] -> prop) -> TestTree
    testFromList name prop = testProperty name $ \xs ->
        classify (length xs /= length (nubSort xs)) "deduplicated" (prop xs)

treeStructureHolds :: TestTree
treeStructureHolds =
    testGroup
        "tree structure holds"
        [ testProperty "findMin == getMin <$> foldMap (Just . Min)" $
            markNonEmpty $ \tree ->
                findMin tree === (getMin <$> foldMap' (Just . Min) tree),
          testProperty "findMax == getMax <$> foldMap (Just . Max)" $
            markNonEmpty $ \tree ->
                findMax tree === (getMax <$> foldMap' (Just . Max) tree),
          testProperty "findMin (x `union` y) == findMin x `min` findMin y" $
            markNonEmpty $ \x -> markNonEmpty $ \y ->
                findMin (x `union` y) === findMin x `minJust` findMin y,
          testProperty "findMax (x `union` y) == findMax x `max` findMax y" $
            markNonEmpty $ \x -> markNonEmpty $ \y ->
                findMax (x `union` y) === findMax x `max` findMax y,
          testProperty "not member str => delete str . insert str == id" $
            markNonEmpty $ \tree str ->
                not (str `member` tree) ==> delete str (insert str tree) === tree
        ]
  where
    minJust (Just x) (Just y) = Just (x `min` y)
    minJust mx Nothing = mx
    minJust Nothing my = my

    markNonEmpty :: Testable prop => (RadixTree Char -> prop) -> SameKeyVal Char -> Property
    markNonEmpty prop (SameKeyVal tree) =
        classify (not (null tree)) "non-empty" (prop tree)
