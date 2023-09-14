module MCSP.Tests.RadixTree (radixTreeTests) where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool (Bool, not)
import Data.Char (Char)
import Data.Eq ((/=), (==))
import Data.Foldable (Foldable (..))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List.Extra (map, nubSort, snoc)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord (Ord, max, min)
import Data.Semigroup (Max (..), Min (..))
import GHC.IsList (fromList)
import GHC.Num ((-))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (
    Arbitrary (..),
    Property,
    Testable,
    classify,
    oneof,
    sized,
    suchThat,
    testProperty,
 )

import MCSP.Data.RadixTree (
    RadixTree,
    construct,
    delete,
    empty,
    findMax,
    findMin,
    insert,
    member,
    union,
 )
import MCSP.Data.String (String, Unbox, concat)

radixTreeTests :: TestTree
radixTreeTests = testGroup "RadixTree" [radixTreeFoldsAreSorted, treeStructureHolds]

instance (Unbox a, Arbitrary a) => Arbitrary (String a) where
    arbitrary = fromList <$> arbitrary
    shrink = map fromList . shrink . toList

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

instance (Unbox a, Arbitrary a, Ord a) => Arbitrary (RadixTree a) where
    arbitrary = sized go
      where
        go 0 = pure empty
        go n = oneof [singleton <$> arbitrary, go (n - 1) >>= unique]
        singleton x = construct [x]
        unique tree = do
            str <- suchThat arbitrary (not . (`member` tree))
            pure (insert str tree)
    shrink tree =
        [ empty,
          fromMaybe findMin tree,
          fromMaybe findMax tree,
          without findMin tree,
          without findMax tree
        ]
      where
        fromMaybe get t = construct (toList (get t))
        without get t = case get t of
            Just x -> delete x t
            Nothing -> t

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
