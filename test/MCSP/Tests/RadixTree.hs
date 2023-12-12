module MCSP.Tests.RadixTree (radixTreeTests) where

import Data.Bool (not)
import Data.Char (Char)
import Data.Eq ((/=))
import Data.Foldable (Foldable (..))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List.Extra (map, nubSort, snoc)
import Data.Maybe (Maybe (..))
import Data.Ord (max, min)
import Data.Semigroup (Max (..), Min (..))
import Data.Word (Word8)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (classify, testProperty, (===), (==>))

import MCSP.Data.RadixTree (
    construct,
    delete,
    findMax,
    findMin,
    insert,
    member,
    union,
 )
import MCSP.Data.String (concat)
import MCSP.QuickCheck.Modifiers (getTree, getViaList)

radixTreeTests :: TestTree
radixTreeTests =
    testGroup
        "RadixTree"
        [ radixTreeFoldsAreSorted,
          treeStructureHolds
        ]

radixTreeFoldsAreSorted :: TestTree
radixTreeFoldsAreSorted =
    testGroup
        "folds are sorted and deduplicated"
        [ testFromList "toList . construct == nubSort" $ \list ->
            toList (construct @Char list) === nubSort list,
          testFromList "foldMap (: []) . construct == nubSort" $ \list ->
            foldMap (: []) (construct @Int list) === nubSort list,
          testFromList "foldMap' (: []) . construct == nubSort" $ \list ->
            foldMap' (: []) (construct @Word8 list) === nubSort list,
          testFromList "foldr (:) [] . construct == nubSort" $ \list ->
            foldr (:) [] (construct @Char list) === nubSort list,
          testFromList "foldr' (:) [] . construct == nubSort" $ \list ->
            foldr' (:) [] (construct @Int list) === nubSort list,
          testFromList "foldl snoc [] . construct == nubSort" $ \list ->
            foldl snoc [] (construct @Word8 list) === nubSort list,
          testFromList "foldl' snoc [] . construct == nubSort" $ \list ->
            foldl' snoc [] (construct @Char list) === nubSort list,
          testFromList "fold . construct == concat . nubSort" $ \list ->
            fold (construct @Int list) === concat (nubSort list)
        ]
  where
    testFromList name prop = testProperty name $ \(map getViaList -> xs) ->
        classify (length xs /= length (nubSort xs)) "deduplicated" (prop xs)

treeStructureHolds :: TestTree
treeStructureHolds =
    testGroup
        "tree structure holds"
        [ testProperty "findMin == getMin <$> foldMap (Just . Min)" $
            markNonEmpty $ \tree ->
                findMin @Char tree === (getMin <$> foldMap' (Just . Min) tree),
          testProperty "findMax == getMax <$> foldMap (Just . Max)" $
            markNonEmpty $ \tree ->
                findMax @Int tree === (getMax <$> foldMap' (Just . Max) tree),
          testProperty "findMin (x `union` y) == findMin x `min` findMin y" $
            markNonEmpty $ \x -> markNonEmpty $ \y ->
                findMin @Word8 (x `union` y) === findMin x `minJust` findMin y,
          testProperty "findMax (x `union` y) == findMax x `max` findMax y" $
            markNonEmpty $ \x -> markNonEmpty $ \y ->
                findMax @Char (x `union` y) === findMax x `max` findMax y,
          testProperty "not member str => delete str . insert str == id" $
            markNonEmpty $ \tree (getViaList -> str) ->
                not (str `member` tree) ==> delete @Int str (insert str tree) === tree
        ]
  where
    minJust (Just x) (Just y) = Just (x `min` y)
    minJust mx Nothing = mx
    minJust Nothing my = my

    markNonEmpty prop (getTree -> tree) =
        classify (not (null tree)) "non-empty" (prop tree)
