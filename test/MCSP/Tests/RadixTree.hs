module MCSP.Tests.RadixTree (radixTreeTests) where

import Data.Bool (Bool)
import Data.Char (Char)
import Data.Eq ((/=), (==))
import Data.Foldable (Foldable (..))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List.Extra (map, nubSort, snoc)
import GHC.IsList (fromList)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Property, classify, testProperty)

import MCSP.Data.RadixTree (construct)
import MCSP.Data.String (String, Unbox, concat)

radixTreeTests :: TestTree
radixTreeTests = testGroup "RadixTree" [radixTreeFoldsAreSorted]

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
