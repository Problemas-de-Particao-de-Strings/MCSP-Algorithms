module MCSP.Tests.VectorAlgorithms (vectorAlgorithmsTests) where

import Data.Bool (Bool (..), not, (||))
import Data.Function (($))
import Data.Int (Int)
import Data.Ord (Ord (..))
import Data.Tuple (fst, snd)
import Data.Vector.Unboxed (Unbox, Vector, backpermute, imap, length, null, (!))
import GHC.Float (Double, Float)
import GHC.Num ((-))
import Text.Show (Show)

import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary, Testable, classify, testProperty, (===), pattern Fn)

import MCSP.Algorithms.Vector (
    argSort,
    choose,
    map,
    replicate,
    sort,
    sortLike,
    sortOn,
    sum,
    zeros,
    (.*),
    (.*.),
    (.+),
    (.-),
 )
import MCSP.QuickCheck.Modifiers (getViaList)

vectorAlgorithmsTests :: TestTree
vectorAlgorithmsTests =
    testGroup
        "Algorithms.Vector"
        [ elementWiseOpsTests,
          sortingTests
        ]

testVector ::
    (Unbox a, Arbitrary a, Show a, Testable prop) =>
    TestName
    -> (Vector a -> prop)
    -> TestTree
testVector name prop = testProperty name $ \(getViaList -> vec) ->
    classify (not (null vec)) "non-null" (prop vec)

elementWiseOpsTests :: TestTree
elementWiseOpsTests =
    testGroup
        "element-wise operations"
        [ testVector @Double "vec + 0 == vec" $ \vec ->
            vec .+ zeros (length vec) === vec,
          testVector @Int "vec - vec == 0" $ \vec ->
            vec .- vec === zeros (length vec),
          testVector @Float "vec .* 0 == 0" $ \vec ->
            vec .* zeros (length vec) === zeros (length vec),
          testVector @Int "x .*. vec == replicate x .* vec" $ \vec x ->
            x .*. vec === replicate (length vec) x .* vec,
          testVector @Float "sum [v1, v2] == v2 .+ v1" $ \v1 (getViaList -> v2) ->
            sum [v1, v2] === v2 .+ v1,
          testVector @Bool "choose False True == id" $ \vec ->
            choose False True vec === vec
        ]

sortingTests :: TestTree
sortingTests =
    testGroup
        "sorting vectors"
        [ testVector @Double "allIncreasing (sort vec)" $ \vec ->
            imap (\i x -> i <= 0 || vec ! (i - 1) <= x) (sort vec) === replicate (length vec) True,
          testVector @Double "sort . sort == sort" $ \vec ->
            sort (sort vec) === sort vec,
          testVector @(Int, Float) "sortOn fst . sortOn snd == sort" $ \vec ->
            sortOn fst (sortOn snd vec) === sort vec,
          testVector @Float "backpermute . argSort == sort" $ \vec ->
            backpermute vec (argSort vec) === sort vec,
          testVector @Int "sortLike vec (map f vec) == sortOn f vec" $ \vec (Fn f) ->
            vec `sortLike` map @Int @Double f vec === sortOn f vec
        ]
