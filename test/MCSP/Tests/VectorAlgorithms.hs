module MCSP.Tests.VectorAlgorithms (vectorAlgorithmsTests) where

import Data.Bool (Bool (..))
import Data.Function (($), (.))
import Data.Int (Int)
import Data.Vector.Unboxed (Unbox, Vector, length)
import GHC.Float (Double, Float)
import Text.Show (Show)

import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary, Testable, testProperty, (===))

import MCSP.Algorithms.Vector (
    choose,
    replicate,
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
        [elementWiseOpsTests]

testVector ::
    (Unbox a, Arbitrary a, Show a, Testable prop) =>
    TestName
    -> (Vector a -> prop)
    -> TestTree
testVector name prop = testProperty name (prop . getViaList)

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
          testVector "choose False True == id" $ \vec ->
            choose False True vec === vec
        ]
