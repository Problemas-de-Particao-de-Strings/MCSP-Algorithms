module MCSP.Tests.VectorAlgorithms (vectorAlgorithmsTests) where

import Control.Applicative (liftA2, pure)
import Data.Bool (Bool (..), not, (||))
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Ord (Ord (..))
import Data.Tuple (fst, snd)
import Data.Vector.Unboxed (Unbox, Vector, backpermute, imap, length, null, (!))
import GHC.Float (Double, Float)
import GHC.Num (Num (..))
import Text.Show (Show)

import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.QuickCheck (
    Arbitrary,
    Testable,
    classify,
    testProperty,
    (===),
    (==>),
    pattern Fn,
 )

import MCSP.Algorithms.Vector (
    argSort,
    choice,
    choose,
    map,
    normalized,
    replicate,
    sort,
    sortLike,
    sortOn,
    standardized,
    sum,
    sumM,
    uniformRN,
    weighted,
    weightedN,
    zeros,
    (.*),
    (.*.),
    (.+),
    (.-),
 )
import MCSP.QuickCheck.Modifiers (getRandom, getViaList, (=~=))
import MCSP.System.Random (uniformR)

vectorAlgorithmsTests :: TestTree
vectorAlgorithmsTests =
    testGroup
        "Algorithms.Vector"
        [ elementWiseOpsTests,
          sortingTests,
          statisticsTests,
          randomizedVectorsTests
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
            classify (length v1 == length v2) "same-length" $
                sum [v1, v2] === v2 .+ v1,
          testVector @Bool "choose False True == id" $ \vec ->
            choose False True vec === vec
        ]

sortingTests :: TestTree
sortingTests =
    testGroup
        "sorting vectors"
        [ testVector @Int "allIncreasing (sort vec)" $ \vec ->
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

statisticsTests :: TestTree
statisticsTests =
    testGroup
        "statistics on vectors"
        [ testVector @Double "-1 <= normalized vec <= 1" $ \vec ->
            map abs (normalized vec) <= replicate (length vec) 1,
          testVector @Float "map signum normalized vec == map signum vec" $ \vec ->
            map signum (normalized vec) === map signum vec,
          testVector @Double "normalized . normalized == normalized" $ \vec ->
            normalized (normalized vec) === normalized vec,
          testVector @Float "normalized . (x .*.) == normalized" $ \vec x ->
            x > 0 ==> normalized (x .*. vec) === normalized vec,
          testVector @Double "standardized . standardized == standardized" $ \vec ->
            standardized (standardized vec) === standardized vec,
          testVector @Float "standardized . (x .*.) == standardized" $ \vec x ->
            x > 0 ==> standardized (x .*. vec) === standardized vec,
          testVector @Double "standardized . normalized == standardized" $ \vec ->
            standardized (normalized vec) === standardized vec
        ]

randomizedVectorsTests :: TestTree
randomizedVectorsTests =
    testGroup
        "randomized vector operations"
        [ testProperty "sumM [v1, v2] =~= liftA2 (.+) v1 v2" $
            \(getRandomVector -> v1) (getRandomVector -> v2) ->
                sumM @Int [v1, v2] =~= liftA2 (.+) v1 v2,
          testProperty "choice [(1, v1)] =~= v1" $ \(getViaList -> v1) ->
            choice @(Vector Double) [(1, pure v1)] =~= pure v1,
          testProperty "(>>= weighted x) =~= liftA2 (.*.) (uniformR 0 x)" $
            \(getViaList -> v1) x ->
                weighted @Int x v1 =~= liftA2 (.*.) (uniformR 0 x) (pure v1),
          testProperty "(>>= weightedN x) =~= liftA2 (.*) (uniformRN 0 x)" $
            \(getViaList -> v1) x ->
                weightedN @Int x v1 =~= liftA2 (.*) (uniformRN 0 x (length v1)) (pure v1)
        ]
  where
    getRandomVector arb = getViaList <$> getRandom arb
