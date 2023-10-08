module MCSP.Tests.MatchingGraph (matchingGraphTests) where

import Data.Char (Char)
import Data.Eq ((/=))
import Data.ExtendedReal (inf)
import Data.Foldable (length)
import Data.Function (id, ($), (.))
import Data.Int (Int)
import Data.Interval (Interval, member, whole, (<=..<=))
import Data.List (find, map, replicate, sort)
import Data.List.Extra (nubSort)
import Data.List.NonEmpty (last)
import Data.Maybe (fromMaybe)
import Data.Vector.Generic qualified as Vector (length)
import GHC.IsList (toList)
import GHC.Num ((-))

import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.QuickCheck (Testable, collect, testProperty, (===))

import MCSP.Data.MatchingGraph (edgeSet, mergeness, solution, solutions, toPartitions)
import MCSP.Data.Pair (Pair, both, first, left, right, snd, swap)
import MCSP.Data.String (String (..), concat, slice)
import MCSP.Data.String.Extra (BalancedStrings (..), chars)

matchingGraphTests :: TestTree
matchingGraphTests =
    testGroup
        "MatchingGraph"
        [edgeSetTests, solutionTests]

rangeOf :: Int -> Interval Int
rangeOf val =
    fromMaybe whole $
        find (val `member`) $
            list
                [ 0 <=..<= 10,
                  11 <=..<= 50,
                  51 <=..<= 99,
                  100 <=..<= 200,
                  200 <=..<= inf
                ]
  where
    list :: [a] -> [a]
    list = id

testPair :: Testable prop => TestName -> (Pair (String Char) -> prop) -> TestTree
testPair name prop =
    testProperty name $ \(getBalancedStrings -> strs) ->
        collect (rangeOf $ Vector.length (edgeSet strs)) (prop strs)

edgeSetTests :: TestTree
edgeSetTests =
    testGroup
        "edge set properties"
        [ testPair "blocks left edgeSet === blocks right edgeSet" $ \strs ->
            blocks left (left strs) (edges strs) === blocks right (right strs) (edges strs),
          testPair "map length $ blocks left edgeSet === map blockLen edgeSet" $ \strs ->
            map length (blocks left (left strs) (edges strs)) === map snd (edges strs),
          testPair "nubSort edgeSet === sort edgeSet" $ \strs ->
            nubSort (edges strs) === sort (edges strs),
          testPair "map (first swap) $ edgeSet strs === edgeSet $ swap strs" $ \strs ->
            sort (map (first swap) (edges strs)) === sort (edges (swap strs))
        ]
  where
    block get str (start, k) = slice (get start) k str
    blocks get str = map (block get str) . toList
    edges = toList . edgeSet

solutionTests :: TestTree
solutionTests =
    testGroup
        "solutions via edge lists are valid"
        [ testPair "toPartitions . solution [] == chars" $ \strs ->
            toPartitions strs (solution []) === (chars `both` strs),
          testPair "toPartitions . last . solutions == chars" $ \strs ->
            toPartitions strs (lastNE $ solutions $ edgeSet strs) === (chars `both` strs),
          testPair "map (concat . toPartitions) solutions == repeat" $ \strs ->
            let sols = toList $ solutions $ edgeSet strs
             in map (both concat . toPartitions strs) sols === replicate (length sols) strs,
          testPair "map (fst toPartitions) solutions == map (snd toPartitions) solutions" $ \strs ->
            let partitions = map (toPartitions strs) $ toList $ solutions $ edgeSet strs
             in map (sort . left) partitions === map (sort . right) partitions,
          testPair "mergeness solution == length str - length (toPartitions solution)" $ \strs ->
            let sols = toList $ solutions $ edgeSet strs
             in map mergeness sols === map (calcMergeness strs) sols
        ]
  where
    lastNE = last
    calcMergeness strs (toPartitions strs -> parts) =
        let (nl, nr) = length `both` strs
            (pl, pr) = length `both` parts
            (ml, mr) = (nl - pl, nr - pr)
         in if ml /= mr then -ml else mr
