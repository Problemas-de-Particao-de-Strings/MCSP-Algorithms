module MCSP.Tests.MatchingGraph (matchingGraphTests) where

import Control.Applicative (pure, (<$>))
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
import Data.Tuple (snd)
import Data.Vector.Generic qualified as Vector (ifilter, length, (!))
import GHC.IsList (toList)
import GHC.Num (negate, (-))

import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.QuickCheck (Testable, collect, testProperty, (===))

import MCSP.Algorithms.Vector (sortLike, sortOn)
import MCSP.Data.MatchingGraph (
    blockLen,
    compatibleEdges,
    edgeSet,
    mergeness,
    solution,
    solutions,
    toPartitions,
 )
import MCSP.Data.Pair (Pair, both, first, left, right, swap)
import MCSP.Data.String (String (..), concat, slice)
import MCSP.Data.String.Extra (chars)
import MCSP.Heuristics.PSOBased (edgeSizeWeights, partitionWeights)
import MCSP.QuickCheck.Modifiers (getBalancedStrings)
import MCSP.System.Random ((=~=))

matchingGraphTests :: TestTree
matchingGraphTests =
    testGroup
        "MatchingGraph"
        [edgeSetTests, solutionTests, psoBasedTests]

rangeOf :: Int -> Interval Int
rangeOf val =
    fromMaybe whole $
        find (val `member`) $
            id @[Interval Int]
                [ 0 <=..<= 10,
                  11 <=..<= 50,
                  51 <=..<= 99,
                  100 <=..<= 200,
                  200 <=..<= inf
                ]

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
            sort (map (first swap) (edges strs)) === sort (edges (swap strs)),
          testPair "toPartitions $ solutions $ compatibleEdges === toPartitions str $ solutions" $
            \strs ->
                let es = edgeSet strs
                    partitions = toPartitions strs $ solution es
                    compatible = compatibleEdges partitions es
                 in toPartitions strs (solution $ select compatible es) === partitions
        ]
  where
    block get str (start, k) = slice (get start) k str
    blocks get str = map (block get str) . toList
    edges = toList . edgeSet
    select which = Vector.ifilter (\i _ -> which Vector.! i)

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

psoBasedTests :: TestTree
psoBasedTests =
    testGroup
        "pso heuristic use valid initial weights"
        [ testPair "solution $ sortLike $ partitionWeights =~= solution" $ \strs ->
            let es = edgeSet strs
                partitions = toPartitions strs $ solution es
                weights = partitionWeights partitions es
             in (toPartitions strs . solution . sortLike es <$> weights) =~= pure partitions,
          testPair "sortLike $ edgeSizeWeights =~= sortOn (negate . blockLen)" $ \strs ->
            let es = edgeSet strs
             in (sortLike es <$> edgeSizeWeights es) =~= pure (sortOn (negate . blockLen) es)
        ]
