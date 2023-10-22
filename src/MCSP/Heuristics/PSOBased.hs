module MCSP.Heuristics.PSOBased (
    pso,
    partitionWeights,
    edgeSizeWeights,
) where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (reverse)
import Data.List.NonEmpty (take)
import Data.Ord (Ord)
import Data.Vector.Unboxed (Vector, length, map)
import GHC.Err (error)
import GHC.Num (negate)
import GHC.Real (fromIntegral)

import MCSP.Algorithms.PSO (
    Swarm (..),
    Updater,
    Weight,
    globalGuideDirection,
    localGuideDirection,
    particleSwarmOptimization,
    randomVelocity,
    sortedValues,
 )
import MCSP.Algorithms.Vector (
    argSort,
    choice,
    choose,
    sort,
    sortLike,
    sumM,
    uniformSN,
    weighted,
    weightedN,
 )
import MCSP.Data.MatchingGraph (
    Edge,
    blockLen,
    compatibleEdges,
    edgeSet,
    mergeness,
    solution,
    toPartitions,
 )
import MCSP.Data.Pair (Pair)
import MCSP.Data.String (String)
import MCSP.Data.String.Extra (Partition)
import MCSP.Heuristics.Combine (combineS)
import MCSP.Heuristics.Greedy (greedy)
import MCSP.System.Random (Random, Seed, generateWith)

-- | Default updater consider local best, global best and random components.
defaultUpdater :: Updater Edge
defaultUpdater =
    sumM
        [ randomVelocity >>= weighted 1.2,
          weighted 0.005 localGuideDirection,
          weighted 0.005 globalGuideDirection
        ]

-- | Produce random weights for an edge set such that a given
-- partition would be the result of creating a solution using those weights.
--
-- >>> let ps = (["a", "ba", "b"], ["a", "b", "ba"])
-- >>> let es = [((0,0),2),((1,2),2),((2,0),2)]
-- >>> compatibleEdges ps es
-- [False,True,False]
--
-- >>> generateWith (1,2) $ partitionWeights ps es
-- [0.6502342,-0.8818351,7.536712e-2]
partitionWeights :: Pair (Partition a) -> Vector Edge -> Random (Vector Weight)
partitionWeights p es = weightedN 1 $ choose 1 (-1) (compatibleEdges p es)

-- | Produce random weights for an edge set sorted in such a way
-- that longer edges are prioritized.
-- >>> generateWith (1,2) $ edgeSizeWeights [((0,0),4),((1,2),1),((2,0),10)]
-- [0.30046844,0.7636702,-0.84926575]
edgeSizeWeights :: Vector Edge -> Random (Vector Weight)
edgeSizeWeights es = do
    weights <- sort <$> uniformSN (length es)
    let indices = argSort $ map (negate . blockLen) es
    pure $ sortLike weights indices

-- | Generates the initial weights for a particle.
initialWeights :: Ord a => Pair (String a) -> Vector Edge -> Random (Vector Weight)
initialWeights strs edges =
    choice
        [ (1, uniformSN $ length edges),
          (1, edgeSizeWeights edges),
          (1, partitionWeights (combineS strs) edges),
          (1, partitionWeights (greedy strs) edges)
        ]

-- --------- --
-- Heuristic --
-- --------- --

-- | Some meta-parameters for the PSO Heuristic.
type PSOParams =
    ( ?iterations :: Int,
      ?particles :: Int,
      ?seed :: Seed
    )

-- | Create an iterated PSO swarm for the MCSP problem.
mcspSwarm :: (Ord a, PSOParams) => Pair (String a) -> Random [Swarm Edge]
mcspSwarm strs@(edgeSet -> edges) =
    take ?iterations <$> particleSwarmOptimization defaultUpdater ?initialWeights ?particles
  where
    ?eval = fromIntegral . mergeness . solution
    ?values = edges
    ?initialWeights = initialWeights strs edges

-- | PSO heuristic with implicit parameters.
psoWithParams :: (Ord a, PSOParams) => Pair (String a) -> Pair (Partition a)
psoWithParams strs = generateWith ?seed $ do
    swarms <- mcspSwarm strs
    case reverse swarms of
        swarm : _ -> pure $ toPartitions strs $ solution $ sortedValues $ gGuide swarm
        [] -> error "pso: mcspSwarm generated no swarms"

-- ------------------ --
-- Default Parameters --

-- | PSO heuristic.
pso :: Ord a => Pair (String a) -> Pair (Partition a)
pso = psoWithParams
  where
    ?iterations = 10
    ?particles = 1000
    ?seed = (0x7f166a5f52178da7, 0xe190ca41e26454c3)
