module MCSP.Heuristics.PSOBased (
    pso,
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
import MCSP.Algorithms.Vector (choice, normalized, sumM, uniformSN, weighted, weightedN)
import MCSP.Data.MatchingGraph (Edge, edgeSet, mergeness, solution, toPartitions)
import MCSP.Data.Pair (Pair)
import MCSP.Data.String (String)
import MCSP.Data.String.Extra (Partition)
import MCSP.System.Random (Random, Seed, generateWith)

-- | Default updater consider local best, global best and random components.
defaultUpdater :: Updater Edge
defaultUpdater =
    sumM
        [ randomVelocity >>= weighted 1.2,
          weighted 0.005 localGuideDirection,
          weighted 0.005 globalGuideDirection
        ]

-- | Generates the initial weight for a particle.
initialWeights :: Vector Edge -> Random (Vector Weight)
initialWeights edges =
    choice
        [ (0.3, uniformSN $ length edges),
          (0.7, weightedN 1 (normalized $ map edgeLen edges))
        ]
  where
    edgeLen (_, n) = fromIntegral n

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
mcspSwarm (edgeSet -> edges) =
    take ?iterations <$> particleSwarmOptimization defaultUpdater ?initialWeights ?particles
  where
    ?eval = fromIntegral . mergeness . solution
    ?values = edges
    ?initialWeights = initialWeights edges

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
    -- from https://www.random.org
    ?seed = (0x7f166a5f52178da7, 0xe190ca41e26454c3)
