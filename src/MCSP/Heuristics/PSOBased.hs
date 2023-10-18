module MCSP.Heuristics.PSOBased (
    pso,
) where

import Control.Applicative (pure)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (reverse)
import Data.List.NonEmpty (take)
import Data.Ord (Ord)
import Data.Vector.Unboxed (Vector, length, replicateM)
import GHC.Err (error)
import GHC.Float (Double)
import GHC.Real (fromIntegral)

import MCSP.Algorithms.PSO (
    Swarm (..),
    Updater,
    globalGuideDirection,
    localGuideDirection,
    particleSwarmOptimization,
    randomVelocity,
    sortedValues,
    sumVelocities,
    weighted,
 )
import MCSP.Data.MatchingGraph (Edge, edgeSet, mergeness, solution, toPartitions)
import MCSP.Data.Pair (Pair)
import MCSP.Data.String (String)
import MCSP.Data.String.Extra (Partition)
import MCSP.System.Random (Random, generateWith, uniformR)

-- | Generate a random position given the number of coordinates.
randomWeights :: Int -> Random (Vector Double)
randomWeights size = replicateM size (uniformR (-1.0) 1.0)

-- | Default updater consider local best, global best and random components.
defaultUpdater :: Updater Edge
defaultUpdater =
    sumVelocities
        [ weighted 1.2 randomVelocity,
          weighted 0.005 localGuideDirection,
          weighted 0.005 globalGuideDirection
        ]

-- | Number of iterations in the particle swarm optimization.
maximumIterations :: Int
maximumIterations = 10

-- | Number of particles used for each swarm.
numberOfParticles :: Int
numberOfParticles = 1000

-- --------- --
-- Heuristic --
-- --------- --

-- | Create an iterated PSO swarm for the MCSP problem.
mcspSwarm :: Ord a => Int -> Int -> Pair (String a) -> Random [Swarm Edge]
mcspSwarm iterations particles (edgeSet -> es) =
    take iterations
        <$> particleSwarmOptimization defaultUpdater weight es (randomWeights (length es)) particles
  where
    weight = fromIntegral . mergeness . solution

-- | Monadic PSO heuristic.
psoM :: Ord a => Pair (String a) -> Random (Pair (Partition a))
psoM strs = do
    swarms <- mcspSwarm maximumIterations numberOfParticles strs
    case reverse swarms of
        swarm : _ -> pure $ toPartitions strs $ solution $ sortedValues $ gGuide swarm
        [] -> error "pso: mcspSwarm generated no swarms"

-- | PSO heuristic.
pso :: Ord a => Pair (String a) -> Pair (Partition a)
pso strs = generateWith seed (psoM strs)
  where
    -- from https://www.random.org
    seed = (0x7f166a5f52178da7, 0xe190ca41e26454c3)
