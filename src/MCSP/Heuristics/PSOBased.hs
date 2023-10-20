module MCSP.Heuristics.PSOBased (
    pso,
) where

import Control.Applicative (pure)
import Control.Monad (sequence, (>>=))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (reverse)
import Data.List.NonEmpty (take)
import Data.Ord (Ord)
import Data.Vector.Unboxed (length)
import GHC.Err (error)
import GHC.Real (fromIntegral)

import MCSP.Algorithms.PSO (
    Swarm (..),
    Updater,
    globalGuideDirection,
    localGuideDirection,
    particleSwarmOptimization,
    randomVelocity,
    randomWeights,
    sortedValues,
    weighted,
 )
import MCSP.Algorithms.Vector (sum)
import MCSP.Data.MatchingGraph (Edge, edgeSet, mergeness, solution, toPartitions)
import MCSP.Data.Pair (Pair)
import MCSP.Data.String (String)
import MCSP.Data.String.Extra (Partition)
import MCSP.System.Random (Random, Seed, generateWith)

-- | Default updater consider local best, global best and random components.
defaultUpdater :: Updater Edge
defaultUpdater =
    sum
        <$> sequence
            [ randomVelocity >>= weighted 1.2,
              weighted 0.005 localGuideDirection,
              weighted 0.005 globalGuideDirection
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
mcspSwarm (edgeSet -> edges) =
    take ?iterations <$> particleSwarmOptimization defaultUpdater ?initialWeights ?particles
  where
    ?eval = fromIntegral . mergeness . solution
    ?values = edges
    ?initialWeights = randomWeights (length edges)

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
