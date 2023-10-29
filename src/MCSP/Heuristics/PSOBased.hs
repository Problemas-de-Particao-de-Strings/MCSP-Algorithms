module MCSP.Heuristics.PSOBased (
    mcspSwarm,
    pso,
    partitionWeights,
    edgeSizeWeights,

    -- * Meta Parameters
    PSOIterations (..),
    PSOParticles (..),
    PSOSeed (..),
    PSOFirstBestIter (..),
) where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool (Bool (..))
import Data.Eq (Eq)
import Data.Foldable1 (foldMap1')
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List qualified as List (take)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ord (Ord (..))
import Data.Semigroup (Last (..), Min (..))
import Data.Vector.Unboxed (Vector, length, map)
import GHC.Num (negate, (-))
import GHC.Real (fromIntegral)
import Text.Show (Show)

import MCSP.Algorithms.PSO (
    PSOGuide (..),
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
import MCSP.Data.Meta (Meta, MetaVariable, evalMeta, getVarOrDefault, setVar, (<::))
import MCSP.Data.Pair (Pair)
import MCSP.Data.String (String)
import MCSP.Data.String.Extra (Partition)
import MCSP.Heuristics.Combine (UseSingletons (..), combine)
import MCSP.Heuristics.Greedy (greedy)
import MCSP.System.Random (Random, Seed, generateWith)

-- | Default updater consider local best, global best and random components.
defaultUpdater :: Updater Edge
defaultUpdater =
    sumM
        [ randomVelocity >>= weighted 5.0,
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
        [ (10, uniformSN $ length edges),
          (1, edgeSizeWeights edges),
          (1, partitionWeights (evalMeta $ combineS strs) edges),
          (1, partitionWeights (evalMeta $ greedy strs) edges)
        ]
  where
    combineS s = combine s <:: UseSingletons True

-- --------- --
-- Heuristic --
-- --------- --

-- | Create an iterated PSO swarm for the MCSP problem.
mcspSwarm :: Ord a => Int -> Pair (String a) -> Random (NonEmpty (Swarm Edge))
mcspSwarm particles strs@(edgeSet -> edges) =
    particleSwarmOptimization defaultUpdater ?initialWeights particles
  where
    ?eval = fromIntegral . mergeness . solution
    ?values = edges
    ?initialWeights = initialWeights strs edges

-- | PSO heuristic with implicit parameters.
psoWithParams :: Ord a => Seed -> Int -> Int -> Pair (String a) -> (Pair (Partition a), Int)
psoWithParams seed iterations particles strs = generateWith seed $ do
    swarms <- take iterations <$> mcspSwarm particles strs
    let (Min (_, firstBestIter), Last guide) = foldMap1' optimal swarms
    let partitions = toPartitions strs $ solution $ sortedValues guide
    pure (partitions, firstBestIter)
  where
    take n (x :| xs) = x :| List.take (n - 1) xs
    optimal swarm =
        -- get the global guide with maximum grade and minimum iteration
        ( Min (-guideGrade (gGuide swarm), iteration swarm),
          -- and get the last guide
          Last (gGuide swarm)
        )

-- -------------------- --
-- With Meta Parameters --

-- | The number of iterations to run the PSO algorithm.
newtype PSOIterations = PSOIterations Int
    deriving newtype (Eq, Ord, Show)

instance MetaVariable PSOIterations

-- | The number of particles used at each iteration of the PSO algorithm.
newtype PSOParticles = PSOParticles Int
    deriving newtype (Eq, Ord, Show)

instance MetaVariable PSOParticles

-- | Initial seed used for randomized operation in the PSO algorithm.
newtype PSOSeed = PSOSeed Seed
    deriving newtype (Eq, Ord, Show)

instance MetaVariable PSOSeed

-- | Output for the first iteration that reached the best solution in PSO.
newtype PSOFirstBestIter = PSOFirstBestIter
    { getFirstBestIter :: Int
    }
    deriving newtype (Eq, Ord, Show)

instance MetaVariable PSOFirstBestIter

-- | PSO heuristic.
pso :: Ord a => Pair (String a) -> Meta (Pair (Partition a))
pso strs = do
    PSOIterations iterations <- getVarOrDefault (PSOIterations 10)
    PSOParticles particles <- getVarOrDefault (PSOParticles 1000)
    PSOSeed seed <- getVarOrDefault (PSOSeed defaultSeed)
    let (partitions, firstBestIter) = psoWithParams seed iterations particles strs
    setVar (PSOFirstBestIter firstBestIter)
    pure partitions
  where
    defaultSeed = (0x7f166a5f52178da7, 0xe190ca41e26454c3)
