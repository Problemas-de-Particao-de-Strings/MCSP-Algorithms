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
    PSOPure (..),
    PSOCombine (..),
) where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool (Bool (..))
import Data.Eq (Eq (..))
import Data.Foldable qualified as Foldable (length)
import Data.Foldable1 (foldMap1')
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List qualified as List (take)
import Data.List.Extra (sumOn')
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
import MCSP.Data.String (String (Unboxed))
import MCSP.Data.String.Extra (Partition)
import MCSP.Heuristics.Combine (UseSingletons (..), combine, combineP)
import MCSP.Heuristics.Greedy (greedy)
import MCSP.System.Random (Random, Seed, generateWith)

-- --------------- --
-- Meta Parameters --

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

-- | Run PSO only, without using other heuristics.
newtype PSOPure = PSOPure Bool
    deriving newtype (Eq, Ord, Show)

instance MetaVariable PSOPure

-- | Run combine after on the partitions represented by the edge list.
newtype PSOCombine = PSOCombine Bool
    deriving newtype (Eq, Ord, Show)

instance MetaVariable PSOCombine

-- -------------------- --
-- Edge List Operations --
-- -------------------- --

-- | Default updater consider local best, global best and random components.
defaultUpdater :: Updater Edge
defaultUpdater =
    sumM
        [ randomVelocity >>= weighted 2.0,
          weighted 0.1 localGuideDirection,
          weighted 0.1 globalGuideDirection
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
initialWeights :: Ord a => PSOPure -> Pair (String a) -> Vector Edge -> Random (Vector Weight)
initialWeights (PSOPure True) _ edges = uniformSN $ length edges
initialWeights (PSOPure False) strs edges =
    choice
        [ (1, uniformSN $ length edges),
          (3, edgeSizeWeights edges),
          (3, partitionWeights (evalMeta $ combineS strs) edges),
          (3, partitionWeights (evalMeta $ greedy strs) edges)
        ]
  where
    combineS s = combine s <:: UseSingletons True

-- | Resolve the partitions of an edge list and run combine on that.
combineEdges :: Ord a => Pair (String a) -> Vector Edge -> Pair (Partition a)
combineEdges strs@(Unboxed, _) edges =
    evalMeta $ combineP partitions <:: UseSingletons True
  where
    partitions = toPartitions strs $ solution edges

-- | Calculate the mergeness of a given partition.
mergenessOf :: Pair (Partition a) -> Int
mergenessOf (x, y) = mness x `min` mness y
  where
    mness p = sumOn' Foldable.length p - Foldable.length p

-- | The grading function for PSO.
objective :: Ord a => PSOCombine -> Pair (String a) -> Vector Edge -> Int
objective (PSOCombine True) strs = mergenessOf . combineEdges strs
objective (PSOCombine False) _ = mergeness . solution

-- --------- --
-- Heuristic --
-- --------- --

-- | Create an iterated PSO swarm for the MCSP problem.
mcspSwarm :: Ord a => Pair (String a) -> Meta (Random (NonEmpty (Swarm Edge)))
mcspSwarm strs@(edgeSet -> edges) = do
    PSOIterations iterations <- getVarOrDefault (PSOIterations 100)
    PSOParticles particles <- getVarOrDefault (PSOParticles 200)
    usePure <- getVarOrDefault (PSOPure False)
    runCombine <-
        if usePure == PSOPure False
            then getVarOrDefault (PSOCombine False)
            else pure (PSOCombine False)

    let ?eval = fromIntegral . objective runCombine strs
    let ?values = edges
    let initial = initialWeights usePure strs edges
    pure $
        take iterations
            <$> particleSwarmOptimization defaultUpdater initial particles
  where
    take n (x :| xs) = x :| List.take (n - 1) xs

-- | Extract information about the PSO execution.
evalPso :: Pair (String a) -> NonEmpty (Swarm Edge) -> (Pair (Partition a), Int)
evalPso strs swarms = (partitions, firstBestIter)
  where
    partitions = toPartitions strs $ solution $ sortedValues guide
    optimal swarm =
        -- get the global guide with maximum grade and minimum iteration
        ( Min (-guideGrade (gGuide swarm), iteration swarm),
          -- and get the last guide
          Last (gGuide swarm)
        )
    (Min (_, firstBestIter), Last guide) = foldMap1' optimal swarms

-- | PSO heuristic.
pso :: Ord a => Pair (String a) -> Meta (Pair (Partition a))
pso strs = do
    PSOSeed seed <- getVarOrDefault (PSOSeed defaultSeed)

    swarms <- generateWith seed <$> mcspSwarm strs
    let (partitions, firstBestIter) = evalPso strs swarms

    setVar (PSOFirstBestIter firstBestIter)
    pure partitions
  where
    defaultSeed = (0x7f166a5f52178da7, 0xe190ca41e26454c3)
