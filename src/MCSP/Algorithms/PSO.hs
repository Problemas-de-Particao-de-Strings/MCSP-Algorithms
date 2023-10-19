module MCSP.Algorithms.PSO (
    -- * Updaters
    Updater,
    randomVelocity,
    globalGuideDirection,
    localGuideDirection,
    previousVelocity,
    scaleVelocity,
    weighted,
    weightedN,
    sumVelocities,

    -- * Data structures
    PsoGuide (..),
    Particle (..),
    Swarm (..),
    particleSwarmOptimization,
) where

import Control.Applicative (pure, (<$>))
import Control.Monad (fmap, mapM, (>>=))
import Control.Monad qualified as Monad (replicateM)
import Data.Eq (Eq, (==))
import Data.Foldable1 (foldl1')
import Data.Function (on, ($))
import Data.Int (Int)
import Data.List (intercalate, (++))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty.Extra (maximum1)
import Data.Maybe (Maybe (..))
import Data.Ord (Ord (compare), max, (<=), (>=))
import Data.Tuple (fst, snd)
import Data.Vector.Algorithms.Merge (sortBy)
import Data.Vector.Unboxed (Unbox, Vector, length, map, modify, replicate, replicateM, zip, zipWith)
import GHC.Err (error)
import GHC.Exts (fromListN, toList)
import GHC.Float (Double)
import GHC.Num ((*), (+), (-))
import GHC.Stack (HasCallStack)
import Numeric (showFFloat)
import Text.Show (Show, show, showListWith)

import MCSP.System.Random (Random, iterateR, uniformR)

-- -------------------------------------------------------------
-- Based on https://github.com/brianshourd/haskell-Calypso
-- -------------------------------------------------------------

-- | Represents an evaluation of how good a solution is.
type Grade = Double

-- | Element of a vector used to sort values of a permutation problem.
type Weight = Double

-- | Function used to evaluate a permutation.
type EvalFunction a = Vector a -> Grade

-- ----- --
-- Guide --
-- ----- --

-- | Information about a specific position (weights) and
-- the value of the objective function at that point.
data PsoGuide a = PsoGuide
    { -- | Position (weights).
      guideWeights :: Vector Weight,
      -- | Grade of the position.
      guideGrade :: Grade,
      -- | Values sorted by weights.
      sortedValues :: Vector a
    }

instance Show (PsoGuide a) where
    show g = showListWith showF (toList $ guideWeights g) " -> " ++ show (guideGrade g)
      where
        showF = showFFloat (Just 3)

instance Eq (PsoGuide a) where
    PsoGuide _ x _ == PsoGuide _ y _ = x == y

instance Ord (PsoGuide a) where
    PsoGuide _ x _ <= PsoGuide _ y _ = x <= y

-- -------- --
-- Particle --
-- -------- --

-- | A single particle of a swarm.
data Particle a = Particle
    { -- | Position of the particle.
      particleWeights :: Vector Weight,
      -- | Velocity of the particle.
      vel :: Vector Weight,
      -- | Best position the particle found so far.
      pGuide :: PsoGuide a
    }

instance Show (Particle a) where
    show Particle {..} =
        intercalate
            "\n"
            [ "Particle:",
              "- Position: " ++ show particleWeights,
              "- Guide: " ++ show pGuide
            ]

-- -------- --
-- Updaters --
-- -------- --

-- | Implicit parameters used in `Updater`.
type UpdaterContext a =
    ( ?particle :: Particle a,
      ?global :: PsoGuide a,
      ?iteration :: Int
    )

-- | Evaluate the new velocity of a particle using the global best and iteration number.
type Updater a = UpdaterContext a => Random (Vector Weight)

-- | Produce random velocity with components up to given limit.
randomVelocityN :: Int -> Random (Vector Weight)
randomVelocityN n = replicateM n (uniformR (-1.0) 1.0)

-- | Produce random velocity with components up to given limit.
randomVelocity :: UpdaterContext a => Random (Vector Weight)
randomVelocity = randomVelocityN (length $ particleWeights ?particle)
{-# INLINE randomVelocity #-}

-- | Produce random velocity in the direction of the current global best,
-- covering a random portion of the distance between them up to the given limit.
globalGuideDirection :: UpdaterContext a => Random (Vector Weight)
globalGuideDirection = pure (guideWeights ?global .- particleWeights ?particle)
{-# INLINE globalGuideDirection #-}

-- | Produce random velocity in the direction of the current local best,
-- covering a random portion of the distance between them up to the given limit.
localGuideDirection :: UpdaterContext a => Random (Vector Weight)
localGuideDirection = pure (guideWeights (pGuide ?particle) .- particleWeights ?particle)
{-# INLINE localGuideDirection #-}

-- | Just repeats the previous particle velocity.
previousVelocity :: UpdaterContext a => Random (Vector Weight)
previousVelocity = pure (vel ?particle)
{-# INLINE previousVelocity #-}

-- | Multiplies the velocity by a constant factor.
scaleVelocity :: Weight -> Random (Vector Weight) -> Random (Vector Weight)
scaleVelocity factor updater = map (factor *) <$> updater
{-# INLINE scaleVelocity #-}

-- | Scale all components of the velocity by the same random weight.
weighted :: Weight -> Random (Vector Weight) -> Random (Vector Weight)
weighted maxWeight updater = do
    k <- uniformR 0 maxWeight
    scaleVelocity k updater
{-# INLINE weighted #-}

-- | Scale each component of the velocity by a random weight.
weightedN :: Weight -> Random (Vector Weight) -> Random (Vector Weight)
weightedN maxWeight updater = do
    velocity <- updater
    ks <- replicateM (length velocity) (uniformR 0 maxWeight)
    pure (ks .* velocity)
{-# INLINE weightedN #-}

-- | Sum all velocities generated by multiple updaters.
sumVelocities :: NonEmpty (Random (Vector Weight)) -> Random (Vector Weight)
sumVelocities = foldl1' add
  where
    add lhs rhs = do
        x <- lhs
        y <- rhs
        pure (x .+ y)
{-# INLINE sumVelocities #-}

-- ----- --
-- Swarm --
-- ----- --

-- | A swarm for the PSO algorithm.
data Swarm a = Swarm
    { -- | Particles in the swarm.
      parts :: NonEmpty (Particle a),
      -- | Global guide.
      gGuide :: PsoGuide a,
      -- | Current iteration.
      iteration :: Int
    }

instance Show (Swarm a) where
    show Swarm {..} = show iteration ++ " - " ++ show gGuide

-- | Create a particle using the evaluation function, the vector of
-- original values and a specific position.
createParticle ::
    (HasCallStack, Unbox a) =>
    EvalFunction a
    -> Vector a
    -> Vector Weight
    -> Particle a
createParticle eval originalV weights =
    Particle
        { particleWeights =
            if length weights == length originalV
                then weights
                else error "size mismatch while creating particle",
          vel = zeros (length weights),
          pGuide =
            let sortedValues = sortByWeight originalV weights
             in PsoGuide {guideWeights = weights, guideGrade = eval sortedValues, sortedValues}
        }

-- | Creates a swarm from the evaluation function, the vector of
-- original values, the number of particles and a generator of positions.
createSwarm ::
    (HasCallStack, Unbox a) =>
    EvalFunction a
    -> Vector a
    -> Int
    -> Random (Vector Weight)
    -> Random (Swarm a)
createSwarm eval originalV n gen = do
    parts <- fromListN n <$> Monad.replicateM n (createParticle eval originalV <$> gen)
    let gGuide = maximum1 $ fmap pGuide parts
    pure $ Swarm {parts, gGuide, iteration = 0}

-- ---------------- --
-- Iterating swarms --
-- ---------------- --

-- | Updates a single particle using an updater, the evaluation function,
-- the vector of original values and swarm information.
updateParticle ::
    Unbox a =>
    Updater a
    -> EvalFunction a
    -> Vector a
    -> Swarm a
    -> Particle a
    -> Random (Particle a)
updateParticle newVel eval originalV Swarm {..} part = do
    vel <- newVel
    let w' = particleWeights part .+ vel
    let sortedValues = sortByWeight originalV w'
    let newVal = eval sortedValues
    let oldGuide = pGuide part
    let pGuide' =
            if newVal >= guideGrade oldGuide
                then PsoGuide w' newVal sortedValues
                else oldGuide
    pure Particle {particleWeights = w', vel, pGuide = pGuide'}
  where
    ?particle = part
    ?global = gGuide
    ?iteration = iteration

-- | Updates all particles in the swarm once.
updateSwarm :: Unbox a => Updater a -> EvalFunction a -> Vector a -> Swarm a -> Random (Swarm a)
updateSwarm up eval originalV swarm = do
    newParts <- mapM (updateParticle up eval originalV swarm) (parts swarm)
    let bestGuide = maximum1 $ fmap pGuide newParts
    let newGuide = max bestGuide (gGuide swarm)
    pure Swarm {parts = newParts, gGuide = newGuide, iteration = iteration swarm + 1}

-- | Create iterations of a swarm.
iterateSwarm ::
    Unbox a =>
    Updater a
    -> EvalFunction a
    -> Vector a
    -> Swarm a
    -> Random (NonEmpty (Swarm a))
iterateSwarm up eval originalV = iterateR (updateSwarm up eval originalV)

particleSwarmOptimization ::
    Unbox a =>
    Updater a
    -> EvalFunction a
    -> Vector a
    -> Random (Vector Weight)
    -> Int
    -> Random (NonEmpty (Swarm a))
particleSwarmOptimization updater eval values weights size =
    createSwarm eval values size weights >>= iterateSwarm updater eval values

-- ----------------- --
-- Vector operations --
-- ----------------- --

-- | Sort values according to weights.
--
-- >>> sortByWeight [1, 2, 3, 4, 5] [0.1, 0.8, 0.2, 1.0, -0.4] :: Vector Int
-- [5,1,3,2,4]
sortByWeight :: Unbox a => Vector a -> Vector Weight -> Vector a
sortByWeight v weights = map snd (sortValues (zip weights v))
  where
    sortValues = modify (sortBy (compare `on` fst))

-- | Element-wise addition.
--
-- >>> [1, 2, 3] .+ [5, 6, 7]
-- [6.0,8.0,10.0]
(.+) :: Vector Double -> Vector Double -> Vector Double
(.+) = zipWith (+)

-- | Element-wise subtraction.
--
-- >>> [1, 2, 3] .- [5, 6, 7]
-- [-4.0,-4.0,-4.0]
(.-) :: Vector Double -> Vector Double -> Vector Double
(.-) = zipWith (-)

-- | Element-wise multiplication.
--
-- >>> [1, 2, 3] .* [5, 6, 7]
-- [5.0,12.0,21.0]
(.*) :: Vector Double -> Vector Double -> Vector Double
(.*) = zipWith (*)

-- | Create a vector of zeros given the length.
--
-- >>> zeros 3
-- [0.0,0.0,0.0]
-- >>> zeros 0
-- []
zeros :: Int -> Vector Double
zeros s = replicate s 0
