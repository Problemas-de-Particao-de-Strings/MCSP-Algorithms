module MCSP.Algorithms.PSO (
    -- * Updaters
    Weight,
    Updater,
    randomVelocity,
    globalGuideDirection,
    localGuideDirection,
    previousVelocity,

    -- * Data structures
    PSOGuide (..),
    Particle (..),
    Swarm (..),
    particleSwarmOptimization,
) where

import Control.Applicative (pure)
import Control.Monad (fmap, mapM, replicateM, (>>=))
import Data.Eq (Eq, (==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (intercalate, (++))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty.Extra (maximum1)
import Data.Maybe (Maybe (..))
import Data.Ord (Ord (..))
import Data.Vector.Unboxed (Unbox, Vector, length)
import GHC.Err (error)
import GHC.Exts (fromListN, toList)
import GHC.Num ((+))
import GHC.Stack (HasCallStack)
import Numeric (showFFloat)
import Text.Show (Show, show, showListWith)

import MCSP.Algorithms.Vector (Default, sortLike, uniformSN, zeros, (.+), (.-))
import MCSP.System.Random (Random, iterateR)

-- -------------------------------------------------------------
-- Based on https://github.com/brianshourd/haskell-Calypso
-- -------------------------------------------------------------

-- | Represents an evaluation of how good a solution is.
type Grade = Int

-- | Element of a vector used to sort values of a permutation problem.
type Weight = Default

-- ----- --
-- Guide --
-- ----- --

-- | Information about a specific position (weights) and
-- the value of the objective function at that point.
data PSOGuide a = PsoGuide
    { -- | Position (weights).
      guideWeights :: Vector Weight,
      -- | Grade of the position.
      guideGrade :: Grade,
      -- | Values sorted by weights.
      sortedValues :: Vector a
    }

instance Show (PSOGuide a) where
    show g = showListWith showF (toList $ guideWeights g) " -> " ++ show (guideGrade g)
      where
        showF = showFFloat (Just 3)

instance Eq (PSOGuide a) where
    PsoGuide _ x _ == PsoGuide _ y _ = x == y

instance Ord (PSOGuide a) where
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
      pGuide :: PSOGuide a
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
      ?global :: PSOGuide a,
      ?iteration :: Int
    )

-- | Evaluate the new velocity of a particle using the global best and iteration number.
type Updater a = UpdaterContext a => Random (Vector Weight)

-- | Produce random velocity with components up to given limit.
randomVelocity :: UpdaterContext a => Random (Vector Weight)
randomVelocity = uniformSN (length $ particleWeights ?particle)
{-# INLINE randomVelocity #-}

-- | Produce random velocity in the direction of the current global best,
-- covering a random portion of the distance between them up to the given limit.
globalGuideDirection :: UpdaterContext a => Vector Weight
globalGuideDirection = guideWeights ?global .- particleWeights ?particle
{-# INLINE globalGuideDirection #-}

-- | Produce random velocity in the direction of the current local best,
-- covering a random portion of the distance between them up to the given limit.
localGuideDirection :: UpdaterContext a => Vector Weight
localGuideDirection = guideWeights (pGuide ?particle) .- particleWeights ?particle
{-# INLINE localGuideDirection #-}

-- | Just repeats the previous particle velocity.
previousVelocity :: UpdaterContext a => Vector Weight
previousVelocity = vel ?particle
{-# INLINE previousVelocity #-}

-- ----- --
-- Swarm --
-- ----- --

-- | A swarm for the PSO algorithm.
data Swarm a = Swarm
    { -- | Particles in the swarm.
      parts :: NonEmpty (Particle a),
      -- | Global guide.
      gGuide :: PSOGuide a,
      -- | Current iteration.
      iteration :: Int
    }

instance Show (Swarm a) where
    show Swarm {..} = show iteration ++ " - " ++ show gGuide

-- | Implicit parameters used in iterations of the PSO algorithm.
type PSOContext a =
    ( Unbox a,
      ?eval :: Vector a -> Grade,
      ?values :: Vector a
    )

-- | Create a particle using the evaluation function, the vector of
-- original values and a specific position.
createParticle :: (HasCallStack, PSOContext a) => Vector Weight -> Particle a
createParticle weights =
    Particle
        { particleWeights =
            if length weights == length ?values
                then weights
                else error "size mismatch while creating particle",
          vel = zeros (length weights),
          pGuide =
            let sortedValues = ?values `sortLike` weights
             in PsoGuide {guideWeights = weights, guideGrade = ?eval sortedValues, sortedValues}
        }

-- | Creates a swarm from the evaluation function, the vector of
-- original values, the number of particles and a generator of positions.
createSwarm :: (HasCallStack, PSOContext a) => Int -> Random (Vector Weight) -> Random (Swarm a)
createSwarm n gen = do
    parts <- fromListN n <$> replicateM n (createParticle <$> gen)
    let gGuide = maximum1 $ fmap pGuide parts
    pure $ Swarm {parts, gGuide, iteration = 0}

-- ---------------- --
-- Iterating swarms --
-- ---------------- --

-- | Updates a single particle using an updater, the evaluation function,
-- the vector of original values and swarm information.
updateParticle :: PSOContext a => Updater a -> Swarm a -> Particle a -> Random (Particle a)
updateParticle newVel Swarm {..} part = do
    vel <- newVel
    let w' = particleWeights part .+ vel
    let sortedValues = ?values `sortLike` w'
    let newVal = ?eval sortedValues
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
updateSwarm :: PSOContext a => Updater a -> Swarm a -> Random (Swarm a)
updateSwarm up swarm = do
    newParts <- mapM (updateParticle up swarm) (parts swarm)
    let bestGuide = maximum1 $ fmap pGuide newParts
    let newGuide = max bestGuide (gGuide swarm)
    pure Swarm {parts = newParts, gGuide = newGuide, iteration = iteration swarm + 1}

-- | Create iterations of swarms, trying to maximize the objective funtion.
particleSwarmOptimization ::
    PSOContext a => Updater a -> Random (Vector Weight) -> Int -> Random (NonEmpty (Swarm a))
particleSwarmOptimization update weights size =
    createSwarm size weights >>= iterateR (updateSwarm update)
