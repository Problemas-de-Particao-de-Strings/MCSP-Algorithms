module Strings.System.Random (
    Random,
    generateWithSeed,
    generate,
    R.Variate,
    uniform,
    uniformR,
    uniformB,
    uniformE,
    choose,
    shuffle,
    partitions,
) where

import Prelude hiding (splitAt)

import Control.Monad.Random.Strict (RandT, evalRandT, liftRandT)
import Data.Vector.Generic (Vector, indexM, splitAt)
import Data.Vector.Generic qualified as G (length)
import Data.Word (Word64)
import GHC.Exts (IsList (..))
import System.Random.PCG.Class (Generator)
import System.Random.PCG.Fast qualified as R
import System.Random.Shuffle qualified as S

-- | A monad capable of producing random values of `a`.
type Random a = forall g m. Generator g m => RandT g m a

-- | Use given seed to generate value.
--
-- @
--   generateWithSeed 100 uniform :: Int  -- 2236967910
-- @
generateWithSeed :: Word64 -> Random a -> a
generateWithSeed seed r = fst $ R.withFrozen (R.initFrozen seed) (evalRandT r)
{-# INLINE generateWithSeed #-}

-- | Use random seed to generate value in IO.
--
-- @
--   generate uniform :: IO Int
-- @
generate :: Random a -> IO a
generate r = R.withSystemRandom (evalRandT r)
{-# INLINE generate #-}

-- | Turn a standard RNG function into a `Random` monad.
liftRandom :: (forall g m. Generator g m => g -> m a) -> Random a
liftRandom gen = liftRandT genAndReturn
  where
    genAndReturn rng = do
        value <- gen rng
        pure (value, rng)
{-# INLINE liftRandom #-}

-- | Generate a uniformly distributed random variate.
--
-- * Use entire range for integral types.
--
-- * Use (0,1] range for floating types.
uniform :: R.Variate a => Random a
uniform = liftRandom R.uniform
{-# INLINE uniform #-}

-- | Generate a uniformly distributed random vairate in the given range.
--
-- * Use inclusive range for integral types.
--
-- * Use (a,b] range for floating types.
uniformR :: R.Variate a => (a, a) -> Random a
uniformR r = liftRandom $ R.uniformR r
{-# INLINE uniformR #-}

-- \| Generate a uniformly distributed random vairate in the range
--   [0,b). For integral types the bound must be less than the max bound
--   of 'Word32' (4294967295). Behaviour is undefined for negative
--   bounds.
uniformB :: R.Variate a => a -> Random a
uniformB b = liftRandom $ R.uniformB b
{-# INLINE uniformB #-}

-- | Generates a random `a` in the inclusive range `enumFromTo`.
uniformFromTo :: Enum a => a -> a -> Random a
uniformFromTo lo hi = do
    let bounds = (fromEnum lo, fromEnum hi)
    value <- uniformR bounds
    pure $ toEnum value
{-# INLINE uniformFromTo #-}

-- | Generates a random `Enum a` in the inclusive range [`minBound`, `maxBound`].
--
-- It is equivalent to `uniformR (minBound, maxBound)`, but without requiring `Variate a`.
uniformE :: (Enum a, Bounded a) => Random a
uniformE = uniformFromTo minBound maxBound
{-# INLINE uniformE #-}

-- | Choose a single value from `v a` randomly.
choose :: Vector v a => v a -> Random a
choose v = do
    let n = G.length v
    idx <- uniformB n
    indexM v idx
{-# INLINE choose #-}

-- | Generates indices for `S.shuffle`.
--
-- See [random-shuffle](https://hackage.haskell.org/package/random-shuffle-0.0.4/docs/System-Random-Shuffle.html#v:shuffle).
treeIndices :: Int -> Random [Int]
treeIndices n = mapM sample bounds
  where
    bounds = reverse [2 .. n]
    sample (i :: Int) = uniformB i
{-# INLINEABLE treeIndices #-}

-- | Shuffles a list randomly.
shuffle :: IsList l => l -> Random l
shuffle list = do
    let xs = toList list
    let n = length xs
    idx <- treeIndices n
    let ys = S.shuffle xs idx
    pure $ fromListN n ys
{-# INLINEABLE shuffle #-}

-- | Generate random partitions of a vector.
partitions :: Vector v a => v a -> Random [v a]
partitions xs
    | n == 0 = pure []
    | n == 1 = pure [xs]
    | otherwise = do
        idx <- uniformB n
        let (part, rest) = splitAt (idx + 1) xs
        parts <- partitions rest
        pure (part : parts)
  where
    n = G.length xs
{-# INLINEABLE partitions #-}
