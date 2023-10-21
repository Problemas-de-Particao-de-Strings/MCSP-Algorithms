-- | Operations on vector of `Double`.
module MCSP.Algorithms.Vector (
    -- * Initialization
    zeros,
    replicate,

    -- * Element-wise Operations
    (.+),
    (.-),
    (.*),
    (.*.),
    sum,

    -- * Monadic Operations
    sumM,
    replicateM,

    -- ** Random Operations
    uniformN,
    uniformSN,
    uniformRN,
    weighted,
    weightedN,
) where

import Control.Applicative (pure)
import Control.Monad (Monad, sequence)
import Data.Foldable1 (foldl1')
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List.NonEmpty (NonEmpty)
import Data.Vector.Generic qualified as Vector (replicateM)
import Data.Vector.Unboxed (Vector, length, map, replicate, zipWith)
import GHC.Float (Double)
import GHC.Num ((*), (+), (-))

import MCSP.System.Random (Random, uniformR)

-- -------------- --
-- Initialization --
-- -------------- --

-- | Create a vector of zeros given the length.
--
-- >>> zeros 3
-- [0.0,0.0,0.0]
-- >>> zeros 0
-- []
zeros :: Int -> Vector Double
zeros s = replicate s 0

-- ------------ --
-- Element-Wise --
-- ------------ --

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

-- | Multiplication by a scalar.
--
-- >>> 3 .*. [5, 6, 7]
-- [15.0,18.0,21.0]
(.*.) :: Double -> Vector Double -> Vector Double
factor .*. vector = map (factor *) vector

-- | Element-wise sum of all vectors.
--
-- >>> sum [[1, 2], [3, 4], [5, 6]]
-- [9.0,12.0]
sum :: NonEmpty (Vector Double) -> Vector Double
sum = foldl1' (.+)
{-# INLINE sum #-}

-- ------------------ --
-- Monadic Operations --
-- ------------------ --

-- | Lifted version of `sum`.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> generateWith (2,3) (sumM [uniformN 4, uniformSN 4])
-- [0.17218197108856648,0.21998774703644852,-0.16158831286684616,1.0345635554897776]
sumM :: Monad m => NonEmpty (m (Vector Double)) -> m (Vector Double)
sumM values = sum <$> sequence values
{-# INLINE sumM #-}

-- | Execute the monadic action the given number of times and store the results in a vector.
--
-- Specialized version of `Vector.replicateM`.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> generateWith (2,3) (replicateM 3 (uniformR 10 100))
-- [11.778912346364445,43.065578409152636,61.77055926021891]
replicateM :: Monad m => Int -> m Double -> m (Vector Double)
replicateM = Vector.replicateM

-- | Generate multiple uniformly distributed values in the given range.
--
-- Replicated version of `uniformR`.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> generateWith (2,3) (uniformRN 5 50 3)
-- [5.889456173182222,21.532789204576318,30.885279630109455]
uniformRN :: Double -> Double -> Int -> Random (Vector Double)
uniformRN lo hi count = replicateM count (uniformR lo hi)

-- | Generate multiple uniformly distributed values between @[0,1]@.
--
-- Replicated version of `uniform`.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> generateWith (2,3) (uniformN 3)
-- [1.9765692737382712e-2,0.3673953156572515,0.5752284362246546]
uniformN :: Int -> Random (Vector Double)
uniformN = uniformRN 0 1

-- | Generate multiple uniformly distributed values between @[-1,1]@.
--
-- Signed version of `uniformN`.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> generateWith (2,3) (uniformSN 3)
-- [-0.9604686145252346,-0.26520936868549705,0.15045687244930916]
uniformSN :: Int -> Random (Vector Double)
uniformSN = uniformRN (-1) 1

-- | Multiplies the vector by a single random value between @[0,maxWeight]@.
--
-- Randomized version of `.*.`. See also `weightedN`.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> generateWith (2,3) (weighted 10 [1, 2, 10])
-- [0.19765692737382712,0.39531385474765424,1.9765692737382712]
weighted :: Double -> Vector Double -> Random (Vector Double)
weighted maxWeight vec = do
    k <- uniformR 0 maxWeight
    pure (k .*. vec)

-- | Multiplies the vector by multiple random values between @[0,maxWeight]@.
--
-- Randomized version of `.*`. See also `weighted`.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> generateWith (2,3) (weightedN 10 [1, 2, 10])
-- [0.19765692737382712,7.34790631314503,57.52284362246546]
weightedN :: Double -> Vector Double -> Random (Vector Double)
weightedN maxWeight vec = do
    k <- uniformRN 0 maxWeight (length vec)
    pure (k .* vec)
