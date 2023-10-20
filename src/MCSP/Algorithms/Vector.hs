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
) where

import Data.Foldable1 (foldl1')
import Data.Int (Int)
import Data.List.NonEmpty (NonEmpty)
import Data.Vector.Unboxed (Vector, map, replicate, zipWith)
import GHC.Float (Double)
import GHC.Num ((*), (+), (-))

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
