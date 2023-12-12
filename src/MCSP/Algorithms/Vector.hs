-- | Operations on vector of `Default`.
module MCSP.Algorithms.Vector (
    Default,

    -- * Initialization
    zeros,
    replicate,

    -- * Element-wise Operations
    map,
    choose,
    (.+),
    (.-),
    (.*),
    (.*.),
    sum,

    -- * Sorting
    sort,
    sortOn,
    sortBy,
    argSort,
    sortLike,

    -- * Statistics
    normalized,
    standardized,

    -- * Monadic Operations
    sumM,
    replicateM,

    -- ** Random Operations
    uniformN,
    uniformSN,
    uniformRN,
    weighted,
    weightedN,
    choice,
) where

import Control.Applicative (pure)
import Control.Exception.Extra (errorWithoutStackTrace)
import Control.Monad (Monad, join, sequence)
import Data.Bool (Bool (..), bool, otherwise, (||))
import Data.Eq (Eq (..))
import Data.Foldable1 (foldl1')
import Data.Function (id, on, ($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List.NonEmpty (NonEmpty)
import Data.Ord (Ord (..), Ordering)
import Data.Vector.Algorithms.Merge qualified as Vector (sort, sortBy)
import Data.Vector.Generic qualified as Vector (maximumOn, sum)
import Data.Vector.Unboxed (
    Unbox,
    Vector,
    create,
    length,
    map,
    modify,
    null,
    replicate,
    replicateM,
    unsafeBackpermute,
    unsafeIndex,
    zipWith,
 )
import Data.Vector.Unboxed.Mutable (generate)
import GHC.Float (Double, Float, Floating, sqrt)
import GHC.Num (Num (..))
import GHC.Real (Fractional, fromIntegral, (/))
import Text.Printf (printf)

import MCSP.System.Random (Random, Variate, uniformR, weightedChoice)

-- | Default type used in specialized vector operations.
type Default = Float

-- | Checks that both input vectors have same length before applying an operation.
--
-- >>> withSameLength (zipWith (+)) [1, 2, 3] [4, 5, 6]
-- [5.0,7.0,9.0]
--
-- >>> withSameLength (zipWith (+)) [1, 2, 3] [4, 5]
-- *** Exception: length mismatch: 3 != 2
withSameLength :: (Unbox a, Unbox b) => (Vector a -> Vector b -> c) -> Vector a -> Vector b -> c
withSameLength f v1 v2 =
    if length v1 == length v2
        then f v1 v2
        else errorWithoutStackTrace $ printf "length mismatch: %d != %d" (length v1) (length v2)
{-# INLINE withSameLength #-}

-- -------------- --
-- Initialization --
-- -------------- --

-- | Create a vector of zeros given the length.
--
-- >>> zeros 3
-- [0.0,0.0,0.0]
--
-- >>> zeros 0
-- []
zeros :: (Unbox a, Num a) => Int -> Vector a
zeros s = replicate s 0
{-# SPECIALIZE zeros :: Int -> Vector Default #-}

-- ----------------------- --
-- Element-Wise Operations --
-- ----------------------- --

-- | Case analysis for the Bool type.
--
-- @`choose` x y p@ evaluates to @x@ in every position that is `False` in @p@, and evaluates to @y@
-- everywehere else. Works as a vector version of `bool`.
--
-- The name is taken from
-- [numpy.choose](https://numpy.org/doc/stable/reference/generated/numpy.choose.html).
--
-- >>> choose 10 (-5) [True, False, False, True]
-- [-5.0,10.0,10.0,-5.0]
choose :: Unbox a => a -> a -> Vector Bool -> Vector a
choose falsy truthy = map (bool falsy truthy)
{-# SPECIALIZE choose :: Default -> Default -> Vector Bool -> Vector Default #-}

infixl 7 .*, .*.
infixl 6 .+, .-

-- | Element-wise addition.
--
-- >>> [1, 2, 3] .+ [5, 6, 7]
-- [6.0,8.0,10.0]
(.+) :: (Unbox a, Num a) => Vector a -> Vector a -> Vector a
(.+) = zipWith (+)
{-# SPECIALIZE (.+) :: Vector Default -> Vector Default -> Vector Default #-}

-- | Element-wise subtraction.
--
-- >>> [1, 2, 3] .- [5, 6, 7]
-- [-4.0,-4.0,-4.0]
(.-) :: (Unbox a, Num a) => Vector a -> Vector a -> Vector a
(.-) = withSameLength $ zipWith (-)
{-# SPECIALIZE (.-) :: Vector Default -> Vector Default -> Vector Default #-}

-- | Element-wise multiplication.
--
-- >>> [1, 2, 3] .* [5, 6, 7]
-- [5.0,12.0,21.0]
(.*) :: (Unbox a, Num a) => Vector a -> Vector a -> Vector a
(.*) = withSameLength $ zipWith (*)
{-# SPECIALIZE (.*) :: Vector Default -> Vector Default -> Vector Default #-}

-- | Multiplication by a scalar.
--
-- >>> 3 .*. [5, 6, 7]
-- [15.0,18.0,21.0]
(.*.) :: (Unbox a, Num a) => a -> Vector a -> Vector a
factor .*. vector = map (factor *) vector
{-# SPECIALIZE (.*.) :: Default -> Vector Default -> Vector Default #-}

-- | Element-wise sum of all vectors.
--
-- >>> sum [[1, 2], [3, 4], [5, 6]]
-- [9.0,12.0]
sum :: (Unbox a, Num a) => NonEmpty (Vector a) -> Vector a
sum = foldl1' (.+)
{-# INLINE sum #-}
{-# SPECIALIZE INLINE sum :: NonEmpty (Vector Default) -> Vector Default #-}

-- ------- --
-- Sorting --
-- ------- --

-- | Sorts an array using the default comparison.
--
-- >>> sort [3, 1, 2]
-- [1.0,2.0,3.0]
sort :: (Unbox a, Ord a) => Vector a -> Vector a
sort = modify Vector.sort
{-# SPECIALIZE sort :: Vector Default -> Vector Default #-}

-- | Sorts a vector using a custom comparison.
--
-- >>> import Data.Ord (Ordering (..))
-- >>> sortBy (\x y -> if x * x < y * y then LT else GT) [-3, -1, 2]
-- [-1.0,2.0,-3.0]
sortBy :: Unbox a => (a -> a -> Ordering) -> Vector a -> Vector a
sortBy cmp = modify (Vector.sortBy cmp)

-- | Sorts a vector by comparing the results of a key function applied to each element.
--
-- >>> sortOn (\x -> x * x) [-3, -1, 2]
-- [-1.0,2.0,-3.0]
sortOn :: (Unbox a, Ord b) => (a -> b) -> Vector a -> Vector a
sortOn key = sortBy (compare `on` key)
{-# SPECIALIZE sortOn :: Unbox a => (a -> Default) -> Vector a -> Vector a #-}

-- | Returns the indices that would sort the vector.
--
-- >>> argSort [30, 10, 20]
-- [1,2,0]
--
-- >>> argSort $ argSort [30, 10, 20]
-- [2,0,1]
argSort :: (Unbox a, Ord a) => Vector a -> Vector Int
argSort vec = create $ do
    index <- generate (length vec) id
    -- SAFETY: index was created above, so it must be inbounds
    Vector.sortBy (compare `on` unsafeIndex vec) index
    pure index
{-# SPECIALIZE argSort :: Vector Default -> Vector Int #-}
{-# SPECIALIZE argSort :: Vector Int -> Vector Int #-}

-- | Sort a vector based on the values of another array.
--
-- >>> sortLike [30, 10, 20] [0.2, 0.9, 0.1]
-- [20.0,30.0,10.0]
sortLike :: (Unbox a, Unbox b, Ord b) => Vector a -> Vector b -> Vector a
sortLike = withSameLength $ \x y ->
    -- SAFETY: beckpermute is safe here because both vector have the same length
    unsafeBackpermute x (argSort y)
{-# SPECIALIZE sortLike :: Unbox a => Vector a -> Vector Default -> Vector a #-}
{-# SPECIALIZE sortLike :: Vector Default -> Vector Int -> Vector Default #-}

-- ---------- --
-- Statistics --
-- ---------- --

-- | Normalize a vector by its maximum absolute value.
--
-- >>> normalized [1, 2, 5, 10]
-- [0.1,0.2,0.5,1.0]
--
-- >>> normalized [1, 2, 5, -10]
-- [0.1,0.2,0.5,-1.0]
--
-- >>> normalized []
-- []
--
-- >>> normalized [0]
-- [0.0]
normalized :: (Unbox a, Fractional a, Ord a) => Vector a -> Vector a
normalized vector
    | null vector || absMax == 0 = vector
    | otherwise = map (/ absMax) vector
  where
    absMax = abs (Vector.maximumOn abs vector)
{-# SPECIALIZE normalized :: Vector Default -> Vector Default #-}

-- | Average value in a vector.
--
-- >>> mean [1, 2, 5, 10]
-- 4.5
--
-- >>> mean []
-- 0.0
mean :: (Unbox a, Fractional a) => Vector a -> a
mean vector
    | null vector = 0
    | otherwise = Vector.sum vector / fromIntegral (length vector)
{-# SPECIALIZE mean :: Vector Default -> Default #-}

-- | Variance of the values in a vector.
--
-- >>> variance [1, 2, 5, 10]
-- 12.25
--
-- >>> variance []
-- 0.0
variance :: (Unbox a, Fractional a) => Vector a -> a
variance vector = mean (dev .* dev)
  where
    u = mean vector
    dev = map (\x -> x - u) vector
{-# SPECIALIZE variance :: Vector Default -> Default #-}

-- | Standard Deviation of the values in a vector.
--
-- >>> stdev [1, 2, 5, 10]
-- 3.5
stdev :: (Unbox a, Floating a) => Vector a -> a
stdev = sqrt . variance
{-# SPECIALIZE stdev :: Vector Default -> Default #-}

-- | Adapt values for such that the mean becomes zero and standard deviation, one.
--
-- >>> standardized [1, 2, 5, 10]
-- [-1.0,-0.7142857142857143,0.14285714285714285,1.5714285714285714]
--
-- >>> standardized []
-- []
--
-- >>> standardized [1, 1]
-- [0.0,0.0]
standardized :: (Unbox a, Floating a, Eq a) => Vector a -> Vector a
standardized vector
    | null vector = vector
    | s == 0 = map (\x -> x - u) vector
    | otherwise = map (\x -> (x - u) / s) vector
  where
    u = mean vector
    s = stdev vector
{-# SPECIALIZE standardized :: Vector Default -> Vector Default #-}

-- ------------------ --
-- Monadic Operations --
-- ------------------ --

-- | Lifted version of `sum`.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> generateWith (2,3) $ sumM [uniformN 4, uniformSN 4]
-- [0.17218197108856648,0.21998774703644852,-0.16158831286684616,1.0345635554897776]
sumM :: (Unbox a, Num a, Monad m) => NonEmpty (m (Vector a)) -> m (Vector a)
sumM values = sum <$> sequence values
{-# INLINE sumM #-}
{-# SPECIALIZE INLINE sumM :: Monad m => NonEmpty (m (Vector Default)) -> m (Vector Default) #-}
{-# SPECIALIZE INLINE sumM :: NonEmpty (Random (Vector Default)) -> Random (Vector Default) #-}

-- | Generate multiple uniformly distributed values in the given range.
--
-- Replicated version of `uniformR`.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> generateWith (2,3) $ uniformRN 5 50 3
-- [5.889456173182222,21.532789204576318,30.885279630109455]
uniformRN :: (Unbox a, Variate a) => a -> a -> Int -> Random (Vector a)
uniformRN lo hi count = replicateM count (uniformR lo hi)
{-# SPECIALIZE uniformRN :: Default -> Default -> Int -> Random (Vector Default) #-}

-- | Generate multiple uniformly distributed values between @[0,1]@.
--
-- Replicated version of `uniform`.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> generateWith (2,3) $ uniformN 3
-- [1.9765692737382712e-2,0.3673953156572515,0.5752284362246546]
uniformN :: (Unbox a, Variate a, Num a) => Int -> Random (Vector a)
uniformN = uniformRN 0 1
{-# SPECIALIZE uniformN :: Int -> Random (Vector Default) #-}

-- | Generate multiple uniformly distributed values between @[-1,1]@.
--
-- Signed version of `uniformN`.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> generateWith (2,3) $ uniformSN 3
-- [-0.9604686145252346,-0.26520936868549705,0.15045687244930916]
uniformSN :: (Unbox a, Variate a, Num a) => Int -> Random (Vector a)
uniformSN = uniformRN (-1) 1
{-# SPECIALIZE uniformSN :: Int -> Random (Vector Default) #-}

-- | Multiplies the vector by a single random value between @[0,maxWeight]@.
--
-- Randomized version of `.*.`. See also `weightedN`.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> generateWith (2,3) $ weighted 10 [1, 2, 10]
-- [0.19765692737382712,0.39531385474765424,1.9765692737382712]
weighted :: (Unbox a, Variate a, Num a) => a -> Vector a -> Random (Vector a)
weighted maxWeight vec = do
    k <- uniformR 0 maxWeight
    pure (k .*. vec)
{-# SPECIALIZE weighted :: Default -> Vector Default -> Random (Vector Default) #-}

-- | Multiplies the vector by multiple random values between @[0,maxWeight]@.
--
-- Randomized version of `.*`. See also `weighted`.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> generateWith (2,3) $ weightedN 10 [1, 2, 10]
-- [0.19765692737382712,7.34790631314503,57.52284362246546]
weightedN :: (Unbox a, Variate a, Num a) => a -> Vector a -> Random (Vector a)
weightedN maxWeight vec = do
    k <- uniformRN 0 maxWeight (length vec)
    pure (k .* vec)
{-# SPECIALIZE weightedN :: Default -> Vector Default -> Random (Vector Default) #-}

-- | Choose randomly between multiple `Random` monad, with probablity proportional to its given
-- weight.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> generateWith (1,2) $ replicateM 10 $ choice [(1, pure 'a'), (2, pure 'b')]
-- "abbbaabbbb"
--
-- >>> generateWith (1,2) $ replicateM 10 $ choice [(1, uniformR (-1) 0), (2, uniformR 0 1)]
-- [-0.11816487538074749,0.5798377716767166,0.12231072251084052,0.754750234725723,0.5163453222019222,0.9673060222002038,-0.28900858364465354,0.609061325679456,-0.15187385001852494,0.4987697781636008]
choice :: NonEmpty (Double, Random a) -> Random a
choice options = join (weightedChoice options)
{-# INLINE choice #-}
