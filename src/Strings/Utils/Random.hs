module Strings.Utils.Random (
    uniform,
    uniformN,
    shuffle,
    shuffleV,
    partitions,
    generate,
) where

import Prelude hiding (splitAt)

import Data.Vector.Generic (Vector, basicLength, fromListN, replicateM, splitAt, toList)
import System.Random.PCG.Class (Generator)
import System.Random.PCG.Fast (GenIO, Variate (uniformB, uniformR), withSystemRandom)
import System.Random.Shuffle qualified as S

-- | Generates a rangom `a` in the inclusive range `enumFromTo`.
uniformFromTo :: (Generator g m, Enum a) => a -> a -> g -> m a
uniformFromTo lo hi gen = toEnum <$> uniformR (fromEnum lo, fromEnum hi) gen

-- | Generates a rangom `a` in the inclusive range [`minBound`, `maxBound`].
uniform :: (Generator g m, Enum a, Bounded a) => g -> m a
uniform = uniformFromTo minBound maxBound
{-# INLINE uniform #-}

uniformN :: (Generator g m, Enum a, Bounded a, Vector v a) => Int -> g -> m (v a)
uniformN n gen = replicateM n $ uniform gen
{-# INLINEABLE uniformN #-}

-- | Generates indices for `S.shuffle`.
--
-- See [random-shuffle](https://hackage.haskell.org/package/random-shuffle-0.0.4/docs/System-Random-Shuffle.html#v:shuffle).
treeIndices :: Generator g m => Int -> g -> m [Int]
treeIndices n gen = mapM sample bounds
  where
    bounds = reverse [2 .. n]
    sample i = uniformB i gen

-- | Shuffles a list `xs` using `g` assuming `n == length xs`.
shuffle' :: Generator g m => Int -> [a] -> g -> m [a]
shuffle' n xs gen = do
    idx <- treeIndices n gen
    pure $ S.shuffle xs idx

-- | Shuffles a list using the RNG `g`.
shuffle :: Generator g m => [a] -> g -> m [a]
shuffle xs = shuffle' (length xs) xs
{-# INLINEABLE shuffle #-}

-- | Shuffles a vector using the RNG `g`.
shuffleV :: (Generator g m, Vector v a) => v a -> g -> m (v a)
shuffleV xs gen = do
    let len = basicLength xs
    xs' <- shuffle' len (toList xs) gen
    pure $ fromListN len xs'
{-# INLINEABLE shuffleV #-}

-- | Generate random partitions of a vector.
partitions :: (Generator g m, Vector v a) => v a -> g -> m [v a]
partitions xs gen
    | n == 0 = pure []
    | n == 1 = pure [xs]
    | otherwise = do
        idx <- uniformB n gen
        let (part, rest) = splitAt (idx + 1) xs
        parts <- partitions rest gen
        pure (part : parts)
  where
    n = basicLength xs
{-# INLINEABLE partitions #-}

-- | Use random generator in IO.
generate :: (GenIO -> IO a) -> IO a
generate = withSystemRandom
{-# INLINEABLE generate #-}
