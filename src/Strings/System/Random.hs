module Strings.System.Random (
    staticSeed,
    Random,
    generateWithSeed,
    generateStatic,
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
import Data.Bits (Bits (complement))
import Data.Vector.Generic (Vector, indexM, splitAt)
import Data.Vector.Generic qualified as G (length)
import Data.Word (Word64, bitReverse64)
import GHC.Exts (IsList (..))
import System.Random.PCG qualified as R
import System.Random.PCG.Class (Generator)
import System.Random.Shuffle qualified as S

import Strings.System.Random.Static (mkStaticSeed)

-- | A random seed regenerated for every compilation.
mkStaticSeed "staticSeed"

-- | A monad capable of producing random values of `a`.
type Random a = forall g m. Generator g m => RandT g m a

-- | Use given seed to generate value.
--
-- >>> generateWithSeed 100 200 uniform :: Int
-- -8529848114442733943
generateWithSeed :: Word64 -> Word64 -> Random a -> a
generateWithSeed s1 s2 r = fst $ R.withFrozen seed (evalRandT r)
  where
    seed = R.initFrozen (complement s1) (bitReverse64 s2)
{-# INLINE generateWithSeed #-}

-- | Use `staticSeed` to generate a random value.
--
-- >>> generateStatic uniform :: Double
-- 0.9943416107068436  -- Changes in every compilation
generateStatic :: Random a -> a
generateStatic = let (s1, s2) = staticSeed in generateWithSeed s1 s2
{-# INLINE generateStatic #-}

-- | Use random seed to generate value in IO.
--
-- >>> generate uniform :: IO Int
-- 4619012372051643558  -- Could be any Int
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
-- * Use (0,1] range for floating types.
--
-- >>> generateWithSeed 1 2 uniform :: Double
-- 0.7673711397211634
uniform :: R.Variate a => Random a
uniform = liftRandom R.uniform
{-# INLINE uniform #-}

-- | Generate a uniformly distributed random variate in the given range.
--
-- * Use inclusive range for integral types.
-- * Use (a,b] range for floating types.
--
-- >>> generateWithSeed 1 2 $ uniformR 0 10 :: Int
-- 2
uniformR :: R.Variate a => a -> a -> Random a
uniformR lo hi = liftRandom $ R.uniformR (lo, hi)
{-# INLINE uniformR #-}

-- | Generate a uniformly distributed random variate in the range [0,b).
--
-- * For integral types the bound must be less than the max bound of 'Word32' (4294967295). Behaviour is undefined for
--   negative bounds.
--
-- >>> generateWithSeed 1 2 $ uniformB 200 :: Int
-- 100
uniformB :: R.Variate a => a -> Random a
uniformB b = liftRandom $ R.uniformB b
{-# INLINE uniformB #-}

-- | Generate a uniformly distributed random variate.
--
-- It should be equivalent to `uniformR (minBound, maxBound)`, but for non-`Variate`.
--
-- >>> data T = A | B | C | D deriving (Enum, Bounded, Show)
-- >>> generateWithSeed 1 2 uniformE :: T
-- B
uniformE :: (Enum a, Bounded a) => Random a
uniformE = uniformRE minBound maxBound
{-# INLINE uniformE #-}

-- | Generate a uniformly distributed random variate in the given inclusive range.
--
-- It should be equivalent to `uniformR`, but for non-`Variate`.
--
-- >>> generateWithSeed 1 2 $ uniformRE 0 10 :: Int
-- 2
-- >>> generateWithSeed 1 2 $ uniformRE 'a' 'z'
-- 'g'
uniformRE :: Enum a => a -> a -> Random a
uniformRE lo hi = do
    let loNum = fromEnum lo
    let hiNum = fromEnum hi
    value <- uniformR loNum hiNum
    pure $ toEnum value
{-# INLINE uniformRE #-}

-- | Choose a single random value from a vector.
--
-- >>> import Data.Vector as V
-- >>> generateWithSeed 1 2 $ choose (V.fromList ["hi", "hello", "ola"])
-- "hello"
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
--
-- >>> generateWithSeed 1 2 $ shuffle [1..5] :: [Int]
-- [1,2,5,4,3]
shuffle :: IsList l => l -> Random l
shuffle list = do
    let xs = toList list
    let n = length xs
    idx <- treeIndices n
    let ys = S.shuffle xs idx
    pure $ fromListN n ys
{-# INLINEABLE shuffle #-}

-- | Generate random partitions of a vector.
--
-- >>> import Data.Vector as V
-- >>> generateWithSeed 1 2 $ partitions (V.fromList [1..10])
-- [[1],[2,3,4,5],[6,7,8],[9,10]]
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
