-- | Randomized operations using a `Random` monad for "PCG" operations.
module MCSP.System.Random (
    -- * Random Monad
    Random,
    evalRandom,
    liftRandom,

    -- * Evaluation
    Seed,
    generate,
    generateWith,
    randomSeed,
    showSeed,
    readSeed,

    -- * Random Values
    PCG.Variate,
    uniform,
    uniformR,
    uniformB,
    uniformE,
    uniformRE,
    choose,
    shuffle,
    partitions,
) where

import Control.Applicative (pure)
import Control.Exception.Extra (errorWithoutStackTrace)
import Control.Monad (mapM)
import Data.Bits (complement)
import Data.Foldable (length)
import Data.Function (($))
import Data.Int (Int)
import Data.List ((++))
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Maybe (Maybe (..))
import Data.String qualified as Text (String)
import Data.Tuple (fst)
import Data.Vector.Generic (Vector, indexM, splitAt)
import Data.Vector.Generic qualified as Vector (length)
import Data.Word (Word64, bitReverse64)
import GHC.Enum (Bounded (..), Enum (..))
import GHC.Exts (IsList (..))
import GHC.Num ((+), (-))
import Numeric (readHex, showHex)
import System.IO (IO)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S, readS_to_P, skipSpaces)

import System.Random.PCG qualified as PCG
import System.Random.PCG.Class (sysRandom)
import System.Random.Shuffle qualified as Shuffle (shuffle)

import MCSP.System.Random.Monad (Random, evalRandom, liftRandom)

-- ---------- --
-- Evaluation --
-- ---------- --

-- | Values used to seed a random number generator.
type Seed = (Word64, Word64)

-- | Use given seed to generate value.
--
-- >>> generateWith (100,200) uniform :: Int
-- 3081816684322452293
generateWith :: Seed -> Random a -> a
generateWith (s1, s2) r = fst $ PCG.withFrozen seed (evalRandom r)
  where
    seed = PCG.initFrozen (complement s2) (bitReverse64 s1)
{-# INLINE generateWith #-}

-- | Use random seed to generate value in IO.
--
-- >>> generate uniform :: IO Int
-- 3295836545219376626  -- Could be any Int
generate :: Random a -> IO a
generate r = PCG.withSystemRandom (evalRandom r)
{-# INLINE generate #-}

-- | Generate a new random seed.
--
-- >>> randomSeed
-- (7193915830657461549,13617428908513093874) -- Could be any seed
randomSeed :: IO Seed
randomSeed = do
    l <- sysRandom
    r <- sysRandom
    pure (l, r)
{-# INLINE randomSeed #-}

-- | String representing the RNG seed in hex.
--
-- Inverse of `readSeed`.
--
-- >>> showSeed (0, 1)
-- "0 1"
showSeed :: Seed -> Text.String
showSeed (x, y) = showHex x " " ++ showHex y ""
{-# INLINE showSeed #-}

-- | Parser combinator for reading seeds.
readSeedP :: ReadP Seed
readSeedP = do
    l <- readS_to_P readHex
    skipSpaces
    r <- readS_to_P readHex
    pure (l, r)

-- | Read a seed in hexadecimal format.
--
-- Inverse of `showSeed`.
--
-- >>> readSeed "0 1"
-- (0,1)
-- >>> readSeed (showSeed (5, 10))
-- (5,10)
-- >>> readSeed "75f9fea579c63117 8a3a15e4c0a7029f"
-- (8501105758304612631,9960297598112170655)
readSeed :: Text.String -> Seed
readSeed str = case readP_to_S readSeedP str of
    [(seed, "")] -> seed
    [] -> errorWithoutStackTrace "readSeed: no parse"
    _ -> errorWithoutStackTrace "readSeed: ambiguous parse"

-- ------------- --
-- Random Values --
-- ------------- --

-- | /O(1)/ Generate a uniformly distributed random variate.
--
-- * Use entire range for integral types.
-- * Use (0,1] range for floating types.
--
-- >>> generateWith (1,2) uniform :: Double
-- 0.6502342391751404
uniform :: PCG.Variate a => Random a
uniform = liftRandom PCG.uniform
{-# INLINE uniform #-}

-- | /O(1)/ Generate a uniformly distributed random variate in the given range.
--
-- * Use inclusive range for integral types.
-- * Use (a,b] range for floating types.
--
-- >>> generateWith (1,2) $ uniformR 10 50 :: Int
-- 16
uniformR :: PCG.Variate a => a -> a -> Random a
uniformR lo hi = liftRandom $ PCG.uniformR (lo, hi)
{-# INLINE uniformR #-}

-- | /O(1)/ Generate a uniformly distributed random variate in the range [0,b).
--
-- * For integral types the bound must be less than the max bound of `Data.Word.Word32`
-- (4294967295). Behaviour is undefined for negative bounds.
--
-- >>> generateWith (1,2) $ uniformB 200 :: Int
-- 143
uniformB :: PCG.Variate a => a -> Random a
uniformB b = liftRandom $ PCG.uniformB b
{-# INLINE uniformB #-}

-- | /O(1)/ Generate a uniformly distributed random variate.
--
-- It should be equivalent to @`uniformR` (`minBound`, `maxBound`)@, but for non-`PCG.Variate`.
--
-- >>> data T = A | B | C | D deriving (Enum, Bounded, Show)
-- >>> generateWith (1,3) uniformE :: T
-- C
uniformE :: (Enum a, Bounded a) => Random a
uniformE = uniformRE minBound maxBound
{-# INLINE uniformE #-}

-- | /O(1)/ Generate a uniformly distributed random variate in the given inclusive range.
--
-- It should be equivalent to `uniformR`, but for non-`PCG.Variate`.
--
-- >>> generateWith (1,3) $ uniformRE 0 10 :: Int
-- 5
-- >>> generateWith (1,3) $ uniformRE 'a' 'z'
-- 'n'
uniformRE :: Enum a => a -> a -> Random a
uniformRE lo hi = do
    let loNum = fromEnum lo
    let hiNum = fromEnum hi
    value <- uniformR loNum hiNum
    pure (toEnum value)
{-# INLINE uniformRE #-}

-- | /O(1)/ Choose a single random value from a vector.
--
-- >>> import Data.Vector as V
-- >>> generateWith (1,2) $ choose (V.fromList ["hi", "hello", "ola"])
-- "hello"
choose :: Vector v a => v a -> Random a
choose v = do
    let n = Vector.length v
    idx <- uniformB n
    indexM v idx
{-# INLINE choose #-}

-- | /O(n)/ Generates indices for `S.shuffle`.
--
-- See [random-shuffle](https://hackage.haskell.org/package/random-shuffle-0.0.4/docs/System-Random-Shuffle.html#v:shuffle).
treeIndices :: Int -> Random [Int]
treeIndices n = mapM sample bounds
  where
    bounds = [n, n - 1 .. 2]
    sample (i :: Int) = uniformB i
{-# INLINE treeIndices #-}

-- | /O(?)/ Shuffles a non-empty list.
--
-- This simple implementation runs forever with empty lists.
shuffleNonEmpty :: IsList l => NonEmpty (Item l) -> Random l
shuffleNonEmpty (h :| rest) = do
    let xs = h : rest
    let n = length xs
    idx <- treeIndices n
    let ys = Shuffle.shuffle xs idx
    pure (fromListN n ys)
{-# INLINE shuffleNonEmpty #-}

-- | /O(?)/ Shuffles a list randomly.
--
-- >>> generateWith (1,2) $ shuffle [1..5] :: [Int]
-- [4,1,2,3,5]
shuffle :: IsList l => l -> Random l
shuffle values = case nonEmpty (toList values) of
    Just xs -> shuffleNonEmpty xs
    Nothing -> pure values
{-# INLINEABLE shuffle #-}

-- | /O(n)/ Generate random partitions of a vector.
--
-- >>> import Data.Vector as V
-- >>> generateWith (1,4) $ partitions (V.fromList [1..10])
-- [[1,2,3],[4,5,6,7,8],[9],[10]]
partitions :: Vector v a => v a -> Random [v a]
partitions xs = case Vector.length xs of
    0 -> pure []
    1 -> pure [xs]
    n -> do
        idx <- uniformB n
        let (part, rest) = splitAt (idx + 1) xs
        parts <- partitions rest
        pure (part : parts)
{-# INLINEABLE partitions #-}
