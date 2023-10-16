-- | Apply generators to the `Random` monad.
module MCSP.System.Random.Generate (
    -- * Default Generator
    generate,
    generateWith,

    -- * Faster Generator
    generateFast,
    generateFastWith,

    -- * Standard Seed
    Seed,
    randomSeed,
    showSeed,
    readSeed,
) where

import Control.Applicative (pure)
import Control.Exception.Extra (errorWithoutStackTrace)
import Control.Monad ((>>=))
import Control.Monad.ST (runST)
import Data.Bits (complement, xor)
import Data.Function ((.))
import Data.String qualified as Text (String)
import Data.Word (Word64, bitReverse64)
import Numeric (readHex, showHex)
import System.IO (IO)
import System.Random.PCG.Class (sysRandom)
import System.Random.PCG.Fast.Pure qualified (initialize)
import System.Random.PCG.Pure qualified (initialize)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S, readS_to_P, skipSpaces)
import Text.Show (ShowS, showChar)

import MCSP.Data.Pair (Pair, dupe, zipM)
import MCSP.System.Random.Monad (Random, evalRandom)

-- ----------------- --
-- Random Generation --
-- ----------------- --

-- | Mixes the input seed, resulting in another 128-bit seed base on the first two integers.
--
-- This is mostly done for fixed seeds in examples.
--
-- >>> mixSeed2 (1, 2)
-- (18446744073709551613,9223372036854775808)
mixSeed2 :: Seed -> Pair Word64
mixSeed2 (s1, s2) = (complement s2, bitReverse64 s1)
{-# INLINE mixSeed2 #-}

-- | Mixes the input seed, resulting in a 64-bit seed base on the first two integers.
--
-- This is useful for generators that only use a `Word64` seed.
mixSeed1 :: Seed -> Word64
mixSeed1 (mixSeed2 -> (s1, s2)) = complement (s1 `xor` s2)
{-# INLINE mixSeed1 #-}

-- | Use given seed to generate value using the Standard PCG generator.
--
-- >>> import MCSP.System.Random.Monad (liftRandom)
-- >>> import Data.Function (id)
-- >>> import System.Random.PCG.Class (uniform1)
-- >>> generateWith (100,200) (liftRandom (uniform1 id))
-- 717541362
generateWith :: Seed -> Random a -> a
generateWith (mixSeed2 -> (s1, s2)) r =
    runST (System.Random.PCG.Pure.initialize s1 s2 >>= evalRandom r)
{-# INLINE generateWith #-}

-- | Use given seed to generate value using the Fast PCG generator.
--
-- >>> import MCSP.System.Random.Monad (liftRandom)
-- >>> import Data.Function (id)
-- >>> import System.Random.PCG.Class (uniform1)
-- >>> generateFastWith (100,200) (liftRandom (uniform1 id))
-- 77824
generateFastWith :: Seed -> Random a -> a
generateFastWith (mixSeed1 -> seed) r =
    runST (System.Random.PCG.Fast.Pure.initialize seed >>= evalRandom r)
{-# INLINE generateFastWith #-}

-- | Generate value using a random seed with the Standard PCG Generator.
--
-- >>> import MCSP.System.Random.Monad (liftRandom)
-- >>> import Data.Function (id)
-- >>> import System.Random.PCG.Class (uniform1)
-- >>> generate (liftRandom (uniform1 id))
-- ...
generate :: Random a -> IO a
generate r = do
    seed <- randomSeed
    pure (generateWith seed r)
{-# INLINE generate #-}

-- | Generate value using a random seed with the Fast PCG Generator.
--
-- >>> import MCSP.System.Random.Monad (liftRandom)
-- >>> import Data.Function (id)
-- >>> import System.Random.PCG.Class (uniform1)
-- >>> generateFast (liftRandom (uniform1 id))
-- ...
generateFast :: Random a -> IO a
generateFast r = do
    seed <- randomSeed
    pure (generateFastWith seed r)
{-# INLINE generateFast #-}

-- --------------- --
-- Seed Operations --
-- --------------- --

-- | Values used to seed a random number generator.
type Seed = Pair Word64

-- | Generate a new random seed.
--
-- >>> randomSeed
-- ...
randomSeed :: IO Seed
randomSeed = zipM (dupe sysRandom)
{-# INLINE randomSeed #-}

-- | `ShowS` implementation for hexadecimal seed.
showSeedS :: Seed -> ShowS
showSeedS (x, y) = showHex x . showChar ' ' . showHex y
{-# INLINE showSeedS #-}

-- | String representing the RNG seed in hexadecimal.
--
-- Inverse of `readSeed`.
--
-- >>> showSeed (0, 1)
-- "0 1"
showSeed :: Seed -> Text.String
showSeed s = showSeedS s ""
{-# INLINE showSeed #-}

-- | Parser combinator for reading seeds.
readSeedP :: ReadP Seed
readSeedP = do
    l <- readS_to_P readHex
    skipSpaces
    r <- readS_to_P readHex
    pure (l, r)
{-# INLINE readSeedP #-}

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
