-- | Random number generators.
module MCSP.System.Random.Generator (
    Generator (..),
    GenPCG,
) where

import Control.Applicative (pure)
import Control.Monad (Monad (..), fail)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits (FiniteBits, finiteBitSize, shift, (.|.))
import Data.Bool (otherwise)
import Data.Eq (Eq (..))
import Data.Function (id)
import Data.Int (Int)
import Data.Maybe (Maybe (..), maybe)
import Data.Ord (Ord (..))
import Data.Store (decodeIO)
import Data.Tuple (uncurry)
import Data.Type.Equality (type (~))
import Data.Vector.Unboxed (Vector)
import Data.Word (Word32, Word64, Word8)
import GHC.Base (asTypeOf, ($!))
import GHC.Enum (Bounded, maxBound)
import GHC.Num ((*), (+))
import GHC.Real (Integral, div, fromIntegral)
import System.IO (IO)
import Text.Show (Show)

import System.Entropy (getEntropy, getHardwareEntropy)
import System.Random.MWC qualified (Gen, initialize, uniformM, uniformRM)
import System.Random.PCG qualified (Gen, initialize)
import System.Random.PCG.Class qualified as PCG (uniform1, uniform1B, uniform2, uniformRW64)
import System.Random.PCG.Fast qualified (Gen, initialize)
import System.Random.PCG.Fast.Pure qualified (Gen, initialize)
import System.Random.PCG.Pure qualified (Gen, initialize)
import System.Random.PCG.Single qualified (Gen, initialize)
import System.Random.PCG.Unique qualified (Gen, initialize)

-- | A random number generator.
--
-- The numbers generated here may be from computer entropy or may be generated with a pseudo-random
-- RNG seeded in some way.
class Monad m => Generator g m where
    -- | The type used to seed the generator.
    type Seed g

    -- Construct the generator from a seed.
    initialize :: Seed g -> m g

    -- | Generates a single uniformly distributed 32-bit value.
    uniform1 :: g -> m Word32

    -- | Generates a single uniformly distributed 64-bit value.
    uniform2 :: g -> m Word64

    -- | Generates a single 32-bit value up to a given limit with a uniform distribution.
    uniform1B :: Word32 -> g -> m Word32

    -- | Generates a single 64-bit value up to a given limit with a uniform distribution.
    uniform2B :: Word64 -> g -> m Word64

-- | Pseudo-random number generation using Marsaglia's MWC256, (also known as MWC8222)
-- multiply-with-carry generator
--
-- The WMC generator is supposed to be really fast. See <https://hackage.haskell.org/package/mwc-random>.
type GenMWC s = System.Random.MWC.Gen s

wordsTo64Bit :: Word32 -> Word32 -> Word64
wordsTo64Bit x y = fromIntegral x `shift` 32 .|. fromIntegral y
{-# INLINE wordsTo64Bit #-}

instance (PrimMonad m, s ~ PrimState m) => Generator (GenMWC s) m where
    type Seed (GenMWC s) = Vector Word32
    initialize = System.Random.MWC.initialize
    {-# INLINE initialize #-}
    uniform1 = System.Random.MWC.uniformM
    {-# INLINE uniform1 #-}
    uniform2 = System.Random.MWC.uniformM
    {-# INLINE uniform2 #-}
    uniform1B bound = System.Random.MWC.uniformRM (0, bound)
    {-# INLINE uniform1B #-}
    uniform2B bound = System.Random.MWC.uniformRM (0, bound)
    {-# INLINE uniform2B #-}

-- | The standard generator for PCG (Permuted Congruential Generator) algorithm, implemented in C.
--
-- The PCG generators have good statstical quality, is fast and use very little memory. See
-- <https://www.pcg-random.org>.
type GenPCG s = System.Random.PCG.Gen s

instance (PrimMonad m, s ~ PrimState m) => Generator (GenPCG s) m where
    type Seed (GenPCG s) = (Word64, Word64)
    initialize = uncurry System.Random.PCG.initialize
    {-# INLINE initialize #-}
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 wordsTo64Bit
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}
    uniform2B bound = PCG.uniformRW64 (0, bound)
    {-# INLINE uniform2B #-}

-- | A fast generator for PCG (Permuted Congruential Generator) algorithm, implemented in C.
--
-- The PCG generators have good statstical quality, is fast and use very little memory. See
-- <https://www.pcg-random.org>.
type GenPCGFast s = System.Random.PCG.Fast.Gen s

instance (PrimMonad m, s ~ PrimState m) => Generator (GenPCGFast s) m where
    type Seed (GenPCGFast s) = Word64
    initialize = System.Random.PCG.Fast.initialize
    {-# INLINE initialize #-}
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 wordsTo64Bit
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}
    uniform2B bound = PCG.uniformRW64 (0, bound)
    {-# INLINE uniform2B #-}

-- | The standard generator for PCG (Permuted Congruential Generator) algorithm, implemented in
-- Haskell.
--
-- The PCG generators have good statstical quality, is fast and use very little memory. See
-- <https://www.pcg-random.org>.
type GenPCGPure s = System.Random.PCG.Pure.Gen s

instance (PrimMonad m, s ~ PrimState m) => Generator (GenPCGPure s) m where
    type Seed (GenPCGPure s) = (Word64, Word64)
    initialize = uncurry System.Random.PCG.Pure.initialize
    {-# INLINE initialize #-}
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 wordsTo64Bit
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}
    uniform2B bound = PCG.uniformRW64 (0, bound)
    {-# INLINE uniform2B #-}

-- | A fast generator for PCG (Permuted Congruential Generator) algorithm, implemented in Haskell.
--
-- The PCG generators have good statstical quality, is fast and use very little memory. See
-- <https://www.pcg-random.org>.
type GenPCGFastPure s = System.Random.PCG.Fast.Pure.Gen s

instance (PrimMonad m, s ~ PrimState m) => Generator (GenPCGFastPure s) m where
    type Seed (GenPCGFastPure s) = Word64
    initialize = System.Random.PCG.Fast.Pure.initialize
    {-# INLINE initialize #-}
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 wordsTo64Bit
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}
    uniform2B bound = PCG.uniformRW64 (0, bound)
    {-# INLINE uniform2B #-}

-- | A single stream generator for PCG (Permuted Congruential Generator) algorithm, implemented in
-- C.
--
-- The PCG generators have good statstical quality, is fast and use very little memory. See
-- <https://www.pcg-random.org>.
type GenPCGSingle s = System.Random.PCG.Single.Gen s

instance (PrimMonad m, s ~ PrimState m) => Generator (GenPCGSingle s) m where
    type Seed (GenPCGSingle s) = Word64
    initialize = System.Random.PCG.Single.initialize
    {-# INLINE initialize #-}
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 wordsTo64Bit
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}
    uniform2B bound = PCG.uniformRW64 (0, bound)
    {-# INLINE uniform2B #-}

-- | A unique generator for PCG (Permuted Congruential Generator) algorithm, implemented in C.
--
-- Guarantees the sequence to be unique by using the pointer address to select the output sequence.
--
-- The PCG generators have good statstical quality, is fast and use very little memory. See
-- <https://www.pcg-random.org>.
type GenPCGUnique = System.Random.PCG.Unique.Gen

instance Generator GenPCGUnique IO where
    type Seed GenPCGUnique = Word64
    initialize = System.Random.PCG.Unique.initialize
    {-# INLINE initialize #-}
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 wordsTo64Bit
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}
    uniform2B bound = PCG.uniformRW64 (0, bound)
    {-# INLINE uniform2B #-}

uniformBounded :: (Integral a, Bounded a, Monad m) => a -> m a -> m a
uniformBounded hi genUniform
    | hi == maxBound = genUniform
    | otherwise = loop
  where
    n = hi + 1
    buckets = maxBound `div` n
    maxN = buckets * n
    loop = do
        x <- genUniform
        if x < maxN
            then pure $! x `div` buckets
            else loop
{-# INLINE uniformBounded #-}

-- | Gets the size in bytes for type @a@.
--
-- >>> sizeOf (0 :: Word32)
-- 4
sizeOf :: (FiniteBits a, Bounded a) => a -> Int
sizeOf val = finiteBitSize (asTypeOf maxBound val) `div` finiteBitSize (maxBound :: Word8)
{-# INLINE sizeOf #-}

data GenEntropy = GenEntropy
    deriving stock (Eq, Ord, Show)

instance Generator GenEntropy IO where
    type Seed GenEntropy = GenEntropy
    initialize = pure
    {-# INLINE initialize #-}
    uniform1 GenEntropy = getEntropy (sizeOf (0 :: Word32)) >>= decodeIO
    {-# INLINE uniform1 #-}
    uniform2 GenEntropy = getEntropy (sizeOf (0 :: Word64)) >>= decodeIO
    {-# INLINE uniform2 #-}
    uniform1B hi gen = uniformBounded hi (uniform1 gen)
    {-# INLINE uniform1B #-}
    uniform2B hi gen = uniformBounded hi (uniform2 gen)
    {-# INLINE uniform2B #-}

data GenHWEntropy = GenHWEntropy
    deriving stock (Eq, Ord, Show)

unwrapIO :: Maybe a -> IO a
unwrapIO = maybe (fail "GenHWEntropy: no hardware entropy available") pure

instance Generator GenHWEntropy IO where
    type Seed GenHWEntropy = GenHWEntropy
    initialize = pure
    {-# INLINE initialize #-}
    uniform1 GenHWEntropy = getHardwareEntropy (sizeOf (0 :: Word32)) >>= unwrapIO >>= decodeIO
    {-# INLINE uniform1 #-}
    uniform2 GenHWEntropy = getHardwareEntropy (sizeOf (0 :: Word64)) >>= unwrapIO >>= decodeIO
    {-# INLINE uniform2 #-}
    uniform1B hi gen = uniformBounded hi (uniform1 gen)
    {-# INLINE uniform1B #-}
    uniform2B hi gen = uniformBounded hi (uniform2 gen)
    {-# INLINE uniform2B #-}
