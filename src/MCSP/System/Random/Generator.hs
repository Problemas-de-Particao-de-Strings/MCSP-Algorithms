-- | Random number generators.
module MCSP.System.Random.Generator (
    Generator (..),
    GenPCG,
) where

import Control.Monad (Monad (..))
import Control.Monad.ST (RealWorld, ST)
import Data.Bits (finiteBitSize, shift, (.&.))
import Data.Function (id)
import Data.Functor ((<$>))
import Data.Tuple (uncurry)
import Data.Tuple.Extra (both)
import Data.Vector.Unboxed (Vector)
import Data.Word (Word32, Word64)
import GHC.Enum (maxBound)
import GHC.Real (fromIntegral)
import System.IO (IO)

import System.Random.MWC qualified (Gen, initialize, uniformM, uniformRM)
import System.Random.PCG qualified (Gen, initialize)
import System.Random.PCG.Class qualified as PCG (uniform1, uniform1B, uniform2)
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

    -- | Generates a pair of uniformly distributed 32-bit values.
    uniform2 :: g -> m (Word32, Word32)

    -- | Generates a single 32-bit value up to a given limit with a uniform distribution.
    uniform1B :: Word32 -> g -> m Word32

-- | Pseudo-random number generation using Marsaglia's MWC256, (also known as MWC8222)
-- multiply-with-carry generator
--
-- The WMC generator is supposed to be really fast. See <https://hackage.haskell.org/package/mwc-random>.
type GenMWC s = System.Random.MWC.Gen s

-- | Break a 64 bit integer into two 32 bit integers.
splitW64 :: Word64 -> (Word32, Word32)
splitW64 val = toW32 `both` (val `shift` bitsW32, val)
  where
    maxW32 = fromIntegral (maxBound :: Word32)
    bitsW32 = finiteBitSize (maxBound :: Word32)
    toW32 x = fromIntegral (x .&. maxW32)
{-# INLINE splitW64 #-}

instance Generator (GenMWC RealWorld) IO where
    type Seed (GenMWC RealWorld) = Vector Word32
    initialize = System.Random.MWC.initialize
    {-# INLINE initialize #-}
    uniform1 = System.Random.MWC.uniformM
    {-# INLINE uniform1 #-}
    uniform2 gen = splitW64 <$> System.Random.MWC.uniformM gen
    {-# INLINE uniform2 #-}
    uniform1B bound = System.Random.MWC.uniformRM (0, bound)
    {-# INLINE uniform1B #-}

instance Generator (GenMWC s) (ST s) where
    type Seed (GenMWC s) = Vector Word32
    initialize = System.Random.MWC.initialize
    {-# INLINE initialize #-}
    uniform1 = System.Random.MWC.uniformM
    {-# INLINE uniform1 #-}
    uniform2 gen = splitW64 <$> System.Random.MWC.uniformM gen
    {-# INLINE uniform2 #-}
    uniform1B bound = System.Random.MWC.uniformRM (0, bound)
    {-# INLINE uniform1B #-}

-- | The standard generator for PCG (Permuted Congruential Generator) algorithm, implemented in C.
--
-- The PCG generators have good statstical quality, is fast and use very little memory. See
-- <https://www.pcg-random.org>.
type GenPCG s = System.Random.PCG.Gen s

instance Generator (GenPCG RealWorld) IO where
    type Seed (GenPCG RealWorld) = (Word64, Word64)
    initialize = uncurry System.Random.PCG.initialize
    {-# INLINE initialize #-}
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

instance Generator (GenPCG s) (ST s) where
    type Seed (GenPCG s) = (Word64, Word64)
    initialize = uncurry System.Random.PCG.initialize
    {-# INLINE initialize #-}
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

-- | A fast generator for PCG (Permuted Congruential Generator) algorithm, implemented in C.
--
-- The PCG generators have good statstical quality, is fast and use very little memory. See
-- <https://www.pcg-random.org>.
type GenPCGFast s = System.Random.PCG.Fast.Gen s

instance Generator (GenPCGFast RealWorld) IO where
    type Seed (GenPCGFast RealWorld) = Word64
    initialize = System.Random.PCG.Fast.initialize
    {-# INLINE initialize #-}
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

instance Generator (GenPCGFast s) (ST s) where
    type Seed (GenPCGFast s) = Word64
    initialize = System.Random.PCG.Fast.initialize
    {-# INLINE initialize #-}
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

-- | The standard generator for PCG (Permuted Congruential Generator) algorithm, implemented in
-- Haskell.
--
-- The PCG generators have good statstical quality, is fast and use very little memory. See
-- <https://www.pcg-random.org>.
type GenPCGPure s = System.Random.PCG.Pure.Gen s

instance Generator (GenPCGPure RealWorld) IO where
    type Seed (GenPCGPure RealWorld) = (Word64, Word64)
    initialize = uncurry System.Random.PCG.Pure.initialize
    {-# INLINE initialize #-}
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

instance Generator (GenPCGPure s) (ST s) where
    type Seed (GenPCGPure s) = (Word64, Word64)
    initialize = uncurry System.Random.PCG.Pure.initialize
    {-# INLINE initialize #-}
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

-- | A fast generator for PCG (Permuted Congruential Generator) algorithm, implemented in Haskell.
--
-- The PCG generators have good statstical quality, is fast and use very little memory. See
-- <https://www.pcg-random.org>.
type GenPCGFastPure s = System.Random.PCG.Fast.Pure.Gen s

instance Generator (GenPCGFastPure RealWorld) IO where
    type Seed (GenPCGFastPure RealWorld) = Word64
    initialize = System.Random.PCG.Fast.Pure.initialize
    {-# INLINE initialize #-}
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

instance Generator (GenPCGFastPure s) (ST s) where
    type Seed (GenPCGFastPure s) = Word64
    initialize = System.Random.PCG.Fast.Pure.initialize
    {-# INLINE initialize #-}
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

-- | A single stream generator for PCG (Permuted Congruential Generator) algorithm, implemented in
-- C.
--
-- The PCG generators have good statstical quality, is fast and use very little memory. See
-- <https://www.pcg-random.org>.
type GenPCGSingle s = System.Random.PCG.Single.Gen s

instance Generator (GenPCGSingle RealWorld) IO where
    type Seed (GenPCGSingle RealWorld) = Word64
    initialize = System.Random.PCG.Single.initialize
    {-# INLINE initialize #-}
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

instance Generator (GenPCGSingle s) (ST s) where
    type Seed (GenPCGSingle s) = Word64
    initialize = System.Random.PCG.Single.initialize
    {-# INLINE initialize #-}
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

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
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}
