-- | Random number generators.
module MCSP.System.Random.Generator (
    Generator (..),
    GenPCG,
) where

import Control.Monad (Monad (..))
import Control.Monad.ST (RealWorld, ST)
import Data.Function (id)
import Data.Word (Word32)
import System.IO (IO)

import System.Random.PCG qualified (Gen)
import System.Random.PCG.Class qualified as PCG (uniform1, uniform1B, uniform2)
import System.Random.PCG.Fast qualified (Gen)
import System.Random.PCG.Fast.Pure qualified (Gen)
import System.Random.PCG.Pure qualified (Gen)
import System.Random.PCG.Single qualified (Gen)
import System.Random.PCG.Unique qualified (Gen)

-- | A random number generator.
--
-- The numbers generated here may be from computer entropy or may be generated with a pseudo-random
-- RNG seeded in some way.
class Monad m => Generator g m where
    -- | Generates a single uniformly distributed 32-bit value.
    uniform1 :: g -> m Word32

    -- | Generates a pair of uniformly distributed 32-bit values.
    uniform2 :: g -> m (Word32, Word32)

    -- | Generates a single 32-bit value up to a given limit with a uniform distribution.
    uniform1B :: Word32 -> g -> m Word32

-- | The standard generator for PCG (Permuted Congruential Generator) algorithm, implemented in C.
--
-- The PCG generators have good statstical quality, is fast and use very little memory. See
-- <https://www.pcg-random.org/>.
type GenPCG s = System.Random.PCG.Gen s

instance Generator (GenPCG RealWorld) IO where
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

instance Generator (GenPCG s) (ST s) where
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

type GenPCGFast s = System.Random.PCG.Fast.Gen s

instance Generator (GenPCGFast RealWorld) IO where
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

instance Generator (GenPCGFast s) (ST s) where
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

type GenPCGPure s = System.Random.PCG.Pure.Gen s

instance Generator (GenPCGPure RealWorld) IO where
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

instance Generator (GenPCGPure s) (ST s) where
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

type GenPCGFastPure s = System.Random.PCG.Fast.Pure.Gen s

instance Generator (GenPCGFastPure RealWorld) IO where
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

instance Generator (GenPCGFastPure s) (ST s) where
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

type GenPCGSingle s = System.Random.PCG.Single.Gen s

instance Generator (GenPCGSingle RealWorld) IO where
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

instance Generator (GenPCGSingle s) (ST s) where
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}

type GenPCGUnique = System.Random.PCG.Unique.Gen

instance Generator GenPCGUnique IO where
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 (,)
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}
