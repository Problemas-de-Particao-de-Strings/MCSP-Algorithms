-- | Random number generators.
module MCSP.System.Random.Generator (
    Generator (..),
    SeedableGenerator (..),
    RandomGenerator (..),
    MWC (..),
    PCG (..),
    PCGFast (..),
    PCGPure (..),
    PCGFastPure (..),
    PCGSingle (..),
    PCGUnique (..),
) where

import Control.Applicative (pure)
import Control.Monad (Monad (..), fail, (>=>))
import Control.Monad.Extra (concatForM)
import Control.Monad.ST (ST, runST)
import Data.Bits (FiniteBits, finiteBitSize, shift, (.|.))
import Data.Bool (otherwise)
import Data.Eq (Eq (..))
import Data.Function (id, ($))
import Data.Int (Int)
import Data.Maybe (Maybe (..), maybe)
import Data.Ord (Ord (..))
import Data.Store (Size (ConstSize), Store, decodeIO, size)
import Data.Tuple (uncurry)
import Data.Vector.Unboxed (Vector)
import Data.Word (Word32, Word64, Word8)
import GHC.Base (asTypeOf, ($!))
import GHC.Enum (Bounded, maxBound)
import GHC.Generics (Generic)
import GHC.Num ((*), (+))
import GHC.Real (Integral, div, fromIntegral)
import Language.Haskell.TH (conT)
import System.IO (IO)
import Text.Read (Read)
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

class Generator (State g m) m => SeedableGenerator g m where
    -- | The actual generator that uses the seed to generate values.
    type State g m

    -- | The type used to seed the generator.
    type Seed g

    -- Construct the generator from a seed.
    initialize :: g -> Seed g -> m (State g m)

-- | A random number generator.
--
-- The numbers generated here may be from computer entropy or may be generated with a pseudo-random
-- RNG seeded in some way.
class Monad m => Generator g m where
    -- | Generates a single uniformly distributed 32-bit value.
    uniform1 :: g -> m Word32

    -- | Generates a single uniformly distributed 64-bit value.
    uniform2 :: g -> m Word64

    -- | Generates a single 32-bit value up to a given limit with a uniform distribution.
    uniform1B :: Word32 -> g -> m Word32

    -- | Generates a single 64-bit value up to a given limit with a uniform distribution.
    uniform2B :: Word64 -> g -> m Word64

class RandomGenerator g where
    generateR :: g -> (forall g' m'. Generator g' m' => g' -> m' a) -> IO a

randomSeed :: forall a. Store a => IO a
randomSeed = case (size :: Size a) of
    ConstSize n -> getHardwareEntropy n >>= maybe (getEntropy n) pure >>= decodeIO
    _ -> fail "randomSeed: could not deduce size of seed"
{-# INLINE randomSeed #-}

generateIO :: Store a => (forall s. a -> ST s b) -> IO b
generateIO gen = randomSeed >>= runIO
  where
    runIO x = pure (runST (gen x))
{-# INLINE generateIO #-}

-- | Pseudo-random number generation using Marsaglia's MWC256, (also known as MWC8222)
-- multiply-with-carry generator
--
-- The MWC generator is supposed to be really fast. See
-- <https://hackage.haskell.org/package/mwc-random>.
data MWC = MWC
    deriving stock (Eq, Ord, Show, Read, Generic)

instance Generator (System.Random.MWC.Gen s) (ST s) where
    uniform1 = System.Random.MWC.uniformM
    {-# INLINE uniform1 #-}
    uniform2 = System.Random.MWC.uniformM
    {-# INLINE uniform2 #-}
    uniform1B bound = System.Random.MWC.uniformRM (0, bound)
    {-# INLINE uniform1B #-}
    uniform2B bound = System.Random.MWC.uniformRM (0, bound)
    {-# INLINE uniform2B #-}

instance SeedableGenerator MWC (ST s) where
    type State MWC (ST s) = System.Random.MWC.Gen s
    type Seed MWC = Vector Word32
    initialize MWC = System.Random.MWC.initialize
    {-# INLINE initialize #-}

instance RandomGenerator MWC where
    generateR gen r = generateIO (initialize gen >=> r)
    {-# INLINE generateR #-}

wordsTo64Bit :: Word32 -> Word32 -> Word64
wordsTo64Bit x y = fromIntegral x `shift` 32 .|. fromIntegral y
{-# INLINE wordsTo64Bit #-}

concatForM
    [ ''System.Random.PCG.Gen,
      ''System.Random.PCG.Fast.Gen,
      ''System.Random.PCG.Pure.Gen,
      ''System.Random.PCG.Fast.Pure.Gen,
      ''System.Random.PCG.Single.Gen
    ]
    $ \name ->
        [d|
            instance Generator ($(conT name) s) (ST s) where
                uniform1 = PCG.uniform1 id
                {-# INLINE uniform1 #-}
                uniform2 = PCG.uniform2 wordsTo64Bit
                {-# INLINE uniform2 #-}
                uniform1B = PCG.uniform1B id
                {-# INLINE uniform1B #-}
                uniform2B bound = PCG.uniformRW64 (0, bound)
                {-# INLINE uniform2B #-}
            |]

-- | The standard generator for PCG (Permuted Congruential Generator) algorithm, implemented in C.
--
-- The PCG generators have good statstical quality, is fast and use very little memory. See
-- <https://www.pcg-random.org>.
data PCG = PCG
    deriving stock (Eq, Ord, Show, Read, Generic)

instance SeedableGenerator PCG (ST s) where
    type Seed PCG = (Word64, Word64)
    type State PCG (ST s) = System.Random.PCG.Gen s
    initialize PCG = uncurry System.Random.PCG.initialize
    {-# INLINE initialize #-}

instance RandomGenerator PCG where
    generateR gen r = generateIO (initialize gen >=> r)
    {-# INLINE generateR #-}

-- | A fast generator for PCG (Permuted Congruential Generator) algorithm, implemented in C.
--
-- The PCG generators have good statstical quality, is fast and use very little memory. See
-- <https://www.pcg-random.org>.
data PCGFast = PCGFast
    deriving stock (Eq, Ord, Show, Read, Generic)

instance SeedableGenerator PCGFast (ST s) where
    type Seed PCGFast = Word64
    type State PCGFast (ST s) = System.Random.PCG.Fast.Gen s
    initialize PCGFast = System.Random.PCG.Fast.initialize
    {-# INLINE initialize #-}

instance RandomGenerator PCGFast where
    generateR gen r = generateIO (initialize gen >=> r)
    {-# INLINE generateR #-}

-- | The standard generator for PCG (Permuted Congruential Generator) algorithm, implemented in
-- Haskell.
--
-- The PCG generators have good statstical quality, is fast and use very little memory. See
-- <https://www.pcg-random.org>.
data PCGPure = PCGPure
    deriving stock (Eq, Ord, Show, Read, Generic)

instance SeedableGenerator PCGPure (ST s) where
    type Seed PCGPure = (Word64, Word64)
    type State PCGPure (ST s) = System.Random.PCG.Pure.Gen s
    initialize PCGPure = uncurry System.Random.PCG.Pure.initialize
    {-# INLINE initialize #-}

instance RandomGenerator PCGPure where
    generateR gen r = generateIO (initialize gen >=> r)
    {-# INLINE generateR #-}

-- | A fast generator for PCG (Permuted Congruential Generator) algorithm, implemented in Haskell.
--
-- The PCG generators have good statstical quality, is fast and use very little memory. See
-- <https://www.pcg-random.org>.
data PCGFastPure = PCGFastPure
    deriving stock (Eq, Ord, Show, Read, Generic)

instance SeedableGenerator PCGFastPure (ST s) where
    type Seed PCGFastPure = Word64
    type State PCGFastPure (ST s) = System.Random.PCG.Fast.Pure.Gen s
    initialize PCGFastPure = System.Random.PCG.Fast.Pure.initialize
    {-# INLINE initialize #-}

instance RandomGenerator PCGFastPure where
    generateR gen r = generateIO (initialize gen >=> r)
    {-# INLINE generateR #-}

-- | A single stream generator for PCG (Permuted Congruential Generator) algorithm, implemented in
-- C.
--
-- The PCG generators have good statstical quality, is fast and use very little memory. See
-- <https://www.pcg-random.org>.
data PCGSingle = PCGSingle
    deriving stock (Eq, Ord, Show, Read, Generic)

instance SeedableGenerator PCGSingle (ST s) where
    type Seed PCGSingle = Word64
    type State PCGSingle (ST s) = System.Random.PCG.Single.Gen s
    initialize PCGSingle = System.Random.PCG.Single.initialize
    {-# INLINE initialize #-}

instance RandomGenerator PCGSingle where
    generateR gen r = generateIO (initialize gen >=> r)
    {-# INLINE generateR #-}

-- | A unique generator for PCG (Permuted Congruential Generator) algorithm, implemented in C.
--
-- Guarantees the sequence to be unique by using the pointer address to select the output sequence.
--
-- The PCG generators have good statstical quality, is fast and use very little memory. See
-- <https://www.pcg-random.org>.
data PCGUnique = PCGUnique
    deriving stock (Eq, Ord, Show, Read, Generic)

instance Generator System.Random.PCG.Unique.Gen IO where
    -- type Seed GenPCGUnique = Word64
    -- initialize = System.Random.PCG.Unique.initialize
    -- {-# INLINE initialize #-}
    uniform1 = PCG.uniform1 id
    {-# INLINE uniform1 #-}
    uniform2 = PCG.uniform2 wordsTo64Bit
    {-# INLINE uniform2 #-}
    uniform1B = PCG.uniform1B id
    {-# INLINE uniform1B #-}
    uniform2B bound = PCG.uniformRW64 (0, bound)
    {-# INLINE uniform2B #-}

instance SeedableGenerator PCGUnique IO where
    type Seed PCGUnique = Word64
    type State PCGUnique IO = System.Random.PCG.Unique.Gen
    initialize PCGUnique = System.Random.PCG.Unique.initialize
    {-# INLINE initialize #-}

instance RandomGenerator PCGUnique where
    generateR gen r = randomSeed >>= initialize gen >>= r
    {-# INLINE generateR #-}

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

data Entropy = Entropy
    deriving stock (Eq, Ord, Show, Read, Generic)

instance Generator Entropy IO where
    uniform1 Entropy = getEntropy (sizeOf (0 :: Word32)) >>= decodeIO
    {-# INLINE uniform1 #-}
    uniform2 Entropy = getEntropy (sizeOf (0 :: Word64)) >>= decodeIO
    {-# INLINE uniform2 #-}
    uniform1B hi gen = uniformBounded hi (uniform1 gen)
    {-# INLINE uniform1B #-}
    uniform2B hi gen = uniformBounded hi (uniform2 gen)
    {-# INLINE uniform2B #-}

instance RandomGenerator Entropy where
    generateR gen r = r gen
    {-# INLINE generateR #-}

data HWEntropy = HWEntropy
    deriving stock (Eq, Ord, Show, Read, Generic)

unwrapIO :: Maybe a -> IO a
unwrapIO = maybe (fail "GenHWEntropy: no hardware entropy available") pure

instance Generator HWEntropy IO where
    uniform1 HWEntropy = getHardwareEntropy (sizeOf (0 :: Word32)) >>= unwrapIO >>= decodeIO
    {-# INLINE uniform1 #-}
    uniform2 HWEntropy = getHardwareEntropy (sizeOf (0 :: Word64)) >>= unwrapIO >>= decodeIO
    {-# INLINE uniform2 #-}
    uniform1B hi gen = uniformBounded hi (uniform1 gen)
    {-# INLINE uniform1B #-}
    uniform2B hi gen = uniformBounded hi (uniform2 gen)
    {-# INLINE uniform2B #-}

instance RandomGenerator HWEntropy where
    generateR gen r = r gen
    {-# INLINE generateR #-}
