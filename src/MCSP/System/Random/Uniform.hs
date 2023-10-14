-- | Randomly uniformly distributions.
module MCSP.System.Random.Uniform (
    Uniform (..),
) where

import Control.Monad.Extra (concatForM)
import Data.Bits (FiniteBits, finiteBitSize, isSigned, shift, (.|.))
import Data.Bool (not, otherwise, (&&))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Ord (Ord (..))
import Data.Word (Word, Word16, Word32, Word64, Word8)
import GHC.Base (asTypeOf, errorWithoutStackTrace)
import GHC.Enum (Bounded (..))
import GHC.Num (Integer, (+), (-))
import GHC.Real (Integral, fromIntegral)
import Language.Haskell.TH (conT)
import Text.Printf (printf)

import MCSP.System.Random.Generator (Generator (..))

-- | The class of types for which we can generate uniformly distributed random values.
class Uniform a where
    -- | Generate a single uniformly distributed random variate.
    --
    -- The range of values produced varies by type:
    -- * For fixed-width integral types, the type's entire range is used.
    -- * For floating point numbers, the range @(0,1]@ is used. Zero is explicitly excluded,
    --   to allow variates to be used in statistical calculations that require non-zero values
    --   (e.g. uses of the `Numeric.log` function).
    uniform :: Generator g m => g -> m a

    -- | Generate single uniformly distributed random variable in a given range.
    --
    --  * For integral types inclusive range is used.
    --  * For floating point numbers range @(a,b]@ is used if one ignores rounding errors.
    uniformR :: Generator g m => (a, a) -> g -> m a

-- | Generate a uniform integer by casting from a uniform `Word32`.
--
-- Note that the cast does silent truncation, so this is only really uniformly distributed for
-- integers @a@ that are powers of 2 and are smaller than 32-bits.
fromUniformW32 :: (Integral a, Generator g m) => g -> m a
fromUniformW32 gen = fromIntegral <$> uniform1 gen
{-# INLINE fromUniformW32 #-}

-- | Generate a uniform integer by casting from a uniform `Word64`.
--
-- Note that the cast does silent truncation, so this is only really uniformly distributed for
-- integers @a@ that are powers of 2 and are smaller than 64-bits.
fromUniformW64 :: (Integral a, Generator g m) => g -> m a
fromUniformW64 gen = fromIntegral . toW64 <$> uniform2 gen
  where
    toW64 :: (Word32, Word32) -> Word64
    toW64 (x, y) = fromIntegral x `shift` 32 .|. fromIntegral y
{-# INLINE fromUniformW64 #-}

-- | Generate a uniform integer by casting from a bounded uniform `Word32`.
--
-- Note that the cast does silent truncation from the input bound, making this only uniformly
-- distributed for integers @a@ smaller than 32-bits.
fromBoundedW32 :: (Integral a, Integral b, Generator g m) => b -> (a, a) -> g -> m a
fromBoundedW32 subTy (x, y) gen = toRange <$> uniform1B maxW32 gen
  where
    (lo, hi) = if x < y then (x, y) else (y, x)
    maxW32 = fromIntegral $ asTypeOf (fromIntegral hi - fromIntegral lo) subTy
    toRange val = fromIntegral val + lo
{-# INLINE fromBoundedW32 #-}

-- | Generate a uniform integer by casting from a bounded uniform `Word64`.
--
-- Note that the cast does silent truncation from the input bound, making this only uniformly
-- distributed for integers @a@ smaller than 64-bits.
fromBoundedW64 :: (Integral a, Integral b, Generator g m) => b -> (a, a) -> g -> m a
fromBoundedW64 subTy (x, y) gen = toRange <$> uniform1B maxW32 gen
  where
    (lo, hi) = if x < y then (x, y) else (y, x)
    maxW32 = fromIntegral $ asTypeOf (fromIntegral hi - fromIntegral lo) subTy
    toRange val = fromIntegral val + lo
{-# INLINE fromBoundedW64 #-}

-- | Run either `fromUniformW32` or `fromUniformW64` dependending on the width of the integer @a@.
--
-- Abort execution with an error if @a@ is larger than 64 bits.
fromUniformWord ::
    forall a g m.
    (FiniteBits a, Bounded a, Integral a, Generator g m) =>
    g
    -> m a
fromUniformWord
    | size <= finiteBitSize (maxBound :: Word32) = fromUniformW32
    | size <= finiteBitSize (maxBound :: Word64) = fromUniformW64
    | otherwise = errorWithoutStackTrace (printf "fromUniformWord: invalid integer size of %d" size)
  where
    size = finiteBitSize (maxBound :: a)
{-# INLINE fromUniformWord #-}

-- | Run either `fromBoundedW32` or `fromBoundedW64` dependending on the width of the integer @a@.
--
-- Abort execution with an error if @a@ is larger than 64 bits.
fromBoundedWord ::
    forall a g m.
    (FiniteBits a, Bounded a, Integral a, Generator g m) =>
    (a, a)
    -> g
    -> m a
fromBoundedWord
    | not signed && size <= finiteBitSize (maxBound :: Word32) = fromBoundedW32 (0 :: a)
    | not signed && size <= finiteBitSize (maxBound :: Word64) = fromBoundedW64 (0 :: a)
    | signed && size <= finiteBitSize (maxBound :: Int8) = fromBoundedW32 (0 :: Int16)
    | signed && size <= finiteBitSize (maxBound :: Int16) = fromBoundedW32 (0 :: Int32)
    | signed && size <= finiteBitSize (maxBound :: Int32) = fromBoundedW32 (0 :: Int64)
    | signed && size <= finiteBitSize (maxBound :: Int64) = fromBoundedW32 (0 :: Integer)
    | otherwise = errorWithoutStackTrace (printf "fromBoundedWord: invalid integer size of %d" size)
  where
    size = finiteBitSize (maxBound :: a)
    signed = not (isSigned (minBound :: a))
{-# INLINE fromBoundedWord #-}

concatForM
    [''Word, ''Word8, ''Word16, ''Word32, ''Word64, ''Int, ''Int8, ''Int16, ''Int32, ''Int64]
    $ \name ->
        [d|
            instance Uniform $(conT name) where
                uniform = fromUniformWord
                {-# INLINE uniform #-}
                uniformR = fromBoundedWord
                {-# INLINE uniformR #-}
            |]
