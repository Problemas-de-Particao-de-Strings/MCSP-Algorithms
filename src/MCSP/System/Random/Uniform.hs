-- | Randomly uniformly distributions.
module MCSP.System.Random.Uniform (
    Uniform (..),
) where

import Control.Applicative (pure)
import Control.Monad.Extra (concatForM)
import Data.Bits (FiniteBits, finiteBitSize, isSigned, shift, (.&.))
import Data.Bool (Bool (..), not, otherwise, (&&))
import Data.Eq (Eq (..))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Ord (Ord (..))
import Data.Tuple.Extra (both)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import GHC.Base (asTypeOf, errorWithoutStackTrace)
import GHC.Enum (Bounded (..))
import GHC.Float (Double, Float)
import GHC.Num ((*), (+), (-))
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

-- ----------------- --
-- Integer instances --
-- ----------------- --

-- | Break a 64 bit integer into two 32 bit integers.
splitW64 :: Word64 -> (Word32, Word32)
splitW64 val = toW32 `both` (val `shift` bitsW32, val)
  where
    maxW32 = fromIntegral (maxBound :: Word32)
    bitsW32 = finiteBitSize (maxBound :: Word32)
    toW32 x = fromIntegral (x .&. maxW32)
{-# INLINE splitW64 #-}

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
fromUniformW64 gen = fromIntegral <$> uniform2 gen
{-# INLINE fromUniformW64 #-}

-- | Generate a uniform integer by casting from a bounded uniform `Word32`.
--
-- Note that the cast does silent truncation from the input bound, making this only uniformly
-- distributed for integers @a@ smaller than 32-bits.
fromBoundedW32 :: (Integral a, Integral b, Generator g m) => b -> (a, a) -> g -> m a
fromBoundedW32 subTy (x, y) gen = toRange <$> uniform1B boundW32 gen
  where
    (lo, hi) = if x < y then (x, y) else (y, x)
    boundW32 = fromIntegral $ asTypeOf (fromIntegral (hi - lo)) subTy
    toRange val = fromIntegral val + lo
{-# INLINE fromBoundedW32 #-}

fromBoundedW64 :: (Integral a, Integral b, Generator g m) => b -> (a, a) -> g -> m a
fromBoundedW64 subTy (x, y) gen = toRange <$> uniform2B boundW64 gen
  where
    (lo, hi) = if x < y then (x, y) else (y, x)
    boundW64 = fromIntegral $ asTypeOf (fromIntegral (hi - lo)) subTy
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
    | not signed && size <= finiteBitSize (maxBound :: Word64) = fromBoundedW64 (0 :: Word64)
    | signed && size <= finiteBitSize (maxBound :: Int8) = fromBoundedW32 (0 :: Word8)
    | signed && size <= finiteBitSize (maxBound :: Int16) = fromBoundedW32 (0 :: Word16)
    | signed && size <= finiteBitSize (maxBound :: Int32) = fromBoundedW32 (0 :: Word32)
    | signed && size <= finiteBitSize (maxBound :: Int64) = fromBoundedW64 (0 :: Word64)
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

-- ----------------- --
-- Floating instance --
-- ----------------- --

-- | Construct a `Float` in range @(0,1]@ from a uniformly distributed `Word32`.
--
-- Taken from [mwc-random](https://hackage.haskell.org/package/mwc-random).
wordToFloat :: Word32 -> Float
wordToFloat x = fromIntegral i * m_inv_32 + 0.5 + m_inv_33
  where
    m_inv_33 = 1.16415321826934814453125e-10
    m_inv_32 = 2.3283064365386962890625e-10
    i = fromIntegral x :: Int32
{-# INLINE wordToFloat #-}

-- | Generates values in range @(0,1]@.
instance Uniform Float where
    uniform gen = wordToFloat <$> uniform1 gen
    {-# INLINE uniform #-}
    uniformR (x1, x2) gen = do
        value <- uniform gen
        pure (x1 + (x2 - x1) * value)
    {-# INLINE uniformR #-}

-- | Construct a `Double` in range @(0,1]@ from a pair of uniformly distributed `Word32`.
--
-- Taken from [mwc-random](https://hackage.haskell.org/package/mwc-random).
wordsToDouble :: (Word32, Word32) -> Double
wordsToDouble (x, y) =
    fromIntegral u * m_inv_32
        + (0.5 + m_inv_53)
        + fromIntegral (v .&. 0xFFFFF) * m_inv_52
  where
    m_inv_52 = 2.220446049250313080847263336181640625e-16
    m_inv_53 = 1.1102230246251565404236316680908203125e-16
    m_inv_32 = 2.3283064365386962890625e-10
    u = fromIntegral x :: Int32
    v = fromIntegral y :: Int32
{-# INLINE wordsToDouble #-}

-- | Generates values in range @(0,1]@.
instance Uniform Double where
    uniform gen = wordsToDouble . splitW64 <$> uniform2 gen
    {-# INLINE uniform #-}
    uniformR (x1, x2) gen = do
        value <- uniform gen
        pure (x1 + (x2 - x1) * value)
    {-# INLINE uniformR #-}

-- ---------------- --
-- Boolean instance --
-- ---------------- --

-- | Take the last bit in a `Word32`.
wordToBool :: Word32 -> Bool
wordToBool i = i .&. 1 /= 0
{-# INLINE wordToBool #-}

instance Uniform Bool where
    uniform gen = wordToBool <$> uniform1 gen
    {-# INLINE uniform #-}
    uniformR (True, True) _ = pure True
    uniformR (False, False) _ = pure False
    uniformR (_, _) gen = uniform gen
    {-# INLINE uniformR #-}

-- --------------- --
-- Tuple instances --
-- --------------- --

instance Uniform () where
    uniform _ = pure ()
    {-# INLINE uniform #-}
    uniformR ((), ()) _ = pure ()
    {-# INLINE uniformR #-}

instance (Uniform a, Uniform b) => Uniform (a, b) where
    uniform gen = do
        x <- uniform gen
        y <- uniform gen
        pure (x, y)
    {-# INLINE uniform #-}
    uniformR ((x1, y1), (x2, y2)) gen = do
        x <- uniformR (x1, x2) gen
        y <- uniformR (y1, y2) gen
        pure (x, y)
    {-# INLINE uniformR #-}

instance (Uniform a, Uniform b, Uniform c) => Uniform (a, b, c) where
    uniform gen = do
        x <- uniform gen
        y <- uniform gen
        z <- uniform gen
        pure (x, y, z)
    {-# INLINE uniform #-}
    uniformR ((x1, y1, z1), (x2, y2, z2)) gen = do
        x <- uniformR (x1, x2) gen
        y <- uniformR (y1, y2) gen
        z <- uniformR (z1, z2) gen
        pure (x, y, z)
    {-# INLINE uniformR #-}
