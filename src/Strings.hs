module Strings (
    String (..),
    Pair,
    generate,
    safeCast,
    clampCast,
    shuffledGenes,
    shuffledPartitions,
) where

import Prelude hiding (String, replicate)

import Strings.Data.String
import Strings.Utils.Random

-- | Cast `a` to `b` if the range `[minBound, maxBound]` is not smaller for `a`.
safeCast :: forall a b. (Enum a, Bounded a, Enum b, Bounded b) => a -> b
safeCast = if safelyConvertible then toEnum . fromEnum else undefined
  where
    minA = fromEnum (minBound :: a)
    maxA = fromEnum (maxBound :: a)
    minB = fromEnum (minBound :: b)
    maxB = fromEnum (maxBound :: b)
    safelyConvertible = minB <= minA && maxA <= maxB
{-# INLINE safeCast #-}

-- | Cast `a` to `b`, clamping to valid values of `b`.
clampCast :: forall a b. (Enum a, Enum b, Bounded b) => a -> b
clampCast = toEnum . max minValue . min maxValue . fromEnum
  where
    minValue = fromEnum (minBound :: b)
    maxValue = fromEnum (maxBound :: b)
{-# INLINE clampCast #-}
