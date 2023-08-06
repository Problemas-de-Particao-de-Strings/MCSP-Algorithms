module Strings.Data.String.Deriving (
    derivingUnbox,
    safeCasts,
) where

import Data.Data (Proxy (Proxy), Typeable, typeRep)
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import GHC.Stack (HasCallStack)

-- | Cast `a` to `b` via their integer representations.
cast :: (Enum a, Enum b) => a -> b
cast = toEnum . fromEnum
{-# INLINE cast #-}

-- | Cast `a` to `b`, clamping to valid values of `b`.
castClamped :: forall a b. (Enum a, Enum b, Bounded b) => a -> b
castClamped = toEnum . max minB . min maxB . fromEnum
  where
    minB = fromEnum (minBound :: b)
    maxB = fromEnum (maxBound :: b)
{-# INLINE castClamped #-}

-- | Types convertible to a bounded integer.
type IntConvertible a = (Enum a, Bounded a, Typeable a)

-- | Error message for non convertible types.
invalidCast :: HasCallStack => String -> String -> never
invalidCast typeA typeB = error (typeA ++ " is larger than " ++ typeB ++ " and cannot be safely converted")

-- | Cast `a` to `b` if the range `[minBound, maxBound]` is not smaller for `a`.
safeCasts :: forall a b. (HasCallStack, IntConvertible a, IntConvertible b) => (a -> b, b -> a)
safeCasts =
    if safelyConvertible
        then (cast, castClamped)
        else invalidCast (show $ typeRep (Proxy :: Proxy a)) (show $ typeRep (Proxy :: Proxy b))
  where
    minA = fromEnum (minBound :: a)
    maxA = fromEnum (maxBound :: a)
    minB = fromEnum (minBound :: b)
    maxB = fromEnum (maxBound :: b)
    safelyConvertible = minB <= minA && maxA <= maxB
{-# INLINE safeCasts #-}
