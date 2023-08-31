-- | Simplified `derivingUnbox`.
module MCSP.Data.String.Deriving (
    EnumLike,
    derivingUnboxVia,
) where

import Control.Applicative (pure)
import Control.Monad (fail, (>>=))
import Data.Bool (otherwise, (&&))
import Data.Char (toLower)
import Data.Data (Proxy (Proxy), Typeable, typeRep)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.List ((++))
import Data.Ord (max, min, (<=))
import Data.String (String)
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import GHC.Enum (Bounded (maxBound, minBound), Enum (fromEnum, toEnum))
import GHC.Err (error)
import Text.Show (show)

import Language.Haskell.TH (
    Dec,
    DecsQ,
    Name,
    Q,
    Type (..),
    TypeQ,
    nameBase,
    newName,
    sigD,
    varE,
    varP,
 )

-- | Makes the first character lowercase.
uncapitalize :: String -> String
uncapitalize [] = []
uncapitalize (ch : rest) = toLower ch : rest

-- | Extract the name of a data type.
getName :: Type -> Q String
getName (ConT name) = pure $ nameBase name
getName typ = fail $ "invalid type " ++ show typ

--- | Defines the signature and name for a cast function from @src@ to @dst@.
mkCastFn :: Type -> Type -> Q (Dec, Name)
mkCastFn src dst = do
    srcName <- getName src
    dstName <- getName dst
    fnName <- newName (uncapitalize srcName ++ "To" ++ dstName)
    fnSig <- sigD fnName [t|$(pure src) -> $(pure dst)|]
    pure (fnSig, fnName)

-- | Creates a prefix name for deriving `Data.Vector.Unboxed.Unbox`.
mkPrefix :: Type -> Type -> Q String
mkPrefix typ rep = do
    typName <- getName typ
    repName <- getName rep
    pure $ typName ++ "Via" ++ repName

-- | @T@ and @U@ from @T -> U@.
splitTypRep :: Type -> Q (Type, Type)
splitTypRep (ArrowT `AppT` typ `AppT` rep) = pure (typ, rep)
splitTypRep typ = fail $ "invalid deriving rule " ++ show typ

-- | Given @T -> U@, derives `Data.Vector.Unboxed.Unbox` @T@ by casting it to @U@.
--
-- >>> data DNA = A | C | G | T
-- >>> derivingUnboxVia [t|DNA -> Word8]
derivingUnboxVia :: TypeQ -> DecsQ
derivingUnboxVia rule = do
    (typ, rep) <- rule >>= splitTypRep
    (abSig, aToB) <- mkCastFn typ rep
    (baSig, bToA) <- mkCastFn rep typ
    decs <- [d|($(varP aToB), $(varP bToA)) = safeCasts|]
    prefix <- mkPrefix typ rep
    derive <- derivingUnbox prefix [t|$(pure typ) -> $(pure rep)|] (varE aToB) (varE bToA)
    pure $ [abSig, baSig] ++ decs ++ derive

-- | Types convertible to a bounded integer.
type EnumLike a = (Enum a, Bounded a, Typeable a)

-- | Cast @a@ to @b@ via their integer representations.
cast :: (EnumLike a, EnumLike b) => a -> b
cast = toEnum . fromEnum
{-# INLINE cast #-}

-- | Cast @b@ to @a@, clamping to valid values of @a@.
clamp :: forall a b. (EnumLike a, EnumLike b) => b -> a
clamp = toEnum . max minA . min maxA . fromEnum
  where
    minA = fromEnum (minBound :: a)
    maxA = fromEnum (maxBound :: a)
{-# INLINE clamp #-}

-- | Holds information about an `EnumLike` @a@.
data Info a = Info {minVal :: !Int, maxVal :: !Int, name :: !String}

-- | Extracts information about an `EnumLike` @a@.
info :: forall a. EnumLike a => Info a
info =
    Info
        { minVal = fromEnum (minBound :: a),
          maxVal = fromEnum (maxBound :: a),
          name = show $ typeRep (Proxy :: Proxy a)
        }

-- | Checks if @a@ is safely convertible to @b@ via `Enum` and `Bounded`, then returns the casts.
safeCasts :: forall a b. (EnumLike a, EnumLike b) => (a -> b, b -> a)
safeCasts
    | minVal rep <= minVal typ && maxVal typ <= maxVal rep = (cast, clamp)
    | otherwise =
        error $ name typ ++ " is larger than " ++ name rep ++ " and cannot be safely converted"
  where
    typ = info :: Info a
    rep = info :: Info b
