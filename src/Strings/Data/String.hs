module Strings.Data.String (
    Gene,
    String (..),
) where

import Prelude hiding (String)

import Data.Data (Typeable)
import Data.Foldable (Foldable (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Semigroup (..), Sum (..))
import Data.Store (Size (..), Store (..))
import Data.String (IsString (..))
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as M
import Data.Vector.Unboxed qualified as U
import GHC.Exts (IsList (..))

-- | Common constraints for a gene.
type Gene a = (Enum a, Bounded a, U.Unbox a)

-- | A string of genes `a`.
--
-- Implemented as a unboxed vector.
data String a where
    String :: U.Unbox a => !(U.Vector a) -> String a
    deriving newtype (Typeable)

instance Eq a => Eq (String a) where
    (String lhs) == (String rhs) = lhs == rhs
    (String lhs) /= (String rhs) = lhs /= rhs

instance Ord a => Ord (String a) where
    (String lhs) `compare` (String rhs) = lhs `compare` rhs
    (String lhs) < (String rhs) = lhs < rhs
    (String lhs) <= (String rhs) = lhs <= rhs
    (String lhs) > (String rhs) = lhs > rhs
    (String lhs) >= (String rhs) = lhs >= rhs
    max (String lhs) (String rhs) = String (max lhs rhs)
    min (String lhs) (String rhs) = String (min lhs rhs)

instance Show a => Show (String a) where
    showsPrec d = foldMap (showsPrec d)
    show = foldMap show

-- | Extract the inner contents of a `String`.
contents :: String a -> U.Vector a
contents (String v) = v

instance Semigroup (String a) where
    (String lhs) <> (String rhs) = String $ lhs U.++ rhs
    sconcat ((String x) :| xs) = String $ G.concatNE (x :| map contents xs)

instance U.Unbox a => Monoid (String a) where
    mempty = String U.empty
    mconcat = String . U.concat . map contents

instance Foldable String where
    fold (String v) = U.foldMap id v
    foldMap f (String v) = U.foldMap f v
    foldMap' f (String v) = U.foldMap' f v
    foldr f x (String v) = U.foldr f x v
    foldr' f x (String v) = U.foldr' f x v
    foldl f x (String v) = U.foldl f x v
    foldl' f x (String v) = U.foldl' f x v
    foldr1 f (String v) = U.foldr1 f v
    foldl1 f (String v) = U.foldl1 f v
    toList (String v) = U.toList v
    null (String v) = U.null v
    length (String v) = U.length v
    elem x (String v) = U.elem x v
    maximum (String v) = U.maximum v
    minimum (String v) = U.minimum v
    sum (String v) = U.sum v
    product (String v) = U.product v

instance U.Unbox a => IsList (String a) where
    type Item (String a) = a
    fromList = String . U.fromList
    fromListN n = String . U.fromListN n
    toList (String v) = U.toList v

instance a ~ Char => IsString (String a) where
    fromString = fromList

instance (Store a, U.Unbox a) => Store (String a) where
    size = VarSize calcSize
      where
        calcSize s = sizeOf size (G.length s) + sizeSum s
        sizeOf (ConstSize n) _ = n
        sizeOf (VarSize f) x = f x
        sizeSum v = getSum $ foldMap (Sum . sizeOf size) v
    poke (String v) = do
        poke $ U.length v
        U.forM_ v poke
    peek = do
        n <- peek
        v <- U.replicateM n peek
        pure $ String v

-- | Mutable variant of `String`.
newtype MString s a = MString {mContents :: U.MVector s a}

instance U.Unbox a => M.MVector MString a where
    basicLength = M.basicLength . mContents
    basicUnsafeSlice s n = MString . M.basicUnsafeSlice s n . mContents
    basicOverlaps lhs rhs = M.basicOverlaps (mContents lhs) (mContents rhs)
    basicUnsafeNew n = MString <$> M.basicUnsafeNew n
    basicInitialize = M.basicInitialize . mContents
    basicUnsafeReplicate n x = MString <$> M.basicUnsafeReplicate n x
    basicUnsafeRead = M.basicUnsafeRead . mContents
    basicUnsafeWrite = M.basicUnsafeWrite . mContents
    basicClear = M.basicClear . mContents
    basicSet = M.basicSet . mContents
    basicUnsafeCopy tgt src = M.basicUnsafeCopy (mContents tgt) (mContents src)
    basicUnsafeMove tgt src = M.basicUnsafeMove (mContents tgt) (mContents src)
    basicUnsafeGrow v n = MString <$> M.basicUnsafeGrow (mContents v) n

type instance G.Mutable String = MString

instance U.Unbox a => G.Vector String a where
    basicUnsafeFreeze (MString v) = String <$> G.basicUnsafeFreeze v
    basicUnsafeThaw (String v) = MString <$> G.basicUnsafeThaw v
    basicLength (String v) = G.basicLength v
    basicUnsafeSlice s n (String v) = String $ G.basicUnsafeSlice s n v
    basicUnsafeIndexM (String v) = G.basicUnsafeIndexM v
    basicUnsafeCopy (MString mv) (String v) = G.basicUnsafeCopy mv v
    elemseq (String v) = G.elemseq v
