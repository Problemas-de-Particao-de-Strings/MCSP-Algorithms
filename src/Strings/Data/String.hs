module Strings.Data.String (
    String (..),
) where

import Prelude hiding (String)

import Data.Foldable (Foldable (..))
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as M
import Data.Vector.Unboxed qualified as U

-- | A string of genes `a`.
--
-- Implemented as a unboxed vector.
data String a where
    String :: U.Unbox a => !(U.Vector a) -> String a

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
