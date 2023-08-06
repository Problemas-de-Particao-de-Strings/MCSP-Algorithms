{-# LANGUAGE TypeFamilies #-}

module Strings.Data.String (
    String (..),
) where

import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as M
import Data.Vector.Unboxed qualified as U
import Prelude hiding (String)

-- | A string of genes `a`.
--
-- Implemented as a unboxed vector.
newtype String a = String {contents :: U.Vector a}
    deriving newtype (Eq, Ord)

instance (Show a, U.Unbox a) => Show (String a) where
    showsPrec d = U.foldMap (showsPrec d) . contents
    show s = U.foldMap show (contents s)

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
    basicUnsafeFreeze ms = String <$> G.basicUnsafeFreeze (mContents ms)
    basicUnsafeThaw s = MString <$> G.basicUnsafeThaw (contents s)
    basicLength = G.basicLength . contents
    basicUnsafeSlice s n = String . G.basicUnsafeSlice s n . contents
    basicUnsafeIndexM = G.basicUnsafeIndexM . contents
    basicUnsafeCopy ms s = G.basicUnsafeCopy (mContents ms) (contents s)
    elemseq = G.elemseq . contents
