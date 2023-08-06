module Strings.Data.String (
    String (..),
    Pair,
    length,
    splitAt,
) where

import Data.Vector.Generic qualified as G
import Data.Vector.Unboxed (Unbox, Vector, foldMap)
import Prelude hiding (String, foldMap, length, splitAt)

-- | A string of genes `a`.
--
-- Implemented as a unboxed vector.
newtype String a = String {contents :: Vector a}
    deriving newtype (Eq, Ord)

instance (Show a, Unbox a) => Show (String a) where
    showsPrec d = foldMap (showsPrec d) . contents
    show s = foldMap show (contents s)

-- | The number of genes in a `String`.
length :: Unbox a => String a -> Int
length = G.basicLength . contents

-- | Yield the first `n` elements paired with the remainder.
splitAt :: Unbox a => Int -> String a -> (String a, String a)
splitAt n = uncheckedPair . G.splitAt n . contents

-- | A pair of strings. No restrictiong applied.
type Pair a = (String a, String a)

-- | Creates a pair of strings from contents, no checking is made.
uncheckedPair :: (Vector a, Vector a) -> Pair a
uncheckedPair (x, y) = (String x, String y)
