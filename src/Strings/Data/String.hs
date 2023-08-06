module Strings.Data.String (
    String (..),
    Pair,
    length,
    splitAt,
    shuffledGenes,
    shuffledPartitions,
) where

import Data.Vector.Generic qualified as G
import Data.Vector.Unboxed (Unbox, Vector, foldMap)
import System.Random.PCG.Class (Generator)
import Prelude hiding (String, foldMap, length, splitAt)

import Strings.Utils.Random (partitions, shuffle, shuffleV, uniformN)

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

-- | Random pair with shuffled genes.
shuffledGenes :: (Generator g m, Unbox a, Enum a, Bounded a) => Int -> g -> m (Pair a)
shuffledGenes n gen = do
    s1 <- uniformN n gen
    s2 <- shuffleV s1 gen
    pure $ uncheckedPair (s1, s2)

-- | Random pair with shuffled partitons of genes.
shuffledPartitions :: (Generator g m, Unbox a, Enum a, Bounded a) => Int -> g -> m (Pair a)
shuffledPartitions n gen = do
    s0 <- uniformN n gen
    p1 <- partitions s0 gen
    p2 <- shuffle p1 gen
    pure $ uncheckedPair (G.concat p1, G.concat p2)
