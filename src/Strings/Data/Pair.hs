module Strings.Data.Pair (
    Pair,
    uncheckedPair,
    shuffledGenes,
    shuffledPartitions,
) where

import Prelude hiding (String, concat)

import Data.Vector.Generic (concat)
import Data.Vector.Unboxed (Unbox, Vector)
import System.Random.PCG.Class (Generator)

import Strings.Data.String (String (..))
import Strings.Utils.Random (partitions, shuffle, shuffleV, uniformN)

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
    pure $ uncheckedPair (concat p1, concat p2)
