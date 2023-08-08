module Strings.Data.Pair (
    Pair,
    uncheckedPair,
    shuffledGenes,
    shuffledPartitions,
) where

import Prelude hiding (String, concat)

import Data.Vector.Generic (concat, replicateM)
import Data.Vector.Unboxed (Unbox, Vector)

import Strings.Data.String (Gene, String (..))
import Strings.System.Random (Random, partitions, shuffle, uniformE)

-- | A pair of strings. No restrictiong applied.
type Pair a = (String a, String a)

-- | Creates a pair of strings from contents, no checking is made.
uncheckedPair :: Unbox a => (Vector a, Vector a) -> Pair a
uncheckedPair (x, y) = (String x, String y)

-- | Random pair with shuffled genes.
shuffledGenes :: Gene a => Int -> Random (Pair a)
shuffledGenes n = do
    s1 <- replicateM n uniformE
    s2 <- shuffle s1
    pure (s1, s2)

-- | Random pair with shuffled partitons of genes.
shuffledPartitions :: Gene a => Int -> Random (Pair a)
shuffledPartitions n = do
    str <- replicateM n uniformE
    p1 <- partitions str
    p2 <- shuffle p1
    pure (concat p1, concat p2)
