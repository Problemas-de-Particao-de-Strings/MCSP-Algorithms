module Strings.Data.Pair (
    Pair,
    shuffledChars,
    shuffledPartitions,
) where

import Prelude hiding (String, concat)

import Data.Vector.Generic (concat, replicateM)

import Strings.Data.String (Character, String (..))
import Strings.System.Random (Random, partitions, shuffle, uniformE)

-- | A pair of strings. No restrictiong applied.
type Pair a = (String a, String a)

-- | Random pair of string with shuffled characters.
shuffledChars :: Character a => Int -> Random (Pair a)
shuffledChars n = do
    s1 <- replicateM n uniformE
    s2 <- shuffle s1
    pure (s1, s2)

-- | Random pair with shuffled partitons of characters.
shuffledPartitions :: Character a => Int -> Random (Pair a)
shuffledPartitions n = do
    str <- replicateM n uniformE
    p1 <- partitions str
    p2 <- shuffle p1
    pure (concat p1, concat p2)
