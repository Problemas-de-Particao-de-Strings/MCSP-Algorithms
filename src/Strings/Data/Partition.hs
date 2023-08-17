module Strings.Data.Partition (
    Partition,
    concat,
    chars,
    randomShuffle,
    shuffled,
    shuffledPartitions,
) where

import Prelude hiding (String, concat, splitAt)

import Data.Bifunctor (Bifunctor (bimap))

import Strings.Data.String (String (Null, (:<:)), Unbox, concat, replicateM)
import Strings.System.Random (Random, partitions, shuffle, uniformE)

-- | A collection of substring of the same string.
type Partition a = [String a]

-- | Common constraints for a character.
type Character a = (Enum a, Bounded a, Unbox a)

-- | Split the `String` in substrings of 1 char each.
chars :: String a -> Partition a
chars (ch :<: rest) = ch : chars rest
chars Null = []

-- | Generates a pair of string where one is a simple permutation of the other.
randomShuffle :: Character a => Int -> Random (String a, String a)
randomShuffle n = do
    s1 <- replicateM n uniformE
    s2 <- shuffle s1
    pure (s1, s2)

-- | Generates a pair of partitions where one is a simple permutation of the other.
shuffled :: Character a => Int -> Random (Partition a, Partition a)
shuffled n = do
    str <- replicateM n uniformE
    p1 <- partitions str
    p2 <- shuffle p1
    pure (p1, p2)

-- | Generates a pair of strings where one is a permutation of random partitions from the other.
shuffledPartitions :: Character a => Int -> Random (String a, String a)
shuffledPartitions n = bimap concat concat <$> shuffled n
