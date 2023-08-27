-- | Operations on partitions of `String`.
module Strings.Data.Partition (
    Partition,
    PartitionPair,
    SimpleEnum,
    concat,
    chars,
    randomShuffledChars,
    randomShuffledPartitions,
    randomShuffledBlocks,
    randomShuffledCharsWithSingletons,
) where

import Control.Applicative (pure, (<$>))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (foldl', length)
import Data.Int (Int)
import GHC.Enum (Bounded, Enum)
import GHC.Num ((-))

import Strings.Data.String (String (Null, (:>), (:>:)), Unbox, concat, replicateM)
import Strings.System.Random (Random, partitions, shuffle, uniformE, uniformRE)

-- | A collection of substrings of the same string.
type Partition a = [String a]

-- | A pair of partitions.
type PartitionPair a = (Partition a, Partition a)

-- | Common constraints for a character.
type SimpleEnum a = (Enum a, Bounded a, Unbox a)

-- | /O(n)/ Split the string in substrings of 1 char each.
--
-- >>> chars "abcd"
-- [a,b,c,d]
chars :: String a -> Partition a
chars = go []
  where
    go p (rest :>: ch) = go (ch : p) rest
    go p Null = p

-- | Generates a pair of related strings by shuffling all the characters.
--
-- >>> import Strings.System.Random (generateWith)
-- >>> import Data.Word (Word8)
-- >>> generateWith (1,2) $ randomShuffledChars 5 :: (String Word8, String Word8)
-- (38 147 20 189 107,147 38 189 20 107)
randomShuffledChars :: SimpleEnum a => Int -> Random (String a, String a)
randomShuffledChars n = do
    s1 <- replicateM n uniformE
    s2 <- shuffle s1
    pure (s1, s2)

-- | Generates a pair of related strings with a defined number of singletons and shuffled characters.
--
-- >>> import Strings.System.Random (generateWith)
-- >>> import Data.Word (Word8)
-- >>> generateWith (1,2) $ randomShuffledCharsWithSingletons 10 1 3 [4, 5] :: (String Word8, String Word8)
-- (1 2 2 5 3 1 4 3 1 3,5 3 1 1 2 1 3 4 2 3)
randomShuffledCharsWithSingletons :: SimpleEnum a => Int -> a -> a -> [a] -> Random (String a, String a)
randomShuffledCharsWithSingletons n lo hi singles = do
    str <- replicateM (n - length singles) (uniformRE lo hi)
    let str' = foldl' (:>) str singles
    s1 <- shuffle str'
    s2 <- shuffle str'
    pure (s1, s2)

-- | Generates a pair of common partitions by shuffling the blocks.
--
-- >>> import Strings.System.Random (generateWith)
-- >>> import Data.Word (Word8)
-- >>> generateWith (1,2) $ randomShuffledPartitions 5 :: (Partition Word8, Partition Word8)
-- ([20 189 107,38 147],[38 147,20 189 107])
randomShuffledPartitions :: SimpleEnum a => Int -> Random (PartitionPair a)
randomShuffledPartitions n = do
    str <- replicateM n uniformE
    p <- partitions str
    p1 <- shuffle p
    p2 <- shuffle p
    pure (p1, p2)

-- | Generates a pair of related strings by shuffling the randomly generated blocks.
--
-- >>> import Strings.System.Random (generateWith)
-- >>> import Data.Word (Word8)
-- >>> generateWith (1,2) $ randomShuffledBlocks 5 :: (String Word8, String Word8)
-- (20 189 107 38 147,38 147 20 189 107)
randomShuffledBlocks :: SimpleEnum a => Int -> Random (String a, String a)
randomShuffledBlocks n = bimap concat concat <$> randomShuffledPartitions n
