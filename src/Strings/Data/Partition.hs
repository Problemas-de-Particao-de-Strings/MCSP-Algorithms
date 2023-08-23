-- | Operations on partitions of `String`.
module Strings.Data.Partition (
    Partition,
    SimpleEnum,
    concat,
    chars,
    randomShuffledChars,
    randomShuffledPartitions,
    randomShuffledBlocks,
) where

import Control.Applicative (pure, (<$>))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Int (Int)
import GHC.Enum (Bounded, Enum)

import Strings.Data.String (String (Null, (:>:)), Unbox, concat, replicateM)
import Strings.System.Random (Random, partitions, shuffle, uniformE)

-- | A collection of substrings of the same string.
type Partition a = [String a]

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

-- | Generates a pair of common partitions by shuffling the blocks.
--
-- >>> import Strings.System.Random (generateWith)
-- >>> import Data.Word (Word8)
-- >>> generateWith (1,2) $ randomShuffledPartitions 5 :: (Partition Word8, Partition Word8)
-- ([20 189 107,38 147],[38 147,20 189 107])
randomShuffledPartitions :: SimpleEnum a => Int -> Random (Partition a, Partition a)
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
