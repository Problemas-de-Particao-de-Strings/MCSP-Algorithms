-- | Operations on partitions of `String`.
module MCSP.TestLib.Random (
    SimpleEnum,
    randomShuffledChars,
    randomShuffledPartitions,
    randomShuffledBlocks,
    randomShuffledCharsWithSingletons,
) where

import Control.Applicative (pure, (<$>))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Eq (Eq)
import Data.Function (($))
import Data.Int (Int)
import GHC.Enum (Bounded, Enum, succ)
import GHC.IsList (fromList)
import GHC.Num ((-))

import MCSP.Data.String (String (..), Unbox, concat, elem, empty, length, replicateM, (!), (++))
import MCSP.Data.String.Extra (PartitionPair)
import MCSP.System.Random (Random, partitions, shuffle, uniformB, uniformE, uniformRE)

-- | Common constraints for a character.
type SimpleEnum a = (Enum a, Bounded a, Unbox a, Eq a)

-- | Generates a pair of related strings by shuffling all the characters.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> import Data.Word (Word8)
-- >>> generateWith (1,2) (randomShuffledChars 5) :: (String Word8, String Word8)
-- (38 147 20 189 107,147 38 189 20 107)
randomShuffledChars :: SimpleEnum a => Int -> Random (String a, String a)
randomShuffledChars n = do
    s1 <- replicateM n uniformE
    s2 <- shuffle s1
    pure (s1, s2)

-- | Generates a string with random characters and guarantees absence of singletons.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> import Data.Word (Word8)
-- >>> import MCSP.Data.String (empty)
-- >>> generateWith (1,2) (randomReplicated 5 1 8) :: (String Word8)
-- 2 2 5 5 5
-- >>> generateWith (1,2) (randomReplicated 10 1 6) :: (String Word8)
-- 1 1 4 4 1 5 5 3 3 4
randomReplicated :: SimpleEnum a => Int -> a -> a -> Random (String a)
randomReplicated size lo hi = go size lo hi empty
  where
    go 0 _ _ old = pure old
    go 1 _ _ old = do
        index <- uniformB $ length old
        pure $ old :> (old ! index)
    go n lo' hi' old = do
        value <- uniformRE lo' hi'
        if value `elem` old
            then go (n - 1) lo' hi' (old :> value)
            else go (n - 2) lo' hi' (old :> value :> value)

-- | Generates a pair of related strings with a fixed range of singletons and shuffled
-- characters.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> import Data.Char (Char)
-- >>> generateWith (1,2) (randomShuffledCharsWithSingletons 10 'a' 'c' 'e') :: (String Char, String Char)
-- (dbacaacebc,adeacbbcac)
randomShuffledCharsWithSingletons ::
    SimpleEnum a =>
    Int
    -> a
    -> a
    -> a
    -> Random (String a, String a)
randomShuffledCharsWithSingletons n lo mid hi = do
    str <- randomReplicated (n - length singletons) lo mid
    let str' = str ++ singletons
    s1 <- shuffle str'
    s2 <- shuffle str'
    pure (s1, s2)
  where
    singletons = fromList [succ mid .. hi]

-- | Generates a pair of common partitions by shuffling the blocks.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> import Data.Word (Word8)
-- >>> generateWith (1,2) (randomShuffledPartitions 5) :: (PartitionPair Word8)
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
-- >>> import MCSP.System.Random (generateWith)
-- >>> import Data.Word (Word8)
-- >>> generateWith (1,2) (randomShuffledBlocks 5) :: (String Word8, String Word8)
-- (20 189 107 38 147,38 147 20 189 107)
randomShuffledBlocks :: SimpleEnum a => Int -> Random (String a, String a)
randomShuffledBlocks n = bimap concat concat <$> randomShuffledPartitions n
