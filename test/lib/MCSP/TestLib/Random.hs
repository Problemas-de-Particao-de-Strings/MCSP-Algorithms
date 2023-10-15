-- | Operations on partitions of `String`.
module MCSP.TestLib.Random (
    SimpleEnum,
    randomWithSingletons,
    pairShufflingChars,
    pairShufflingBlocks,
) where

import Control.Applicative (pure, (<$>))
import Data.Eq (Eq)
import Data.Function (($))
import Data.Int (Int)
import GHC.Enum (Bounded, Enum, succ)
import GHC.IsList (fromList)
import GHC.Num ((-))

import MCSP.Data.Pair (Pair, both, bothM, dupe)
import MCSP.Data.String (String (..), Unbox, concat, elem, empty, length, (!), (++))
import MCSP.Data.String.Extra (Partition)
import MCSP.System.Random (Random, partitions, shuffle, uniformR, uniformRE)

-- | Common constraints for a character.
type SimpleEnum a = (Enum a, Bounded a, Unbox a, Eq a)

-- ----------------- --
-- String generation --
-- ----------------- --

-- | Generates a string with random characters and guarantees absence of singletons.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> import Data.Word (Word8)
-- >>> generateWith (1,2) (randomReplicated 5 1 8) :: (String Word8)
-- 2 2 5 5 5
-- >>> generateWith (1,2) (randomReplicated 10 1 6) :: (String Word8)
-- 1 1 4 4 1 5 5 3 3 4
randomReplicated :: SimpleEnum a => Int -> a -> a -> Random (String a)
randomReplicated size lo hi = go size lo hi empty
  where
    go 0 _ _ old = pure old
    go 1 _ _ old = do
        index <- uniformR 0 (length old - 1)
        pure $ old :> (old ! index)
    go n lo' hi' old = do
        value <- uniformRE lo' hi'
        if value `elem` old
            then go (n - 1) lo' hi' (old :> value)
            else go (n - 2) lo' hi' (old :> value :> value)

-- | Generates a string with a fixed range of singletons and shuffled characters.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> generateWith (1,2) (randomWithSingletons 10 'a' 'c' 'e')
-- dbacaacebc
randomWithSingletons :: SimpleEnum a => Int -> a -> a -> a -> Random (String a)
randomWithSingletons n lo mid hi = do
    let singletons = fromList [succ mid .. hi]
    str <- randomReplicated (n - length singletons) lo mid
    shuffle (str ++ singletons)

-- --------- --
-- Shuffling --
-- --------- --

-- | Generates a pair of related strings by shuffling the input string characters.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> generateWith (1,2) (pairShufflingChars "impossibletoread!")
-- (teiposmosdbeair!l,mobldepreti!isosa)
pairShufflingChars :: String a -> Random (Pair (String a))
pairShufflingChars str@Unboxed = bothM shuffle (dupe str)

-- | Generates a pair partitions by shuffling the randomly broken blocks
-- of the input string.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> generateWith (1,2) (partitionsShufflingBlocks "brokeninblocksandshuffled!")
-- ([d,!,brokeninblocksands,e,huffl],[e,d,brokeninblocksands,huffl,!])
partitionsShufflingBlocks :: String a -> Random (Pair (Partition a))
partitionsShufflingBlocks str@Unboxed = do
    p <- partitions str
    bothM shuffle (dupe p)

-- | Generates a pair of related strings by shuffling randomly broken blocks
-- of the input string.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> generateWith (1,2) (pairShufflingBlocks "stillhardtoread!")
-- (d!stillharadtore,adstillhardtore!)
pairShufflingBlocks :: String a -> Random (Pair (String a))
pairShufflingBlocks str@Unboxed = both concat <$> partitionsShufflingBlocks str
