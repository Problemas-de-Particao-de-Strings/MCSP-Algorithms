-- | Correctness checking.
module MCSP.TestLib.Heuristics.Safe (
    -- * Heuristic correctness
    Debug,
    runChecked,

    -- * Checked operations
    checkedLen,
    checkedDiv,
) where

import Control.Applicative (pure, (<|>))
import Control.Monad (fail, void, when)
import Data.Bool (otherwise)
import Data.Eq (Eq (..))
import Data.Foldable (Foldable, length)
import Data.Function (($))
import Data.Int (Int)
import Data.List (sort, (++))
import Data.List.Extra ((!?))
import Data.Maybe (Maybe (..))
import Data.Ord (Ord (..))
import Data.Ratio ((%))
import Data.String qualified as Text (String)
import GHC.Real (Fractional, Integral, fromRational, toInteger)
import System.IO (IO)
import Text.Show (Show, show)

import MCSP.Data.Meta (evalMeta)
import MCSP.Data.Pair (Pair, both, liftP, zipM)
import MCSP.Data.String (ShowString, String (..), concat)
import MCSP.Data.String.Extra (Partition, alphabet, occurrences)
import MCSP.Heuristics (Heuristic)

-- | Common constraints for debugging a heuristic.
type Debug a = (Show a, ShowString a, Ord a)

-- | Run a heuristic checking the inputs and output for correctnes.
--
-- >>> import MCSP.Data.Meta (evalMeta)
-- >>> import MCSP.Heuristics (trivial)
--
-- >>> runChecked trivial ("abba", "abab")
-- ([a,b,b,a],[a,b,a,b])
--
-- >>> runChecked trivial ("abba", "abad")
-- user error (mismatch in input strings 'abba' and 'abad': fromList "ab" != fromList "abd")
runChecked :: Debug a => Heuristic a -> Pair (String a) -> IO (Pair (Partition a))
runChecked heuristic pair = do
    checkBalanced pair
    let partitions = evalMeta $ heuristic pair
    void $ zipM $ liftP checkPartition pair partitions
    checkCommonPartition partitions
    pure partitions

-- | Check that the input strings are balanced.
--
-- >>> checkBalanced ("abba", "abab")
-- <BLANKLINE>
--
-- >>> checkBalanced ("abba", "abad")
-- user error (mismatch in input strings 'abba' and 'abad': fromList "ab" != fromList "abd")
--
-- >>> checkBalanced ("abba", "abbb")
-- user error (mismatch in input strings 'abba' and 'abbb': fromList [('a',2),('b',2)] != fromList [('a',1),('b',3)])
checkBalanced :: Debug a => Pair (String a) -> IO ()
checkBalanced (s1, s2) = do
    -- the strings must have the same alphabet
    when (alphabet s1 /= alphabet s2) $
        show (alphabet s1) =/= show (alphabet s2)
    -- the strings must have the same occurence of characters
    when (occurrences s1 /= occurrences s2) $
        show (occurrences s1) =/= show (occurrences s2)
  where
    (=/=) = mismatchIn $ "input strings " ++ quoted s1 ++ " and " ++ quoted s2

-- | Check that the partition is valid for the given string.
--
-- >>> checkPartition "abba" ["ab", "b", "a"]
-- <BLANKLINE>
--
-- >>> checkPartition "abba" ["ab", "a", "b"]
-- user error (mismatch in partition for 'abba': [ab,a,b] != abba)
--
-- >>> checkPartition "abba" ["ab", "bb"]
-- user error (mismatch in partition for 'abba': fromList [('a',1),('b',3)] != fromList [('a',2),('b',2)])
checkPartition :: Debug a => String a -> [String a] -> IO ()
checkPartition str@Unboxed partition = do
    -- the partition must have the same occurence of characters
    when (occurrences (concat partition) /= occurrences str) $
        show (occurrences (concat partition)) =/= show (occurrences str)
    -- the partition must have the same order of characters
    when (concat partition /= str) $
        show partition =/= show str
  where
    (=/=) = mismatchIn $ "partition for " ++ quoted str

-- | Check that two partitions are a valid solution for some pair of strings.
--
-- Note that this only checks if the partitions are valid between them, checking against the input
-- strings is left to `checkPartition`.
--
-- >>> checkCommonPartition (["ab", "b", "a"], ["a", "b", "ab"])
-- <BLANKLINE>
--
-- >>> checkCommonPartition (["ab"], [])
-- user error (mismatch in common partitions 'ab' and '': fromList [('a',1),('b',1)] != fromList [])
--
-- >>> checkCommonPartition (["ab", "a", "b"], ["aa", "bb"])
-- user error (mismatch in common partitions 'abab' and 'aabb': [a,ab,b] != [aa,bb])
--
-- >>> checkCommonPartition (["ab", "b", "b"], ["a", "b", "ab"])
-- user error (mismatch in common partitions 'abbb' and 'abab': fromList [('a',1),('b',3)] != fromList [('a',2),('b',2)])
checkCommonPartition :: Debug a => Pair (Partition a) -> IO ()
checkCommonPartition (p1, p2) = case p1 !? 0 <|> p2 !? 0 of
    -- this check only really makes sense for non-empty partitions
    Just Unboxed -> do
        let (s1, s2) = concat `both` (p1, p2)
        let (=/=) = mismatchIn $ "common partitions " ++ quoted s1 ++ " and " ++ quoted s2
        -- the partitions must have the same occurence of characters
        when (occurrences s1 /= occurrences s2) $
            show (occurrences s1) =/= show (occurrences s2)
        -- the partition must have the same substrings
        when (sort p1 /= sort p2) $
            show (sort p1) =/= show (sort p2)
    -- empty partitions are valid as a common partiton of the empty string
    Nothing -> pure ()

-- | Show value with a pair of single quotes surrounding it.
--
-- >>> quoted 12
-- "'12'"
quoted :: Show a => a -> Text.String
quoted value = "'" ++ show value ++ "'"

-- | Returns an error message describing where the algorithm is broken and which pair of values
-- should be the same.
--
-- >>> mismatchIn "someplace" "leftValue" "rightValue"
-- user error (mismatch in someplace: leftValue != rightValue)
mismatchIn :: Text.String -> Text.String -> Text.String -> IO never
mismatchIn location x y = fail $ "mismatch in " ++ location ++ ": " ++ x ++ " != " ++ y

-- | Returns the length of the foldables, if both have the same length, or throws an error.
--
-- >>> import Prelude (Char)
--
-- >>> checkedLen @[] @Int "list" ([1..4], [2..5])
-- 4
--
-- >>> checkedLen @String @Char "string" ("", "abc")
-- user error (length mismatch for string: 0 != 3)
checkedLen :: Text.String -> Foldable t => (t a, t a) -> IO Int
checkedLen name (x, y)
    | nx == ny = pure nx
    | otherwise = fail $ "length mismatch for " ++ name ++ ": " ++ show nx ++ " != " ++ show ny
  where
    nx = length x
    ny = length y

-- | Just the division of the two numbers, or `Nothing` if the divisor is zero.
--
-- >>> 1 `checkedDiv` 2
-- Just 0.5
--
-- >>> 2 `checkedDiv` 0
-- Nothing
checkedDiv :: (Integral a, Fractional b) => a -> a -> Maybe b
checkedDiv dividend divisor =
    if divisor /= 0
        then Just $ fromRational (toInteger dividend % toInteger divisor)
        else Nothing
