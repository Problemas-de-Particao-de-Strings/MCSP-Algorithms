-- | Heuristic for the MCSP problem.
module MCSP.Heuristics (
    -- * Main heuristics
    Heuristic,
    trivial,
    module MCSP.Heuristics.Combine,
    module MCSP.Heuristics.Greedy,
    module MCSP.Heuristics.PSOBased,

    -- * Correctness checking
    ErrorMessage,
    Debug,
    checked,
    checked',
    checkedIO,
) where

import Control.Applicative (pure, (<|>))
import Control.Monad (MonadFail, fail, void, when)
import Data.Either (Either (..), either)
import Data.Eq (Eq (..))
import Data.Function (id, ($), (.))
import Data.List (sort, (++))
import Data.List.Extra ((!?))
import Data.Maybe (Maybe (..))
import Data.Ord (Ord (..))
import Data.String qualified as Text (String)
import GHC.Err (error)
import GHC.Stack (HasCallStack)
import Text.Show (Show, show)

import MCSP.Data.Pair (Pair, both, liftP, zipM)
import MCSP.Data.String (ShowString, String (..), concat)
import MCSP.Data.String.Extra (Partition, alphabet, chars, occurrences)
import MCSP.Heuristics.Combine (combine, combineS)
import MCSP.Heuristics.Greedy (greedy)
import MCSP.Heuristics.PSOBased (pso)

-- | Heuristic for the MCSP problem.
type Heuristic a = Pair (String a) -> Pair (Partition a)

-- | The trivial solution for MCSP problem.
--
-- Just split each string into a partition of single characters.
--
-- >>> trivial ("abba", "abab")
-- ([a,b,b,a],[a,b,a,b])
trivial :: Heuristic a
trivial = both chars
{-# INLINE trivial #-}

-- | Message describing the error found with the solution.
type ErrorMessage = Text.String

-- | Common constraints for debugging a heuristic.
type Debug a = (Show a, ShowString a, Ord a)

-- | Run a heuristic checking the inputs and output for correctnes.
--
-- >>> checked trivial ("abba", "abab")
-- Right ([a,b,b,a],[a,b,a,b])
-- >>> checked trivial ("abba", "abad")
-- Left "mismatch in input strings 'abba' and 'abad': fromList \"ab\" != fromList \"abd\""
checked :: Debug a => Heuristic a -> Pair (String a) -> Either ErrorMessage (Pair (Partition a))
checked heuristic pair = do
    checkBalanced pair
    let partitions = heuristic pair
    void $ zipM $ liftP checkPartition pair partitions
    checkCommonPartition partitions
    pure partitions

-- | Run a heuristic checking the inputs and output for correctnes.
--
-- Partial version of `checked`.
checked' :: (HasCallStack, Debug a) => Heuristic a -> Heuristic a
checked' heuristic = either error id . checked heuristic

-- | Run a heuristic checking the inputs and output for correctnes.
--
-- Monadic version of `checked`.
checkedIO :: (MonadFail m, Debug a) => Heuristic a -> Pair (String a) -> m (Pair (Partition a))
checkedIO heuristic = either fail pure . checked heuristic

-- | Check that the input strings are balanced.
--
-- >>> checkBalanced ("abba", "abab")
-- Right ()
--
-- >>> checkBalanced ("abba", "abad")
-- Left "mismatch in input strings 'abba' and 'abad': fromList \"ab\" != fromList \"abd\""
--
-- >>> checkBalanced ("abba", "abbb")
-- Left "mismatch in input strings 'abba' and 'abbb': fromList [('a',2),('b',2)] != fromList [('a',1),('b',3)]"
checkBalanced :: Debug a => Pair (String a) -> Either ErrorMessage ()
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
-- Right ()
--
-- >>> checkPartition "abba" ["ab", "a", "b"]
-- Left "mismatch in partition for 'abba': [ab,a,b] != abba"
--
-- >>> checkPartition "abba" ["ab", "bb"]
-- Left "mismatch in partition for 'abba': fromList [('a',1),('b',3)] != fromList [('a',2),('b',2)]"
checkPartition :: Debug a => String a -> [String a] -> Either ErrorMessage ()
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
-- Right ()
--
-- >>> checkCommonPartition (["ab"], [])
-- Left "mismatch in common partitions 'ab' and '': fromList [('a',1),('b',1)] != fromList []"
--
-- >>> checkCommonPartition (["ab", "a", "b"], ["aa", "bb"])
-- Left "mismatch in common partitions 'abab' and 'aabb': [a,ab,b] != [aa,bb]"
--
-- >>> checkCommonPartition (["ab", "b", "b"], ["a", "b", "ab"])
-- Left "mismatch in common partitions 'abbb' and 'abab': fromList [('a',1),('b',3)] != fromList [('a',2),('b',2)]"
checkCommonPartition :: Debug a => Pair (Partition a) -> Either ErrorMessage ()
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
-- Left "mismatch in someplace: leftValue != rightValue"
mismatchIn :: Text.String -> Text.String -> Text.String -> Either ErrorMessage never
mismatchIn location x y = Left $ "mismatch in " ++ location ++ ": " ++ x ++ " != " ++ y
