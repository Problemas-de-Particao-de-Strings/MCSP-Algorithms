-- | Heuristic for the MCSP problem.
module MCSP.Heuristics (
    -- * Main heuristics
    Heuristic,
    module MCSP.Heuristics.Combine,
    module MCSP.Heuristics.Greedy,

    -- * Correctness checking
    Debug,
    checked,
) where

import Data.Bool (otherwise)
import Data.Eq (Eq (..))
import Data.Function (($), (.))
import Data.List (sort, (++))
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (fromJust)
import Data.Ord (Ord (..))
import Data.String qualified as Text
import Data.Tuple.Extra (both)
import GHC.Err (error)
import GHC.Stack (HasCallStack)
import Text.Show (Show, show)

import MCSP.Data.String (ShowString, String (..), concat, concatNE)
import MCSP.Data.String.Extra (Partition, frequency)
import MCSP.Heuristics.Combine (combine, combineS)
import MCSP.Heuristics.Greedy (greedy)

-- | Heuristic for the MCSP problem.
type Heuristic a = String a -> String a -> (Partition a, Partition a)

-- | Common constraints for debugging a heuristic.
type Debug a = (HasCallStack, Show a, ShowString a, Ord a)

-- | Transform a heuristic into one that checks the output for correctnes, but not the inputs.
checked :: Debug a => Heuristic a -> Heuristic a
checked h sx sy =
    let (px, py) = h sx sy
        px' = checkPart sx px
        py' = checkPart sy py
     in checkCommonPart px' py'

-- | Check that the partition is valid and returns it.
checkPart :: Debug a => String a -> [String a] -> [String a]
checkPart s@Unboxed p
    -- a valid partition must have the same substrings in the same order
    | concat p == s = p
    -- otherwise, check if at least nothing was left out and all the characters are there
    | f1 == f2 = mismatch header p s
    -- if some characters were erased, show another error message
    | otherwise = mismatch header f1 f2
  where
    header = "partition for '" ++ show s ++ "'"
    (f1, f2) = (frequency s, frequency $ concat p)

-- | Check that two partitions are of the same initial string.
checkCommonPart :: Debug a => Partition a -> Partition a -> (Partition a, Partition a)
checkCommonPart p1 p2
    -- each substring in must have an identical one in the other partition
    | sort p1 == sort p2 = (p1, p2)
    -- if they don't match, check if at least the character frequency is okay
    | f1 == f2 = mismatch header (sort p1) (sort p2)
    -- if not, show a different error message
    | otherwise = mismatch header f1 f2
  where
    header = "partitions for '" ++ show s1 ++ "' and '" ++ show s2 ++ "'"
    (s1, s2) = both (concatNE . fromJust . nonEmpty) (p1, p2)
    (f1, f2) = (frequency s1, frequency s2)

-- | Stop execution and display an error message specific for `checked`.
mismatch :: (HasCallStack, Show a, Show b) => Text.String -> a -> b -> never
mismatch loc x y = error $ "mismatch in " ++ loc ++ ": " ++ show x ++ " != " ++ show y
