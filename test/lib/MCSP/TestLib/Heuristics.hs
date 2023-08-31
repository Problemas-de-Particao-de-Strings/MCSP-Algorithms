module MCSP.TestLib.Heuristics (
    MCSPHeuristic,
    StringParameters (..),
    genStringPair,
    testHeuristic,
) where

import Prelude hiding (String)

import MCSP.Data.String (String)
import MCSP.Data.String.Extra (PartitionPair)
import MCSP.System.Random (Random)
import MCSP.TestLib.Random (randomShuffledCharsWithSingletons)

-- | Heuristic for the MCSP problem.
type MCSPHeuristic a = String a -> String a -> PartitionPair a

-- | Parameters to generate a string of integers.
--
-- Alphabet size does not include the elements included as singletons.
data StringParameters = StringParameters
    { stringSize :: Int,
      alphabetSize :: Int,
      nSingletons :: Int
    }

-- | Generate a pair of strings of integers using the given parameters.
genStringPair :: StringParameters -> Random (String Int, String Int)
genStringPair params =
    randomShuffledCharsWithSingletons
        (stringSize params)
        1
        (alphabetSize params)
        [3 .. (2 + nSingletons params)]

-- | Test an heuristic against a sample and return
-- the mean length of the resulting partitions.
testHeuristic :: MCSPHeuristic a -> [(String a, String a)] -> Double
testHeuristic heuristic sample = fromIntegral (sum results) / fromIntegral (length sample)
  where
    results = map (length . fst . uncurry heuristic) sample
