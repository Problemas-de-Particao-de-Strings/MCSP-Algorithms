module MCSP.TestLib.Heuristics (
    Heuristic,
    testHeuristic,
    heuristics,
) where

import Prelude hiding (String)

import Data.String qualified as Text

import MCSP.Data.String (String)
import MCSP.Heuristics (
    Heuristic,
    combine,
    combineS,
    greedy,
 )

import MCSP.TestLib.Heuristics.TH (mkNamedList)

-- | Test an heuristic against a sample and return the mean length of the resulting partitions.
testHeuristic :: Heuristic a -> [(String a, String a)] -> Double
testHeuristic heuristic sample = fromIntegral (sum results) / fromIntegral (length sample)
  where
    results = map (length . fst . uncurry heuristic) sample

-- | List of all heuristics implemented and their names.
heuristics :: Ord a => [(Text.String, Heuristic a)]
heuristics = $(mkNamedList ['combine, 'combineS, 'greedy])
