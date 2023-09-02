module MCSP.TestLib.Heuristics (
    testHeuristic,
) where

import Prelude hiding (String)

import MCSP.Data.String (String)
import MCSP.Heuristics (Heuristic)

-- | Test an heuristic against a sample and return the mean length of the resulting partitions.
testHeuristic :: Heuristic a -> [(String a, String a)] -> Double
testHeuristic heuristic sample = fromIntegral (sum results) / fromIntegral (length sample)
  where
    results = map (length . fst . uncurry heuristic) sample
