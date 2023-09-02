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
 )

-- | Test an heuristic against a sample and return the mean length of the resulting partitions.
testHeuristic :: Heuristic a -> [(String a, String a)] -> Double
testHeuristic heuristic sample = fromIntegral (sum results) / fromIntegral (length sample)
  where
    results = map (length . fst . uncurry heuristic) sample

heuristics :: Ord a => [(Text.String, Heuristic a)]
heuristics =
    [ ("combine", combine),
      ("combineS", combineS)
    ]
