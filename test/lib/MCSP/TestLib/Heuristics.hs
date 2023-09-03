module MCSP.TestLib.Heuristics (
    Heuristic,
    heuristics,
) where

import Prelude hiding (String)

import Data.String qualified as Text

import MCSP.Heuristics (
    Heuristic,
    combine,
    combineS,
    greedy,
 )

import MCSP.TestLib.Heuristics.TH (mkNamedList)

-- | List of all heuristics implemented and their names.
heuristics :: Ord a => [(Text.String, Heuristic a)]
heuristics = $(mkNamedList ['combine, 'combineS, 'greedy])
