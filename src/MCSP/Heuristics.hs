-- | Heuristic for the MCSP problem.
module MCSP.Heuristics (
    Heuristic,
    module MCSP.Heuristics.Combine,
    module MCSP.Heuristics.Greedy,
) where

import MCSP.Data.String (String)
import MCSP.Data.String.Extra (Partition)
import MCSP.Heuristics.Combine (combine, combineS)
import MCSP.Heuristics.Greedy (greedy)

-- | Heuristic for the MCSP problem.
type Heuristic a = String a -> String a -> (Partition a, Partition a)
