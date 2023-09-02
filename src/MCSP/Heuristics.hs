-- | Heuristic for the MCSP problem.
module MCSP.Heuristics (
    Heuristic,
    module MCSP.Heuristics.Combine,
) where

import MCSP.Data.String (String)
import MCSP.Data.String.Extra (Partition)
import MCSP.Heuristics.Combine (combine, combineS)

-- | Heuristic for the MCSP problem.
type Heuristic a = String a -> String a -> (Partition a, Partition a)
