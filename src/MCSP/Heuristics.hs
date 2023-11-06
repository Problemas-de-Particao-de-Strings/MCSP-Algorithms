-- | Heuristic for the MCSP problem.
module MCSP.Heuristics (
    -- * Main heuristics
    Heuristic,
    trivial,
    module MCSP.Heuristics.Combine,
    module MCSP.Heuristics.Greedy,
    module MCSP.Heuristics.PSOBased,
) where

import MCSP.Data.Pair (Pair, both)
import MCSP.Data.String (String (..))
import MCSP.Data.String.Extra (Partition, chars)
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
