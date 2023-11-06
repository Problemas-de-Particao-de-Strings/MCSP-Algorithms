-- | Heuristic for the MCSP problem.
module MCSP.Heuristics (
    -- * Main heuristics
    Heuristic,
    trivial,
    module MCSP.Heuristics.Combine,
    module MCSP.Heuristics.Greedy,
    module MCSP.Heuristics.PSOBased,
) where

import Control.Applicative (pure)

import MCSP.Data.Meta (Meta)
import MCSP.Data.Pair (Pair, both)
import MCSP.Data.String (String (..))
import MCSP.Data.String.Extra (Partition, chars)
import MCSP.Heuristics.Combine (UseSingletons (..), combine)
import MCSP.Heuristics.Greedy (greedy)
import MCSP.Heuristics.PSOBased (PSOIterations (..), PSOParticles (..), PSOSeed (..), pso)

-- | Heuristic for the MCSP problem.
type Heuristic a = Pair (String a) -> Meta (Pair (Partition a))

-- | The trivial solution for MCSP problem.
--
-- Just split each string into a partition of single characters.
--
-- >>> import MCSP.Data.Meta (evalMeta)
-- >>> evalMeta (trivial ("abba", "abab"))
-- ([a,b,b,a],[a,b,a,b])
trivial :: Heuristic a
trivial strs = pure (chars `both` strs)
{-# INLINE trivial #-}
