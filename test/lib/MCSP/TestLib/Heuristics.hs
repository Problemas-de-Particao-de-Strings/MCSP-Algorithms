module MCSP.TestLib.Heuristics (
    Heuristic,
    heuristics,
    Measured (..),
    measure,
) where

import Prelude hiding (String)

import Control.DeepSeq (NFData)
import Data.String qualified as Text
import GHC.Generics (Generic)

import MCSP.Data.String (String)
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

-- | A collection of information made on a single execution of a `Heuristic`.
data Measured = Measured
    { -- | The name of the heuristic run.
      heuristic :: Text.String,
      -- | Length of the input strings.
      size :: Int,
      -- | Number of substrings in the output partition.
      blocks :: Int,
      -- | A score of the solution, given by @1 - (`blocks` - 1) / (`size` - 1)@
      score :: Double
    }
    deriving stock (Show, Eq, Generic)

instance NFData Measured

-- | Run the heuristic and returns information about the solution.
--
-- >>> import MCSP.TestLib.Heuristics.TH (mkNamed)
-- >>> measure $(mkNamed 'combine) ("abcd", "cdab")
-- Measured {heuristic = "combine", size = 4, blocks = 2, score = 0.5}
-- >>> import MCSP.Data.String.Extra (chars)
-- >>> trivial x y = (chars x, chars y)
-- >>> measure $(mkNamed 'trivial) ("abcd", "cdab")
-- Measured {heuristic = "trivial", size = 4, blocks = 4, score = 0.0}
measure :: (Text.String, Heuristic a) -> (String a, String a) -> Measured
measure (name, heuristic) pair =
    Measured
        { heuristic = name,
          size = lenBy max pair,
          blocks = lenBy max parts,
          score = 1 - (lenBy (+) parts - 1) / (lenBy (+) pair - 1)
        }
  where
    parts = uncurry heuristic pair
    lenBy join (x, y) = fromIntegral (length x `join` length y)
