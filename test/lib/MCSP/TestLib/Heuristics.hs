-- | Tools for testing heuristics.
module MCSP.TestLib.Heuristics (
    Heuristic,
    heuristics,
    Measured (..),
    measure,
) where

import Prelude hiding (String)

import Control.DeepSeq (NFData)
import Data.Ratio ((%))
import Data.String qualified as Text
import Data.Tuple.Extra (both)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

import MCSP.Data.String (String)
import MCSP.Data.String.Extra (repeated, singletons)
import MCSP.Heuristics (
    Debug,
    Heuristic,
    checked,
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
      score :: Double,
      -- | Number of unique characters in the input pair or strings.
      singles :: Int,
      -- | Number of non-singletons in the input strings.
      repeats :: Int
    }
    deriving stock (Show, Eq, Generic)

instance NFData Measured

-- | Returns the length of the foldables, if both have the same length, or throws an error.
--
-- >>> checkedLen "list" ([1..4], [2..5] :: [Int])
-- 4
-- >>> checkedLen "string" ("", "abc" :: String Char)
-- length mismatch for string: 0 != 3
checkedLen :: HasCallStack => Text.String -> Foldable t => (t a, t a) -> Int
checkedLen name (x, y)
    | nx == ny = nx
    | otherwise = error $ "length mismatch for " ++ name ++ ": " ++ show nx ++ " != " ++ show ny
  where
    nx = length x
    ny = length y

-- | Run the heuristic and returns information about the solution.
--
-- >>> import MCSP.TestLib.Heuristics.TH (mkNamed)
-- >>> measure $(mkNamed 'combine) ("abcd", "cdab")
-- Measured {heuristic = "combine", size = 4, blocks = 2, score = 0.6666666666666666, singles = 4, repeats = 0}
-- >>> import MCSP.Data.String.Extra (chars)
-- >>> trivial x y = (chars x, chars y)
-- >>> measure $(mkNamed 'trivial) ("abcd", "cdab")
-- Measured {heuristic = "trivial", size = 4, blocks = 4, score = 0.0, singles = 4, repeats = 0}
measure :: Debug a => (Text.String, Heuristic a) -> (String a, String a) -> Measured
measure (name, heuristic) pair =
    Measured
        { heuristic = name,
          size = chrs,
          blocks = blks,
          score = (chrs - blks) `divR` (chrs - 1),
          singles = sing,
          repeats = reps
        }
  where
    blks = checkedLen "blocks" (uncurry (checked heuristic) pair)
    chrs = checkedLen "size" pair
    sing = checkedLen "singletons" (both singletons pair)
    reps = checkedLen "repeated" (both repeated pair)
    x `divR` y = fromRational (toInteger x % toInteger y)
