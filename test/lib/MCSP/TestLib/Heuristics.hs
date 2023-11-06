-- | Tools for testing heuristics.
module MCSP.TestLib.Heuristics (
    Debug,
    Heuristic,
    NamedHeuristic,
    heuristics,
    Measured (..),
    measure,
    csvHeader,
    toCsvRow,
) where

import Prelude hiding (String)

import Control.DeepSeq (NFData)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.String qualified as Text
import Data.Vector.Generic qualified as Vector (length)
import GHC.Generics (Generic)
import Numeric.Extra (showDP)
import Safe.Foldable (maximumBound)

import MCSP.Data.MatchingGraph (edgeSet)
import MCSP.Data.Pair (Pair, both, second)
import MCSP.Data.String (String)
import MCSP.Data.String.Extra (occurrences, repeated, singletons)
import MCSP.Heuristics (Heuristic, combine, combineS, greedy, pso)
import MCSP.System.TimeIt (timeIt)
import MCSP.TestLib.Heuristics.Safe (Debug, checkedDiv, checkedLen, runChecked)
import MCSP.TestLib.Heuristics.TH (mkNamed, mkNamedList)

-- | The heuristic with its defined name.
type NamedHeuristic a = (Text.String, Heuristic a)

-- | List of all heuristics implemented and their names.
heuristics :: Ord a => [NamedHeuristic a]
heuristics = $(mkNamedList ['combine, 'combineS, 'greedy, 'pso])

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
      -- | Execution time in seconds.
      time :: Double,
      -- | Number of unique characters in the input pair or strings.
      singles :: Int,
      -- | Number of distinct non-singletons in the input strings.
      repeats :: Int,
      -- | Maximum occurence of a character in the input strings.
      maxRepeat :: Int,
      -- | Number of edges of the graph representation of the strings.
      edges :: Int,
      -- | The partition found for the left string.
      left :: Text.String,
      -- | The partition found for the right string.
      right :: Text.String
    }
    deriving stock (Show, Eq, Generic)

instance NFData Measured

-- | Run the heuristic and returns information about the solution.
--
-- >>> import MCSP.TestLib.Heuristics.TH (mkNamed)
-- >>> result <- measure $(mkNamed 'combine) ("abcd", "cdab")
-- >>> result { time = -1 }
-- Measured {heuristic = "combine", size = 4, blocks = 2, score = 0.6666666666666666, time = -1.0, singles = 4, repeats = 0, maxRepeat = 1, edges = 2, left = "[ab,cd]", right = "[cd,ab]"}
--
-- >>> import MCSP.Heuristics (trivial)
-- >>> result <- measure $(mkNamed 'trivial) ("abcd", "cdab")
-- >>> result { time = -1 }
-- Measured {heuristic = "trivial", size = 4, blocks = 4, score = 0.0, time = -1.0, singles = 4, repeats = 0, maxRepeat = 1, edges = 2, left = "[a,b,c,d]", right = "[c,d,a,b]"}
measure :: Debug a => NamedHeuristic a -> Pair (String a) -> IO Measured
measure (name, heuristic) pair = do
    (timePs, partitions) <- timeIt (runChecked heuristic) pair

    size <- checkedLen "size" pair
    blocks <- checkedLen "blocks" partitions
    let score = fromMaybe 1 $ (size - blocks) `checkedDiv` (size - 1)
    let time = fromRational $ toRational timePs
    singles <- checkedLen "singletons" (singletons `both` pair)
    repeats <- checkedLen "repeated" (repeated `both` pair)
    let maxRepeat = maximumBound 0 (occurrences $ fst pair)
    let edges = Vector.length (edgeSet pair)
    let (left, right) = show `both` partitions
    pure
        Measured
            { heuristic = name,
              blocks,
              size,
              score,
              time,
              singles,
              repeats,
              maxRepeat,
              edges,
              left,
              right
            }

-- | A list of pairs @(columnName, showColumn)@ used to construct the CSV for `Measured`.
--
-- >>> map fst csvColumns
-- ["size","repeats","singles","heuristic","blocks","score","time","left","right"]
csvColumns :: [(Text.String, Measured -> Text.String)]
csvColumns =
    [ second (showColumn 3 show) $(mkNamed 'size),
      second (showColumn 2 show) $(mkNamed 'repeats),
      second (showColumn 2 show) $(mkNamed 'singles),
      second (showColumn 3 show) $(mkNamed 'maxRepeat),
      second (showColumn 3 show) $(mkNamed 'edges),
      second (showColumn 8 id) $(mkNamed 'heuristic),
      second (showColumn 3 show) $(mkNamed 'blocks),
      second (showColumn 4 (showDP 2)) $(mkNamed 'score),
      second (showColumn 8 (showDP 3)) $(mkNamed 'time),
      second (showColumn 0 show) $(mkNamed 'left),
      second (showColumn 0 show) $(mkNamed 'right)
    ]

-- Compose a function that show a value with one that extracts that value from a measurement.
--
-- Also has a parameter for padding the resulting text if it is too short.
--
-- >>> showColumn 10 show id 12
-- "        12"
showColumn :: Int -> (b -> Text.String) -> (a -> b) -> (a -> Text.String)
showColumn minWidth showIt = ((padLeft . showIt) .)
  where
    padLeft str = replicate (minWidth - length str) ' ' ++ str

-- | The CSV header for the columns of `Measured`.
--
-- >>> csvHeader
-- "size,repeats,singles,heuristic,blocks,score,time,left,right"
csvHeader :: Text.String
csvHeader = intercalate "," (map fst csvColumns)

-- | The CSV row representing the values of a `Measured`.
--
-- >>> result <- measure $(mkNamed 'combine) ("abcd", "cdab")
-- >>> toCsvRow result
-- "  4, 0, 4, combine,  2,0.67,   0.000,\"[ab,cd]\",\"[cd,ab]\""
toCsvRow :: Measured -> Text.String
toCsvRow measured = intercalate "," (map getValue csvColumns)
  where
    getValue (_, showIt) = showIt measured
