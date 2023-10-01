-- | Tools for testing heuristics.
module MCSP.TestLib.Heuristics (
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
import Data.Ratio ((%))
import Data.String qualified as Text
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

import MCSP.Data.Pair (Pair, second, ($$))
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

import MCSP.TestLib.Heuristics.TH (mkNamed, mkNamedList)

-- | The heuristic with its defined name.
type NamedHeuristic a = (Text.String, Heuristic a)

-- | List of all heuristics implemented and their names.
heuristics :: Ord a => [NamedHeuristic a]
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
measure :: Debug a => NamedHeuristic a -> Pair (String a) -> Measured
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
    blks = checkedLen "blocks" (checked heuristic pair)
    chrs = checkedLen "size" pair
    sing = checkedLen "singletons" (singletons $$ pair)
    reps = checkedLen "repeated" (repeated $$ pair)
    x `divR` y = fromRational (toInteger x % toInteger y)

-- | A list of pairs @(columnName, showColumn)@ used to construct the CSV for `Measured`.
--
-- >>> map fst csvColumns
-- ["size","repeats","singles","heuristic","blocks","score"]
csvColumns :: [(Text.String, Measured -> Text.String)]
csvColumns =
    [ second (show .) $(mkNamed 'size),
      second (show .) $(mkNamed 'repeats),
      second (show .) $(mkNamed 'singles),
      $(mkNamed 'heuristic),
      second (show .) $(mkNamed 'blocks),
      second (show .) $(mkNamed 'score)
    ]

-- | The CSV header for the columns of `Measured`.
--
-- >>> csvHeader
-- "size,repeats,singles,heuristic,blocks,score"
csvHeader :: Text.String
csvHeader = intercalate "," (map fst csvColumns)

-- | The CSV row representing the values of a `Measured`.
--
-- >>> result = measure $(mkNamed 'combine) ("abcd", "cdab")
-- >>> toCsvRow result
-- "4,0,4,combine,2,0.6666666666666666"
toCsvRow :: Measured -> Text.String
toCsvRow measured = intercalate "," (map getValue csvColumns)
  where
    getValue (_, showColumn) = showColumn measured
