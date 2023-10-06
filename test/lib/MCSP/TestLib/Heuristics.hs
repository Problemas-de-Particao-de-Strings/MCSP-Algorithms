-- | Tools for testing heuristics.
module MCSP.TestLib.Heuristics (
    Heuristic,
    NamedHeuristic,
    heuristics,
    Measured (..),
    randomSeed,
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
import Numeric (showHex)

import MCSP.Data.Pair (Pair, both, second)
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

import MCSP.System.Random (Random, Seed, generate, generateWith, uniform)
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
      repeats :: Int,
      -- | Seed used to generate the input strings.
      seed :: Seed
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

-- | Generate a new random seed.
randomSeed :: IO Seed
randomSeed = generate uniform

-- | Run the heuristic and returns information about the solution.
--
-- >>> import MCSP.TestLib.Heuristics.TH (mkNamed)
-- >>> measure $(mkNamed 'combine) (0, 0) (pure ("abcd", "cdab"))
-- Measured {heuristic = "combine", size = 4, blocks = 2, score = 0.6666666666666666, singles = 4, repeats = 0, seed = (0,0)}
-- >>> import MCSP.Data.String.Extra (chars)
-- >>> trivial = both chars
-- >>> measure $(mkNamed 'trivial) (0, 0) (pure ("abcd", "cdab"))
-- Measured {heuristic = "trivial", size = 4, blocks = 4, score = 0.0, singles = 4, repeats = 0, seed = (0,0)}
measure :: Debug a => NamedHeuristic a -> Seed -> Random (Pair (String a)) -> Measured
measure (name, heuristic) seed genPair =
    let pair = generateWith seed genPair
        size = checkedLen "size" pair
        blocks = checkedLen "blocks" (checked heuristic pair)
        score = (size - blocks) `divR` (size - 1)
        singles = checkedLen "singletons" (singletons `both` pair)
        repeats = checkedLen "repeated" (repeated `both` pair)
     in Measured {heuristic = name, blocks, size, score, singles, repeats, seed}
  where
    x `divR` y = fromRational (toInteger x % toInteger y)

-- | A list of pairs @(columnName, showColumn)@ used to construct the CSV for `Measured`.
--
-- >>> map fst csvColumns
-- ["size","repeats","singles","heuristic","blocks","score","seed"]
csvColumns :: [(Text.String, Measured -> Text.String)]
csvColumns =
    [ second (show .) $(mkNamed 'size),
      second (show .) $(mkNamed 'repeats),
      second (show .) $(mkNamed 'singles),
      $(mkNamed 'heuristic),
      second (show .) $(mkNamed 'blocks),
      second (show .) $(mkNamed 'score),
      second (showSeed .) $(mkNamed 'seed)
    ]
  where
    showSeed (x, y) = showHex x " " ++ showHex y ""

-- | The CSV header for the columns of `Measured`.
--
-- >>> csvHeader
-- "size,repeats,singles,heuristic,blocks,score,seed"
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
