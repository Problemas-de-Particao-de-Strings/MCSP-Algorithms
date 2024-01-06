-- | Tools for testing heuristics.
module MCSP.TestLib.Heuristics (
    -- * Standard Heuristics
    Debug,
    Heuristic,
    NamedHeuristic,
    heuristics,

    -- * Measuring Heuristic input and output
    Measured (..),
    scoreFrom,
    measure,
) where

import Prelude hiding (String, lookup)

import Control.DeepSeq (NFData (..))
import Data.Maybe (fromMaybe)
import Data.String qualified as Text
import Data.Vector.Generic qualified as Vector (length)
import GHC.Generics (Generic)
import Numeric.Extra (showDP)
import Safe.Foldable (maximumBound)

import MCSP.Data.MatchingGraph (edgeSet)
import MCSP.Data.Meta (lookup, (<::))
import MCSP.Data.Pair (Pair, both)
import MCSP.Data.String (String)
import MCSP.Data.String.Extra (occurrences, repeated, singletons)
import MCSP.Heuristics (Heuristic, UseSingletons (..), combine, greedy, pso)
import MCSP.Heuristics.PSOBased (PSOCombine (..), PSOPure (..), getFirstBestIter)
import MCSP.System.TimeIt (timeIt)
import MCSP.TestLib.Heuristics.Safe (Debug, checkedDiv, checkedLen, runChecked)
import MCSP.Text.CSV (CSVData (..), mkColumn, readColumn, readColumnWith, strColumn)
import MCSP.Text.ReadP (eof, readP, (<++))

-- | The heuristic with its defined name.
type NamedHeuristic a = (Text.String, Heuristic a)

-- | List of all heuristics implemented and their names.
heuristics :: Ord a => [NamedHeuristic a]
heuristics =
    [ ("combine", \s -> combine s <:: UseSingletons False),
      ("combineS", \s -> combine s <:: UseSingletons True),
      ("greedy", greedy),
      ("pso", pso),
      ("psoPure", \s -> pso s <:: PSOPure True),
      ("psoComb", \s -> pso s <:: PSOCombine True)
    ]

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
      -- | First PSO iteration with same result as the last iteration.
      psoIter :: Maybe Int,
      -- | The partition found for the left string.
      left :: Text.String,
      -- | The partition found for the right string.
      right :: Text.String,
      -- | The input strings.
      pair :: Text.String
    }
    deriving stock (Show, Eq, Generic)

instance NFData Measured

-- | A special type that "implements" `NFData`, but doesn't force any kind of evaluation.
newtype Lazy a = Lazy a

instance NFData (Lazy a) where
    rnf _ = ()

-- | Calculates the score for a given string size and number of output blocks.
scoreFrom :: Int -> Int -> Double
scoreFrom size blocks = fromMaybe 1 $ (size - blocks) `checkedDiv` (size - 1)

-- | Run the heuristic and returns information about the solution.
--
-- >>> import MCSP.TestLib.Heuristics.TH (mkNamed)
-- >>> result <- measure $(mkNamed 'combine) ("abcd", "cdab")
-- >>> result { time = -1 }
-- Measured {heuristic = "combine", size = 4, blocks = 2, score = 0.6666666666666666, time = -1.0, singles = 4, repeats = 0, maxRepeat = 1, edges = 2, psoIter = Nothing, left = "[ab,cd]", right = "[cd,ab]", pair = "(abcd,cdab)"}
--
-- >>> import MCSP.Heuristics (trivial)
-- >>> result <- measure $(mkNamed 'trivial) ("abcd", "cdab")
-- >>> result { time = -1 }
-- Measured {heuristic = "trivial", size = 4, blocks = 4, score = 0.0, time = -1.0, singles = 4, repeats = 0, maxRepeat = 1, edges = 2, psoIter = Nothing, left = "[a,b,c,d]", right = "[c,d,a,b]", pair = "(abcd,cdab)"}
measure :: Debug a => NamedHeuristic a -> Pair (String a) -> IO Measured
measure (name, heuristic) pair = do
    (timePs, (partitions, Lazy vars)) <- timeIt pair $ \strs -> do
        (solution, vars) <- runChecked heuristic strs
        pure (solution, Lazy vars)

    size <- checkedLen "size" pair
    blocks <- checkedLen "blocks" partitions
    let score = scoreFrom size blocks
    let time = fromRational $ toRational timePs
    singles <- checkedLen "singletons" (singletons `both` pair)
    repeats <- checkedLen "repeated" (repeated `both` pair)
    let maxRepeat = maximumBound 0 (occurrences $ fst pair)
    let edges = Vector.length (edgeSet pair)
    let psoIter = getFirstBestIter <$> lookup vars
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
              psoIter,
              left,
              right,
              pair = show pair
            }

instance CSVData Measured where
    writer =
        $$(mkColumn [||size||] 3 [||show||])
            <> $$(mkColumn [||repeats||] 2 [||show||])
            <> $$(mkColumn [||singles||] 2 [||show||])
            <> $$(mkColumn [||maxRepeat||] 3 [||show||])
            <> $$(mkColumn [||edges||] 3 [||show||])
            <> $$(mkColumn [||psoIter||] 3 [||maybe "" show||])
            <> $$(mkColumn [||heuristic||] 8 [||id||])
            <> $$(mkColumn [||blocks||] 3 [||show||])
            <> $$(mkColumn [||score||] 4 [||showDP 2||])
            <> $$(mkColumn [||time||] 8 [||showDP 3||])
            <> $$(mkColumn [||left||] 0 [||id||])
            <> $$(mkColumn [||right||] 0 [||id||])
            <> $$(mkColumn [||pair||] 0 [||id||])
    parser = do
        heuristic <- strColumn "heuristic"
        blocks <- readColumn "blocks"
        size <- readColumn "size"
        score <- readColumn "score"
        time <- readColumn "time"
        singles <- readColumn "singles"
        repeats <- readColumn "repeats"
        maxRepeat <- readColumn "maxRepeat"
        edges <- readColumn "edges"
        psoIter <- readColumnWith "psoIter" (readNothing <++ readJust)
        left <- strColumn "left"
        right <- strColumn "right"
        pair <- strColumn "pair"
        pure Measured {..}
      where
        readNothing = eof >> pure Nothing
        readJust = Just <$> readP
