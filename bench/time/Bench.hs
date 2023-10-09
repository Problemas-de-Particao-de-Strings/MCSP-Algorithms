-- | Benchmark measuring running time of MCSP heuristics.
module Main (main) where

import Prelude hiding (String, head, lookup)

import Control.DeepSeq (NFData)
import Criterion.Main (bench, bgroup, defaultMainWith, perRunEnv)
import Criterion.Types (Benchmark, Config (..), Verbosity (Verbose))
import Data.List.NonEmpty (head)
import Data.String qualified as Text (String)
import Data.Word (Word8)

import MCSP.Data.MatchingGraph (edgeSet, solution, solutions)
import MCSP.Data.Pair (Pair)
import MCSP.Data.String (String)
import MCSP.System.Path (createDirectory, getCurrentTimestamp, packageRoot, (<.>), (</>))
import MCSP.System.Random (Random, generate, shuffle)
import MCSP.System.Statistics (cl99)
import MCSP.TestLib.Sample (benchParams, randomPairWith, repr)

-- | Generates a configuration for Criterion that saves the outputs by default.
--
-- >>> import Data.Maybe (fromJust)
-- >>> lastN n = reverse . take n . reverse
-- >>> lastN 74 <$> fromJust <$> reportFile <$> getDefaultConfig
-- "MCSP-Algorithms/bench/output/time-2023-09-06T00:20:21.292946511-03:00.html"
getDefaultConfig :: IO Config
getDefaultConfig = do
    let outputDir = packageRoot
    createDirectory outputDir
    timestamp <- getCurrentTimestamp
    pure
        Config
            { confInterval = cl99,
              timeLimit = 10,
              resamples = 1000,
              regressions = [],
              rawDataFile = Nothing,
              reportFile = Just (outputDir </> timestamp ++ "-time-report" <.> "html"),
              csvFile = Just (outputDir </> timestamp ++ "-time" <.> "csv"),
              jsonFile = Nothing,
              junitFile = Nothing,
              verbosity = Verbose,
              template = packageRoot </> "bench" </> "time" </> "report-template.tpl"
            }

-- | Run Criterion with a default configuration from `getDefaultConfig`.
defaultMain :: [Benchmark] -> IO ()
defaultMain benchmarks = do
    config <- getDefaultConfig
    defaultMainWith config benchmarks

-- | Creates a benchmark group running each heuristic against the given parameters.
benchWithParams ::
    (NFData a, NFData b) =>
    Text.String
    -> (a -> b)
    -> (Pair (String Word8) -> Random a)
    -> Benchmark
benchWithParams name run randomInput = bgroup name $ map benchWith benchParams
  where
    input params = generate (randomPairWith params >>= randomInput)
    benchWith params = bench (repr params) $ perRunEnv (input params) (pure . run)

-- | Run a matrix of benchmarks for each parameter set and heuristic.
main :: IO ()
main =
    defaultMain
        [ benchWithParams "solution" solution getEdges,
          benchWithParams "head solutions" (head . solutions) getEdges
        ]
  where
    getEdges pair = shuffle (edgeSet pair)
