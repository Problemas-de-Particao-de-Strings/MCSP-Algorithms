-- | Benchmark measuring running time of MCSP heuristics.
module Main (main) where

import Prelude

import Criterion.Main (bench, bgroup, defaultMainWith, perRunEnv)
import Criterion.Types (Benchmark, Config (..), Verbosity (Verbose))
import Data.Word (Word8)
import Statistics.Types (cl95)

import MCSP.System.Path (createDirectory, directory, getCurrentTimestamp, thisFile, (<.>), (</>))
import MCSP.System.Random (generate)
import MCSP.TestLib.Heuristics (Heuristic, heuristics)
import MCSP.TestLib.Sample (StringParameters, benchParams, randomPairWith, repr)

-- | Path to directory where benchmarks outputs.
--
-- >>> lastN n = reverse . take n . reverse
-- >>> lastN 28 outputDir
-- "MCSP-Algorithms/bench/output"
outputDir :: FilePath
outputDir = directory (directory $$thisFile) </> "output"

-- | Generates a configuration for Criterion that saves the outputs by default.
--
-- >>> import Data.Maybe (fromJust)
-- >>> lastN n = reverse . take n . reverse
-- >>> lastN 74 <$> fromJust <$> reportFile <$> getDefaultConfig
-- "MCSP-Algorithms/bench/output/time-2023-09-06T00:20:21.292946511-03:00.html"
getDefaultConfig :: IO Config
getDefaultConfig = do
    createDirectory outputDir
    timestamp <- getCurrentTimestamp
    pure
        Config
            { confInterval = cl95,
              timeLimit = 10,
              resamples = 1000,
              regressions = [],
              rawDataFile = Nothing,
              reportFile = Just (outputDir </> timestamp ++ "-time-report" <.> "html"),
              csvFile = Just (outputDir </> timestamp ++ "-time" <.> "csv"),
              jsonFile = Nothing,
              junitFile = Nothing,
              verbosity = Verbose,
              template = "default"
            }

-- | Run Criterion with a default configuration from `getDefaultConfig`.
defaultMain :: [Benchmark] -> IO ()
defaultMain benchmarks = do
    config <- getDefaultConfig
    defaultMainWith config benchmarks

-- | Character type used for benchmarking.
type Target = Word8

-- | Create a benchmark for a single heuristic, generating an input string pair for each run.
benchHeuristic :: StringParameters -> (String, Heuristic Target) -> Benchmark
benchHeuristic params (name, heuristic) = bench name $ perRunEnv genPair runHeuristic
  where
    genPair = generate (randomPairWith params)
    runHeuristic = pure . uncurry heuristic

-- | Creates a benchmark group running each heuristic against the given parameters.
benchWithParams :: StringParameters -> Benchmark
benchWithParams params = bgroup (repr params) $ map (benchHeuristic params) heuristics

-- | Run a matrix of benchmarks for each parameter set and heuristic.
main :: IO ()
main = defaultMain (map benchWithParams benchParams)
