-- | Benchmark measuring running time of MCSP heuristics.
module Main (main) where

import Prelude

import Criterion.Main (bench, bgroup, defaultMainWith, perRunEnv)
import Criterion.Types (Benchmark, Config (..), Verbosity (Verbose))
import Data.Word (Word8)

import MCSP.System.Path (createDirectory, getCurrentTimestamp, packageRoot, (<.>), (</>))
import MCSP.System.Random (generate)
import MCSP.System.Statistics (cl95)
import MCSP.TestLib.Heuristics (NamedHeuristic, heuristics)
import MCSP.TestLib.Sample (SimpleEnum, StringParameters, benchParams, randomPairWith, repr)

-- | Generates a configuration for Criterion that saves the outputs by default.
--
-- >>> import Data.Maybe (fromJust)
-- >>> lastN n = reverse . take n . reverse
-- >>> lastN 74 <$> fromJust <$> reportFile <$> getDefaultConfig
-- "MCSP-Algorithms/bench/output/time-2023-09-06T00:20:21.292946511-03:00.html"
getDefaultConfig :: IO Config
getDefaultConfig = do
    let outputDir = packageRoot </> "bench" </> "output"
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
              template = packageRoot </> "bench" </> "time" </> "report-template.tpl"
            }

-- | Run Criterion with a default configuration from `getDefaultConfig`.
defaultMain :: [Benchmark] -> IO ()
defaultMain benchmarks = do
    config <- getDefaultConfig
    defaultMainWith config benchmarks

-- | Create a benchmark for a single heuristic, generating an input string pair for each run.
benchHeuristic :: SimpleEnum a => StringParameters -> NamedHeuristic a -> Benchmark
benchHeuristic params (name, heuristic) = bench name $ perRunEnv genPair (pure . heuristic)
  where
    genPair = generate (randomPairWith params)

-- | Creates a benchmark group running each heuristic against the given parameters.
benchWithParams :: StringParameters -> Benchmark
benchWithParams params = bgroup (repr params) $ map (benchHeuristic @Word8 params) heuristics

-- | Run a matrix of benchmarks for each parameter set and heuristic.
main :: IO ()
main = defaultMain (map benchWithParams benchParams)
