-- | Benchmark measuring running time of MCSP heuristics.
module Main (main) where

import Prelude hiding (String)

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, perRunEnv)
import Data.String qualified as Text

import MCSP.Data.String (String)
import MCSP.System.Random (generate)
import MCSP.TestLib.Heuristics (Heuristic, heuristics)
import MCSP.TestLib.Sample (StringParameters, benchParams, randomPairWith, repr)

-- | Create a benchmark for a single heuristic, generating an input string pair for each run.
benchHeuristic :: IO (String a, String a) -> (Text.String, Heuristic a) -> Benchmark
benchHeuristic genPair (name, heuristic) = bench name $ perRunEnv genPair runHeuristic
  where
    runHeuristic = pure . uncurry heuristic

-- | String type used for benchmarking.
type Target = String Word

-- | Creates a benchmark group running each heuristic against the given parameters.
benchWithParams :: StringParameters -> Benchmark
benchWithParams params = bgroup (repr params) $ map (benchHeuristic genPair) heuristics
  where
    genPair :: IO (Target, Target) = generate (randomPairWith params)

-- | Run a matrix of benchmarks for each parameter set and heuristic.
main :: IO ()
main = defaultMain (map benchWithParams benchParams)
