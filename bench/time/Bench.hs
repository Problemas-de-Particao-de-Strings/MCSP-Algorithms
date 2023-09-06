-- | Benchmark measuring running time of MCSP heuristics.
module Main (main) where

import Prelude

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, perRunEnv)
import Data.Word (Word8)

import MCSP.System.Random (generate)
import MCSP.TestLib.Heuristics (Heuristic, heuristics)
import MCSP.TestLib.Sample (StringParameters, benchParams, randomPairWith, repr)

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
