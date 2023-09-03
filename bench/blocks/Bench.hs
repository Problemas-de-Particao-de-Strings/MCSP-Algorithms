-- | Benchmark measuring size of partitions created by MCSP heuristics.
module Main (main) where

import Prelude hiding (String)

import Data.String qualified as Text

import Control.Monad (forM_)
import MCSP.System.Random (generate)
import MCSP.TestLib.Heuristics (Heuristic, Measured, heuristics, measure, score)
import MCSP.TestLib.Sample (StringParameters, benchParams, randomPairWith, repr)

-- | String type used for benchmarking.
type Target = Word

runHeuristc :: StringParameters -> (Text.String, Heuristic Target) -> IO Measured
runHeuristc params (name, heuristic) = do
    putStr ("    " ++ name ++ ": ")
    pair <- generate (randomPairWith params)
    let meas = measure (name, heuristic) pair
    print (score meas)
    pure meas

main :: IO ()
main =
    forM_
        benchParams
        ( \params -> do
            putStrLn (repr params)
            forM_ heuristics (runHeuristc params)
            putStrLn ""
        )
