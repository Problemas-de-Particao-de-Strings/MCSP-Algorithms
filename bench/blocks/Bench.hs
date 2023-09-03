-- | Benchmark measuring size of partitions created by MCSP heuristics.
module Main (main) where

import Prelude hiding (String)

import Data.String qualified as Text

import Control.Monad (forM_)
import MCSP.Data.String (String)
import MCSP.System.Random (generate)
import MCSP.TestLib.Heuristics (Heuristic, heuristics)
import MCSP.TestLib.Sample (StringParameters, benchParams, randomPairWith, repr)

withRelativeDistance :: Heuristic a -> (String a, String a) -> Double
withRelativeDistance heuristic pair = len (uncurry heuristic pair) / len pair
  where
    len (x, y) = fromIntegral (length x + length y)

-- | String type used for benchmarking.
type Target = Word

runHeuristc :: StringParameters -> (Text.String, Heuristic Target) -> IO Double
runHeuristc params (name, heuristic) = do
    putStr ("    " ++ name ++ ": ")
    pair <- generate (randomPairWith params)
    let relDist = withRelativeDistance heuristic pair
    print relDist
    pure relDist

main :: IO ()
main =
    forM_
        benchParams
        ( \params -> do
            putStrLn (repr params)
            forM_ heuristics (runHeuristc params)
            putStrLn ""
        )
