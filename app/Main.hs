module Main (main) where

import Prelude hiding (String)

import Control.Monad (forM_, replicateM_)
import Data.Word (Word8)

import MCSP.Data.String (String)
import MCSP.System.Random (Random, uniformR)
import MCSP.TestLib.Heuristics (csvHeader, heuristics, measure, randomSeed, toCsvRow)
import MCSP.TestLib.Sample (ShuffleMethod (..), StringParameters (..), randomPairWith)

genPair :: Random (String Word8, String Word8)
genPair = do
    r <- uniformR 1 10
    s <- uniformR 2 50
    n <- uniformR (r + s + 10) 200
    randomPairWith
        StringParameters
            { size = n,
              nReplicated = r,
              nSingletons = s,
              shuffle = Chars
            }

run :: IO ()
run = do
    seed <- randomSeed
    forM_ heuristics $ \heuristc ->
        putStrLn $ toCsvRow $ measure heuristc seed genPair

main :: IO ()
main = putStrLn csvHeader >> replicateM_ 10_000 run
