module Main (main) where

import Prelude hiding (String)

import Control.Monad (forM_, replicateM_)
import Data.Word (Word8)

import MCSP.Data.String (String)
import MCSP.System.Random (generate, uniformR)
import MCSP.TestLib.Heuristics (csvHeader, heuristics, measure, toCsvRow)
import MCSP.TestLib.Sample qualified as S (StringParameters (..), randomPairWith)

genPair :: IO (String Word8, String Word8)
genPair = do
    r <- generate $ uniformR 1 10
    s <- generate $ uniformR 2 50
    n <- generate $ uniformR (r + s + 10) 200
    let params = S.StringParameters {size = n, nReplicated = r, nSingletons = s}
    generate (S.randomPairWith params)

run :: IO ()
run = do
    pair <- genPair
    let results = map (`measure` pair) heuristics
    forM_ results (putStrLn . toCsvRow)

main :: IO ()
main = putStrLn csvHeader >> replicateM_ 10_000 run
