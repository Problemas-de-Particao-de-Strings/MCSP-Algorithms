module Main (main) where

import Prelude hiding (String)

import Control.Monad (forM_, replicateM_)
import Data.Word (Word8)
import System.IO (hFlush, stdout)

import MCSP.Data.String (String)
import MCSP.System.Random (Random, generate, uniformR)
import MCSP.TestLib.Heuristics (csvHeader, heuristics, measure, toCsvRow)
import MCSP.TestLib.Sample (ShuffleMethod (..), StringParameters (..), randomPairWith)

randomPair :: Random (String Word8, String Word8)
randomPair = do
    r <- uniformR 5 10
    s <- uniformR 5 30
    n <- uniformR (2 * r + s) 80
    randomPairWith
        StringParameters
            { size = n,
              nReplicated = r,
              nSingletons = s,
              shuffle = Chars
            }

main :: IO ()
main = do
    putLn csvHeader
    replicateM_ 10_000 $ do
        pair <- generate randomPair
        forM_ heuristics $ \heuristc -> do
            result <- measure heuristc pair
            putLn $ toCsvRow result
  where
    putLn line = putStrLn line >> hFlush stdout
