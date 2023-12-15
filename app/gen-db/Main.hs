module Main (main) where

import Prelude hiding (String)

import Control.Monad (forM_, replicateM_)
import Data.Data (Proxy (..))
import Data.Word (Word8)
import System.IO (hFlush, stdout)

import MCSP.Data.String (String)
import MCSP.System.Random (Random, generate, uniformR)
import MCSP.TestLib.Heuristics (Measured, heuristics, measure)
import MCSP.TestLib.Sample (ShuffleMethod (..), SimpleEnum, StringParameters (..), randomPairWith)
import MCSP.Text.CSV (headers, row)

randomPair :: SimpleEnum a => Random (String a, String a)
randomPair = do
    r <- uniformR 3 10
    s <- uniformR 5 20
    n <- uniformR (2 * r + s) 120
    randomPairWith
        StringParameters
            { size = n,
              nReplicated = r,
              nSingletons = s,
              shuffle = Chars
            }

main :: IO ()
main = do
    putLn $ headers (Proxy @Measured)
    replicateM_ 10_000 $ do
        pair <- generate randomPair
        forM_ heuristics $ \heuristc -> do
            result <- measure @Word8 heuristc pair
            putLn $ row result
  where
    putLn line = putStrLn line >> hFlush stdout
