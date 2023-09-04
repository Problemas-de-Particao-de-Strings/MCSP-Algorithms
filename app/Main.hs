module Main (main) where

import Prelude hiding (String)

import Control.Arrow (second)
import Control.Monad (forM_, replicateM_)
import Data.List (intercalate)
import Data.String qualified as Text
import Data.Word (Word8)
import System.IO (hFlush, stdout)

import MCSP.Data.String (String)
import MCSP.System.Random (generate, uniformR)
import MCSP.TestLib.Heuristics (Measured (..), heuristics, measure)
import MCSP.TestLib.Heuristics.TH (mkNamed)
import MCSP.TestLib.Sample qualified as S (StringParameters (..), randomPairWith)

genPair :: IO (String Word8, String Word8)
genPair = do
    r <- generate $ uniformR 1 10
    s <- generate $ uniformR 2 50
    n <- generate $ uniformR (r + s + 10) 200
    let params = S.StringParameters {size = n, nReplicated = r, nSingletons = s}
    generate (S.randomPairWith params)

flush :: IO ()
flush = hFlush stdout

showCSV :: [Text.String] -> IO ()
showCSV cols = putStrLn (intercalate "," cols) >> flush

columns :: [(Text.String, Measured -> Text.String)]
columns =
    [ showSnd $(mkNamed 'size),
      showSnd $(mkNamed 'repeats),
      showSnd $(mkNamed 'singles),
      $(mkNamed 'heuristic),
      showSnd $(mkNamed 'blocks),
      showSnd $(mkNamed 'score)
    ]
  where
    showSnd :: Show b => (c, a -> b) -> (c, a -> Text.String)
    showSnd = second (show .)

showHead :: IO ()
showHead = showCSV (map fst columns)

showMeas :: Measured -> IO ()
showMeas m = showCSV (map (\(_, f) -> f m) columns)

run :: IO ()
run = do
    pair <- genPair
    let results = map (`measure` pair) heuristics
    forM_ results showMeas

main :: IO ()
main = showHead >> replicateM_ 10_000 run
