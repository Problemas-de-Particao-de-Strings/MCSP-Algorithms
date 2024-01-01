module Main (main) where

import Control.Applicative (pure, (<$>), (<**>))
import Control.Monad (forM_, mapM, (>=>), (>>))
import Data.Bool (Bool (..), not, (&&))
import Data.Data (Proxy (..))
import Data.Eq (Eq (..))
import Data.Foldable (null)
import Data.Function (flip, ($), (.))
import Data.List (dropWhileEnd, filter, lines, map)
import Data.List.Extra (trim)
import Data.Monoid ((<>))
import Data.Word (Word8)
import Options.Applicative (ParserInfo, execParser, fullDesc, header, helper, info, progDesc)
import System.IO (Handle, IO, hFlush, hGetContents, putStrLn, readIO, stdin, stdout)

import MCSP.Data.Pair (fst)
import MCSP.Data.String (String)
import MCSP.TestLib.Heuristics (Measured, heuristics, measure)
import MCSP.Text.CSV (headers, row)

args :: ParserInfo ()
args =
    info (options <**> helper) $
        fullDesc
            <> header "run-db - execute heuristics for the MCSP problem against a database"
            <> progDesc "Execute selected heuristics against a database of string pairs."
  where
    options = pure ()

readDB :: Handle -> IO [String Word8]
readDB = hGetContents >=> mapM readIO . contentLines
  where
    contentLines = dropWhileEnd null . map trim . lines

pairs :: [a] -> [(a, a)]
pairs (x1 : x2 : xs) = (x1, x2) : pairs xs
pairs _ = []

run :: IO ()
run = do
    db <- pairs <$> readDB stdin

    putLn $ headers $ Proxy @Measured
    forM_ db $ \strs ->
        forM_ selectedHeuristics $ \heuristic -> do
            result <- measure heuristic strs
            putLn $ row result
  where
    putLn line = putStrLn line >> hFlush stdout

    runPsoCombine = True
    selectedHeuristics = flip filter heuristics $ \heuristic ->
        fst heuristic == "psoComb" && not runPsoCombine

main :: IO ()
main = execParser args >> run
