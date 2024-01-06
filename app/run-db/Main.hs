module Main (main) where

import Control.Applicative (pure, (<$>))
import Control.Exception (IOException, catch, displayException)
import Control.Monad (forM_, mapM, (>=>), (>>), (>>=))
import Data.Bool (not)
import Data.Data (Proxy (..))
import Data.Foldable (elem, null, toList)
import Data.Function (id, ($), (.))
import Data.List (dropWhileEnd, filter, lines, map, (++))
import Data.List.Extra (trim)
import Data.Map.Strict (Map, empty, fromList, lookup)
import Data.Maybe (maybe)
import Data.String qualified as Text (String)
import Data.Word (Word8)
import System.IO (
    Handle,
    IO,
    IOMode (..),
    hFlush,
    hGetContents,
    hPutStrLn,
    readIO,
    stderr,
 )
import Text.Show (Show (..))

import MCSP.Data.Meta ((<::))
import MCSP.Data.Pair (fst, second, (&&&))
import MCSP.Data.String (String)
import MCSP.TestLib.Heuristics (Measured (..), heuristics, measure)
import MCSP.Text.CSV (headers, parseFile, row)

import Args (Arguments (..), TextInOut, parseArgs, withTextInOut)

readDB :: Handle -> IO [String Word8]
readDB = hGetContents >=> mapM readIO . contentLines
  where
    contentLines = dropWhileEnd null . map trim . lines

pairs :: [a] -> [(a, a)]
pairs (x1 : x2 : xs) = (x1, x2) : pairs xs
pairs _ = []

skipAtOdd :: [a] -> [a]
skipAtOdd (x1 : _ : xs) = x1 : skipAtOdd xs
skipAtOdd [x] = [x]
skipAtOdd [] = []

putLn :: Handle -> Text.String -> IO ()
putLn handle line = hPutStrLn handle line >> hFlush handle

loadResults :: Handle -> IO (Map (Text.String, Text.String) Measured)
loadResults handle = collect <$> parseFile handle
  where
    collect = fromList . map (resultKey &&& id) . toList
    resultKey measured = (heuristic measured, pair measured)

continueResults :: TextInOut -> IO (Map (Text.String, Text.String) Measured)
continueResults handle = catch (withTextInOut handle ReadMode loadResults) $ \err -> do
    hPutStrLn stderr $
        "warning: could not continue CSV output: " ++ displayException @IOException err
    pure empty

run :: Arguments -> IO ()
run Arguments {..} = do
    db <- withTextInOut input ReadMode readDB
    let strs = pairs $ if intergenic then skipAtOdd db else db

    previous <-
        if continue
            then continueResults output
            else pure empty

    withTextInOut output WriteMode $ \out -> do
        putLn out $ headers $ Proxy @Measured
        forM_ strs $ \pair ->
            forM_ selectedHeuristics $ \(second setVars -> heuristic) -> do
                result <-
                    maybe (measure heuristic pair) pure $
                        lookup (fst heuristic, show pair) previous
                putLn out $ row result
  where
    selectedHeuristics = filter (not . (`elem` exclude) . fst) heuristics

    setVars heuristic strs =
        heuristic strs
            <:: psoIterations
            <:: psoParticles
            <:: psoSeed
            <:: psoUpdater
            <:: psoInitial

main :: IO ()
main = parseArgs >>= run
