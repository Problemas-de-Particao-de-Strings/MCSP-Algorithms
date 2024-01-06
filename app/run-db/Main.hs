module Main (main) where

import Control.Applicative (pure, (<$>))
import Control.Exception (IOException, catch, displayException, try)
import Control.Monad (forM_, mapM, (=<<), (>=>), (>>), (>>=))
import Control.Monad.Extra (whenJust)
import Data.Bool (not)
import Data.Data (Proxy (..))
import Data.Either (either)
import Data.Foldable (elem, length, null, toList)
import Data.Function (const, id, ($), (.))
import Data.Int (Int)
import Data.List (dropWhileEnd, filter, isInfixOf, lines, map, words, (++))
import Data.List.Extra (lastDef, trim, zipFrom)
import Data.Map.Strict (Map, empty, fromList, lookup)
import Data.Maybe (maybe)
import Data.String qualified as Text (String)
import Data.Word (Word8)
import GHC.Float (Double)
import GHC.Num ((-))
import System.IO (
    FilePath,
    Handle,
    IO,
    IOMode (..),
    hFlush,
    hGetContents,
    hGetLine,
    hPutStrLn,
    readIO,
    stderr,
    withFile,
 )
import Text.Printf (printf)
import Text.Show (Show (..))

import MCSP.Data.Meta ((<::))
import MCSP.Data.Pair (Pair, fst, second, (&&&))
import MCSP.Data.String (String)
import MCSP.Heuristics (trivial)
import MCSP.TestLib.Heuristics (Measured (..), heuristics, measure, scoreFrom)
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

readFPT :: FilePath -> Int -> Pair (String Word8) -> IO Measured
readFPT prefix solutionId strs =
    withFile filename ReadMode $ \solution -> do
        ftp <- readIO @(String Int) =<< hGetLine solution
        _ <- readIO @(String Int) =<< hGetLine solution
        _ <- readIO @(String Int) =<< hGetLine solution
        _ <- readIO @(String Int) =<< hGetLine solution
        runtimeLine <- words <$> hGetLine solution
        markerLine <- either (const []) words <$> try @IOException (hGetLine solution)

        let blocks = length ftp - 2
        time <- readIO @Double $ lastDef "" runtimeLine
        let exact = not (["Not", "Exact"] `isInfixOf` markerLine)
        let name = if exact then "FPT" else "FPT(NE)"

        result <- measure (name, trivial) strs
        let score = scoreFrom (size result) blocks
        pure result {time, blocks, score}
  where
    filename = printf "%s%04d" prefix solutionId

run :: Arguments -> IO ()
run Arguments {..} = do
    -- read and parse input strings
    db <- withTextInOut input ReadMode readDB
    let strs = pairs $ if intergenic then skipAtOdd db else db

    -- load previous results from output, for continuation
    previousResults <-
        if continue
            then continueResults output
            else pure empty

    -- write results in CSV
    withTextInOut output WriteMode $ \out -> do
        putLn out $ headers $ Proxy @Measured
        forM_ (zipFrom 1 strs) $ \(idx, pair) -> do
            -- run each selected heuritic
            forM_ selectedHeuristics $ \(second setVars -> heuristic) -> do
                result <-
                    maybe (measure heuristic pair) pure $
                        lookup (fst heuristic, show pair) previousResults
                putLn out $ row result
            -- then read and save FPT results
            whenJust fptPrefix $ \prefix -> do
                result <- readFPT prefix idx pair
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
