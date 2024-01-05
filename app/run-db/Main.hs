module Main (main) where

import Control.Applicative (many, pure, (<$>), (<**>))
import Control.Exception (IOException, catch, displayException)
import Control.Monad (fail, forM_, mapM, (>=>), (>>), (>>=))
import Data.Bool (Bool (..), not, otherwise)
import Data.Data (Proxy (..))
import Data.Either (Either (..))
import Data.Eq (Eq (..))
import Data.Foldable (elem, null, toList)
import Data.Function (const, id, ($), (.))
import Data.List (dropWhileEnd, filter, lines, map, (++))
import Data.List.Extra (trim)
import Data.Map.Strict (Map, empty, fromList, lookup)
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Data.String qualified as Text (String)
import Data.Word (Word8)
import Options.Applicative (
    ParserInfo,
    argument,
    eitherReader,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    showDefaultWith,
    str,
    switch,
    value,
 )
import System.FilePath (isValid)
import System.IO (
    FilePath,
    Handle,
    IO,
    IOMode (..),
    hFlush,
    hGetContents,
    hPutStrLn,
    readIO,
    stderr,
    stdin,
    stdout,
    withFile,
 )
import Text.Printf (printf)
import Text.Read (Read (..))
import Text.Show (Show (..))

import MCSP.Data.Pair (fst, (&&&))
import MCSP.Data.String (String)
import MCSP.TestLib.Heuristics (Measured, heuristic, heuristics, measure, pair)
import MCSP.Text.CSV (headers, parseFile, row)

data TextInOut = StdInOut | File FilePath
    deriving stock (Show, Read, Eq)

textInOut :: Text.String -> Either Text.String TextInOut
textInOut text
    | trim text == "-" = Right StdInOut
    | isValid text = Right (File text)
    | otherwise = Left $ printf "invalid path: '%s'" text

withTextInOut :: TextInOut -> IOMode -> (Handle -> IO a) -> IO a
withTextInOut StdInOut mode exec = case mode of
    ReadMode -> exec stdin
    WriteMode -> exec stdout
    AppendMode -> exec stdout
    ReadWriteMode -> fail "invalid IOMode for standard handles"
withTextInOut (File path) mode exec = withFile path mode exec

data Arguments = Arguments
    { input :: TextInOut,
      output :: TextInOut,
      continue :: Bool,
      intergenic :: Bool,
      exclude :: [Text.String]
    }
    deriving stock (Show)

args :: ParserInfo Arguments
args =
    info (options <**> helper) $
        fullDesc
            <> header "run-db - execute heuristics for the MCSP problem against a database"
            <> progDesc "Execute selected heuristics against a database of string pairs."
  where
    options = do
        input <-
            argument (eitherReader textInOut) $
                help "Input file to read the database from"
                    <> metavar "INPUT"
                    <> value StdInOut
                    <> showDefaultWith (const "<stdin>")
        output <-
            option (eitherReader textInOut) $
                help "Output file to write the measurements as CSV"
                    <> long "output"
                    <> short 'o'
                    <> metavar "OUTPUT"
                    <> value StdInOut
                    <> showDefaultWith (const "<stdout>")
        continue <-
            switch $
                help "Parse the CSV output and continue that, instead of running from the start"
                    <> long "continue"
                    <> short 'c'
        intergenic <-
            switch $
                help "Assume input has intergenic regions (for the FPT algorithm)"
                    <> long "intergenic"
        exclude <-
            many . option str $
                help "Don't run the specified heuristic"
                    <> long "exclude"
                    <> short 'x'
                    <> metavar "HEURISTIC"
        pure Arguments {..}

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
            forM_ selectedHeuristics $ \heuristic -> do
                result <-
                    maybe (measure heuristic pair) pure $
                        lookup (fst heuristic, show pair) previous
                putLn out $ row result
  where
    selectedHeuristics = filter (not . (`elem` exclude) . fst) heuristics

main :: IO ()
main = execParser args >>= run
