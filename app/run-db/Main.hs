module Main (main) where

import Control.Applicative (pure, (<**>))
import Control.Monad (fail, forM_, mapM, (>=>), (>>), (>>=))
import Data.Bool (Bool (..), not, otherwise, (&&))
import Data.Data (Proxy (..))
import Data.Either (Either (..))
import Data.Eq (Eq (..))
import Data.Foldable (null)
import Data.Function (const, flip, ($), (.))
import Data.List (dropWhileEnd, filter, lines, map)
import Data.List.Extra (trim)
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
    stdin,
    stdout,
    withFile,
 )
import Text.Printf (printf)
import Text.Read (Read (..))
import Text.Show (Show (..))

import MCSP.Data.Pair (fst)
import MCSP.Data.String (String)
import MCSP.TestLib.Heuristics (Measured, heuristics, measure)
import MCSP.Text.CSV (headers, row)

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
      intergenic :: Bool
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
                help "Output file to write the CSV with the measurements"
                    <> long "output"
                    <> short 'o'
                    <> metavar "OUTPUT"
                    <> value StdInOut
                    <> showDefaultWith (const "<stdout>")
        intergenic <-
            switch $
                help "Assume input has intergenic regions (for the FPT algorithm)"
                    <> long "intergenic"
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

run :: Arguments -> IO ()
run Arguments {..} = do
    db <- withTextInOut input ReadMode readDB
    let strs = pairs $ if intergenic then skipAtOdd db else db

    withTextInOut output WriteMode $ \out -> do
        putLn out $ headers $ Proxy @Measured
        forM_ strs $ \pair ->
            forM_ selectedHeuristics $ \heuristic -> do
                result <- measure heuristic pair
                putLn out $ row result
  where
    runPsoCombine = True
    selectedHeuristics = flip filter heuristics $ \heuristic ->
        fst heuristic == "psoComb" && not runPsoCombine

main :: IO ()
main = execParser args >>= run
