module Main (main) where

import Control.Applicative (many, pure, (<$>), (<**>))
import Control.Exception (IOException, catch, displayException)
import Control.Monad (fail, forM, forM_, mapM, (>=>), (>>), (>>=))
import Data.Bool (Bool (..), not, otherwise)
import Data.Data (Proxy (..))
import Data.Either (Either (..))
import Data.Either.Extra (mapLeft)
import Data.Eq (Eq (..))
import Data.Foldable (elem, null, toList)
import Data.Function (const, id, ($), (.))
import Data.List (dropWhileEnd, filter, length, lines, map, words, (++))
import Data.List.Extra (trim)
import Data.Map.Strict (Map, empty, fromList, lookup)
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Data.String qualified as Text (String)
import Data.Word (Word8)
import Options.Applicative (
    ParserInfo,
    ReadM,
    argument,
    auto,
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

import MCSP.Data.Meta (evalMeta, getVar, (<::))
import MCSP.Data.Pair (fst, second, (&&&))
import MCSP.Data.String (String)
import MCSP.Heuristics (
    PSOInitialDistribution (..),
    PSOIterations (..),
    PSOParticles (..),
    PSOSeed (..),
    PSOUpdaterWeigths (..),
 )
import MCSP.System.Random (Seed, readSeedP, showSeed)
import MCSP.TestLib.Heuristics (Measured, heuristic, heuristics, measure, pair)
import MCSP.Text.CSV (headers, parseFile, row)
import MCSP.Text.ReadP (readEitherP, readP)

readSeed :: ReadM Seed
readSeed = eitherReader $ mapLeft (printf "invalid seed: %s") . readEitherP readSeedP

readValueList :: Read a => ReadM [a]
readValueList = do
    text <- str
    forM (words text) $ \word ->
        case readEitherP readP word of
            Right val -> pure val
            Left error -> fail $ printf "invalid value %s: %s" (show word) error

readUpdaterWeights :: ReadM PSOUpdaterWeigths
readUpdaterWeights = do
    values <- readValueList
    case values of
        [kE, kL, kG] -> pure (PSOUpdaterWeigths {..})
        _ -> fail $ printf "wrong number of weights: expected 3, got %d" (length values)

readInitialDistribution :: ReadM PSOInitialDistribution
readInitialDistribution = do
    values <- readValueList
    case values of
        [d1, d2, d3, d4] -> pure (PSOInitialDistribution d1 d2 d3 d4)
        _ -> fail $ printf "wrong number of weights: expected 4, got %d" (length values)

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
      exclude :: [Text.String],
      psoIterations :: PSOIterations,
      psoParticles :: PSOParticles,
      psoSeed :: PSOSeed,
      psoUpdater :: PSOUpdaterWeigths,
      psoInitial :: PSOInitialDistribution
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
        psoIterations <-
            option (PSOIterations <$> auto) $
                help "Number of iterations to run the PSO heuristic"
                    <> long "pso-iterations"
                    <> metavar "N_ITER"
                    <> value (evalMeta getVar)
                    <> showDefaultWith (\(PSOIterations i) -> show i)
        psoParticles <-
            option (PSOParticles <$> auto) $
                help "Number of particles used in each iteration of the PSO heuristic"
                    <> long "pso-particles"
                    <> metavar "N_PART"
                    <> value (evalMeta getVar)
                    <> showDefaultWith (\(PSOParticles n) -> show n)
        psoSeed <-
            option (PSOSeed <$> readSeed) $
                help "Initial seed for the PSO heuristic"
                    <> long "pso-seed"
                    <> metavar "\"HEX HEX\""
                    <> value (evalMeta getVar)
                    <> showDefaultWith (\(PSOSeed s) -> showSeed s)
        psoUpdater <-
            option readUpdaterWeights $
                help "Weights used for the default particle updater in PSO"
                    <> long "pso-updater-weights"
                    <> metavar "\"kE kL kG\""
                    <> value (evalMeta getVar)
                    <> showDefaultWith (\PSOUpdaterWeigths {..} -> printf "%f %f %f" kE kL kG)
        psoInitial <-
            option readInitialDistribution $
                help "Distribution weights used to generate the initial particles for PSO"
                    <> long "pso-initial-dist"
                    <> metavar "\"D1 D2 D3 D4\""
                    <> value (evalMeta getVar)
                    <> showDefaultWith
                        ( \(PSOInitialDistribution d1 d2 d3 d4) ->
                            printf "%f %f %f %f" d1 d2 d3 d4
                        )
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
main = execParser args >>= run
