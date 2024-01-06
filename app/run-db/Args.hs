module Args (
    Arguments (..),
    parseArgs,
    TextInOut,
    withTextInOut,
) where

import Control.Applicative (many, pure, (<$>), (<**>))
import Control.Monad (fail, forM, unless)
import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Either.Extra (mapLeft)
import Data.Eq (Eq (..))
import Data.Function (const, ($), (.))
import Data.List (length, words)
import Data.List.Extra (trim)
import Data.Maybe (Maybe (..))
import Data.Monoid ((<>))
import Data.String qualified as Text (String)
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
    stdin,
    stdout,
    withFile,
 )
import Text.Printf (printf)
import Text.Read (Read (..))
import Text.Show (Show (..))

import MCSP.Data.Meta (evalMeta, getVar)
import MCSP.Heuristics (
    PSOInitialDistribution (..),
    PSOIterations (..),
    PSOParticles (..),
    PSOSeed (..),
    PSOUpdaterWeigths (..),
 )
import MCSP.System.Random (Seed, readSeedP, showSeed)
import MCSP.Text.ReadP (readEitherP, readP)

readSeed :: ReadM Seed
readSeed = eitherReader $ mapLeft (printf "invalid seed: %s") . readEitherP readSeedP

readValueList :: Read a => ReadM [a]
readValueList = do
    text <- str
    forM (words text) $ \word ->
        case readEitherP readP word of
            Right val -> pure val
            Left error -> fail $ printf "invalid value '%s': %s" word error

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

filePath :: ReadM FilePath
filePath = do
    path <- str
    unless (isValid path) $
        fail (printf "invalid path: '%s'" path)
    pure path

textInOut :: ReadM TextInOut
textInOut = do
    path <- str
    if trim path == "-"
        then pure StdInOut
        else File <$> filePath

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
      fptPrefix :: Maybe FilePath,
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
            argument textInOut $
                help "Input file to read the database from"
                    <> metavar "INPUT"
                    <> value StdInOut
                    <> showDefaultWith (const "<stdin>")
        output <-
            option textInOut $
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
        fptPrefix <-
            option (Just <$> filePath) $
                help "Prefix for the FPT algorithm output files"
                    <> long "fpt"
                    <> metavar "PREFIX"
                    <> value Nothing
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
                    <> metavar "INT"
                    <> value (evalMeta getVar)
                    <> showDefaultWith (\(PSOIterations i) -> show i)
        psoParticles <-
            option (PSOParticles <$> auto) $
                help "Number of particles used in each iteration of the PSO heuristic"
                    <> long "pso-particles"
                    <> metavar "INT"
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

parseArgs :: IO Arguments
parseArgs = execParser args
