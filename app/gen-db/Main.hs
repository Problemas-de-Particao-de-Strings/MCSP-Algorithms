module Main (main) where

import Control.Applicative (pure, (<$>), (<**>))
import Control.Monad (forM_, replicateM, when, (>>), (>>=))
import Data.Bool (Bool)
import Data.Either (Either (..))
import Data.Either.Extra (mapLeft)
import Data.Foldable (length)
import Data.Function (id, ($), (.))
import Data.List (map, sortOn, (++))
import Data.List.Extra qualified as List (splitOn, trim)
import Data.Maybe (Maybe (..), fromMaybe, maybe)
import Data.Monoid ((<>))
import Data.Ord (Ord (..))
import Data.String qualified as Text (String)
import Data.Word (Word, Word8)
import GHC.Enum (maxBound)
import GHC.Num (Num (..))
import GHC.Real (fromIntegral)
import Numeric.Natural (Natural)
import Options.Applicative (
    ParserInfo,
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
    showDefault,
    showDefaultWith,
    switch,
    value,
 )
import System.IO (IO, hFlush, putStrLn, stdout)
import Text.Printf (printf)
import Text.Read (Read (..), readEither)
import Text.Show (Show (..))

import MCSP.Data.Pair (first, ($:))
import MCSP.Data.String (String, replicate)
import MCSP.System.Random (Random, Seed, generate, generateWith, readSeedP, uniformR)
import MCSP.TestLib.Sample (ShuffleMethod (..), StringParameters (..), randomPairWith)
import MCSP.Text.ReadP (readEitherP)

type Parser a = Text.String -> Either Text.String a

readRangeError :: Text.String -> Text.String -> Either Text.String never
readRangeError str message = Left $ printf "invalid range %s: %s" (show str) message

type Range a = (a, a)
type LRange a = (Maybe a, a)

-- | Read a range "x <= y" or an unbounded range "<= y".
readLRange :: Ord a => Parser a -> Parser (LRange a)
readLRange read str = case map List.trim $ List.splitOn "<=" str of
    -- single value "x", represents the range "x <= x"
    [strVal]
        | Right val <- read strVal ->
            Right (Just val, val)
    -- single value "<= x", represents the unbound range "-Infinity <= x"
    ["", strHi] -> do
        hi <- read strHi
        pure (Nothing, hi)
    -- standard two value range "x <= y"
    [strLo, strHi] -> do
        lo <- read strLo
        hi <- read strHi
        when (lo > hi) $ readRangeError str "start is larger than end"
        pure (Just lo, hi)
    (_ : _ : _) -> readRangeError str "too many intervals"
    _ -> readRangeError str "expecting \"X <= Y\" or \"X\""

-- | Read a bounded range "x <= y".
readRange :: Ord a => Parser a -> Parser (Range a)
readRange read str = do
    (maybeLo, hi) <- readLRange read str
    lo <- maybe (readRangeError str "expecting \"X <= Y\" or \"X\"") pure maybeLo
    pure (lo, hi)

showLRange :: Show a => LRange a -> Text.String
showLRange (Nothing, hi) = printf "<= %s" (show hi)
showLRange (Just lo, hi) = printf "%s <= %s" (show lo) (show hi)

showRange :: Show a => Range a -> Text.String
showRange = showLRange . first Just

-- | Read a `Word8`, checking if value is in range.
readWord8 :: Parser Word8
readWord8 str = do
    nat <- readEither @Natural str
    when (nat >= maxValue) $
        Left (printf "value too large %d" nat)
    pure $ fromIntegral nat
  where
    maxValue = fromIntegral (maxBound @Word8)

readSeed :: Parser Seed
readSeed = mapLeft ("invalid seed: " ++) . readEitherP readSeedP

data Arguments = Arguments
    { count :: Word,
      replicated :: Range Word8,
      singletons :: Range Word8,
      strSize :: LRange Word8,
      seed :: Maybe Seed,
      intergenic :: Bool,
      unsorted :: Bool
    }
    deriving stock (Show, Read)

args :: ParserInfo Arguments
args =
    info (options <**> helper) $
        fullDesc
            <> header "gen-db - generate string pairs for the MCSP problem"
            <> progDesc "Generate multiple pairs with custom parameters."
  where
    options = do
        count <-
            option auto $
                help "Number of strings to generate"
                    <> long "count"
                    <> short 'c'
                    <> metavar "INT"
                    <> value 1_000
                    <> showDefault
        replicated <-
            option (eitherReader $ readRange readWord8) $
                help "Number (or interval) of replicated genes in each string"
                    <> long "replicated"
                    <> short 'r'
                    <> metavar "\"INT <= INT\""
                    <> value (5, 10)
                    <> showDefaultWith showRange
        singletons <-
            option (eitherReader $ readRange readWord8) $
                help "Number (or interval) of singletons in each string"
                    <> long "singletons"
                    <> short 's'
                    <> metavar "\"INT <= INT\""
                    <> value (5, 30)
                    <> showDefaultWith showRange
        strSize <-
            option (eitherReader $ readLRange readWord8) $
                help "Number (or interval) of total genes in each string"
                    <> long "size"
                    <> short 'n'
                    <> metavar "\"INT <= INT\""
                    <> value (Nothing, 80)
                    <> showDefaultWith showLRange
        seed <-
            option (Just <$> eitherReader readSeed) $
                help "Seed used to generate strings"
                    <> long "seed"
                    <> metavar "\"HEX HEX\""
                    <> value Nothing
        intergenic <-
            switch $
                help "Generate intergenic regions (for the FPT algorithm)"
                    <> long "intergenic"
        unsorted <-
            switch $
                help "Print strings as they are generated, instead of sorting by size"
                    <> long "unsorted"
        pure Arguments {..}

randomPair :: Arguments -> Random (String Word8, String Word8)
randomPair Arguments {..} = do
    r <- uniformR $: replicated
    s <- uniformR $: singletons
    n <- uniformR $: first (clampTo (2 * r + s)) strSize
    randomPairWith
        StringParameters
            { size = fromIntegral n,
              nReplicated = fromIntegral r,
              nSingletons = fromIntegral s,
              shuffle = Chars
            }
  where
    clampTo nMin (fromMaybe nMin -> val) = max nMin val

run :: Arguments -> IO ()
run arguments@Arguments {..} = do
    strs <- sort <$> gen pairs
    forM_ strs $ \(s1, s2) -> do
        let n = length s1 `max` length s2
        putLn $ show s1
        when intergenic $
            putLn (show $ replicate @Word8 (n + 1) 0)
        putLn $ show s2
        when intergenic $
            putLn (show $ replicate @Word8 (n + 3) 0)
  where
    putLn line = putStrLn line >> hFlush stdout
    strSizeOf (s1, s2) = length s1 + length s2
    sort = if unsorted then id else sortOn strSizeOf
    pairs = replicateM (fromIntegral count) $ randomPair arguments
    gen = case seed of
        Just s -> pure . generateWith s
        Nothing -> generate

main :: IO ()
main = execParser args >>= run
