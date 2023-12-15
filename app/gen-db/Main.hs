module Main (main) where

import Control.Applicative (pure, (<$>), (<**>))
import Control.Monad (forM_, replicateM, (>>))
import Data.Foldable (length)
import Data.Function (($))
import Data.List (sortOn)
import Data.Monoid ((<>))
import Data.Ord (Ord (..))
import Data.Word (Word8)
import GHC.Num (Num (..))
import GHC.Show (Show (..))
import Options.Applicative.Builder (fullDesc, header, info, progDesc)
import Options.Applicative.Common (ParserInfo)
import Options.Applicative.Extra (execParser, helper)
import System.IO (IO, hFlush, putStrLn, stdout)

import MCSP.Data.String (String, replicate)
import MCSP.System.Random (Random, generate, uniformR)
import MCSP.TestLib.Sample (ShuffleMethod (..), SimpleEnum, StringParameters (..), randomPairWith)

args :: ParserInfo ()
args =
    info (options <**> helper) $
        fullDesc
            <> header "gen-db - generate string pairs for the MCSP problem"
            <> progDesc "Generate 1000 pairs with predefined parameters"
  where
    options = pure ()

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

run :: IO ()
run = do
    strs <- sortOn strSize <$> replicateM 1_000 (generate (randomPair @Word8))
    forM_ strs $ \(s1, s2) -> do
        let n = length s1 `max` length s2
        putLn $ show s1
        putLn $ show $ replicate @Word8 (n + 1) 0
        putLn $ show s2
        putLn $ show $ replicate @Word8 (n + 3) 0
  where
    strSize (s1, s2) = length s1 + length s2
    putLn line = putStrLn line >> hFlush stdout

main :: IO ()
main = execParser args >> run
