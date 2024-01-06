module Main (main) where

import Control.Applicative (pure, (<$>))
import Control.Monad (forM_, replicateM, when, (>>), (>>=))
import Data.Foldable (length)
import Data.Function (id, ($), (.))
import Data.List (sortOn)
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Ord (Ord (..))
import Data.Word (Word8)
import GHC.Num (Num (..))
import GHC.Real (fromIntegral)
import System.IO (IO, hFlush, putStrLn, stdout)
import Text.Show (Show (..))

import MCSP.Data.Pair (first, ($:))
import MCSP.Data.String (String, replicate)
import MCSP.System.Random (Random, generate, generateWith, uniformR)
import MCSP.TestLib.Sample (ShuffleMethod (..), StringParameters (..), randomPairWith)

import Args (Arguments (..), parseArgs)

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
main = parseArgs >>= run
