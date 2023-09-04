-- | Benchmark measuring size of partitions created by MCSP heuristics.
module Main (main) where

import Prelude hiding (String)

import Control.Arrow (first)
import Control.Monad (forM_)
import Data.String qualified as Text
import Data.Vector qualified as V (Vector)
import Data.Vector.Generic qualified as G
import Statistics.ConfidenceInt (binomialCI)
import Statistics.Regression (olsRegress)
import Statistics.Types (CL, cl95, confidenceInterval)
import System.IO (hFlush, stdout)

import MCSP.System.Random (generate)
import MCSP.TestLib.Heuristics (Heuristic, Measured, blocks, heuristics, measure, size)
import MCSP.TestLib.Sample (StringParameters, benchParams, randomPairWith, repr)

-- ----------------------------- --
-- Benchmarking and Measurements --

-- | Confidence level expected for each benchmark.
confLevel :: CL Double
confLevel = cl95
{-# INLINE confLevel #-}

-- | Lower and upper limit on the number of runs for a single benchmark.
runs :: (Int, Int)
runs = (4, 30)
{-# INLINE runs #-}

-- | Apply a function to a pair of arguments.
($:) :: (a -> b -> c) -> (a, b) -> c
($:) = uncurry
{-# INLINE ($:) #-}

-- | The x-axis of the regression, counting the number of iterations to run for each data point.
series :: Integral a => [a]
series = squish $ map truncate $ iterate (ratio *) 1
  where
    ratio = 1.05 :: Double
    squish = foldr dropRepeated []
    dropRepeated x xs = x : dropWhile (x ==) xs

-- | Estimate the confidence interval for the running heuristic, returning the expected error in
-- number of blocks.
estimateCI :: CL Double -> Int -> Int -> Int -> Double
estimateCI cl maxBlocks generatedBlocks minBlocks =
    crange * fromIntegral maxBlocks / fromIntegral minBlocks
  where
    ci = confidenceInterval (binomialCI cl (maxBlocks - minBlocks) (generatedBlocks - minBlocks))
    crange = abs ((-) $: ci)

-- | Evaluate each element in a vector and sum the results.
sumOn :: Num b => (a -> b) -> V.Vector a -> b
sumOn f = G.foldl' (\s m -> s + f m) 0

-- | Run a single benchmark until the estimated confidence interval is low enough for `confLevel`.
--
-- Returns a vector of data points (@`V.Vector` `Measured`@), such that the x-axis is the number of
-- iterations for that point (given by `G.length`) and the y-axis should be a fold over the
-- measurements in the data fold.
runBenchmark :: IO Measured -> IO (V.Vector (V.Vector Measured))
runBenchmark = go (take (max $: runs) series) 0 0 0 G.empty
  where
    go [] _ _ _ acc _ = pure acc
    go (it : iters) totalStr totalChr totalBlk acc m = do
        -- current iteration index
        let run = G.length acc + 1

        -- run the heuristic multiple times and collect the measurements
        value <- G.replicateM it m
        let strs = totalStr + G.length value
        let chrs = totalChr + sumOn size value
        let blks = totalBlk + sumOn blocks value

        let result = G.snoc acc value
        -- stop after a minimum number of runs and the confidence interval is smaller than 1 block
        if run >= min $: runs && estimateCI confLevel chrs blks strs < 1
            then pure result
            else go iters strs chrs blks result m

-- --------------- --
-- Result Analysis --

-- | Make a regression of measurements against a value of the measurements.
regress :: (Measured -> Double) -> V.Vector (V.Vector Measured) -> (Double, Double)
regress f v = first G.head (olsRegress [iters] target)
  where
    iters = G.convert $ G.map (fromIntegral . G.length) v
    target = G.convert $ G.map (sumOn f) v

-- -------------------------- --
-- Benchmark groups and setup --

-- | String type used for benchmarking.
type Target = Word

-- | Creates an `IO` that generatores a pair of strings, run the heuristic and run measuments on it.
measuring :: StringParameters -> (Text.String, Heuristic Target) -> IO Measured
measuring params heuristic = measure heuristic <$> generate (randomPairWith params)

-- | Print line and flush `stdout`.
putStrLn' :: Text.String -> IO ()
putStrLn' s = putStrLn s >> hFlush stdout

-- | Run a matrix of benchmarks for each parameter set and heuristic.
main :: IO ()
main = forM_ benchParams (forM_ heuristics . run)
  where
    run params heuristic = do
        putStrLn' $ "benchmarking " ++ repr params ++ "/" ++ fst heuristic
        -- run benchmark and analyse results
        results <- runBenchmark $ measuring params heuristic
        let (blks, r2) = regress (fromIntegral . blocks) results
        -- formatted output
        putStrLn' $ "blocks:     \t" ++ show blks
        putStrLn' $ "            \t" ++ show r2 ++ " R²"
        putStrLn' $ "data points:\t" ++ show (G.length results)
        putStrLn' ""
