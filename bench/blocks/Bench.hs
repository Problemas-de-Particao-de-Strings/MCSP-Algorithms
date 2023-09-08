-- | Benchmark measuring size of partitions created by MCSP heuristics.
module Main (main) where

import Prelude

import Control.Arrow (first)
import Control.Monad (forM_)
import Data.Vector qualified as V (Vector)
import Data.Vector.Generic qualified as G
import Data.Word (Word8)
import Statistics.Regression (olsRegress)
import Statistics.Types (CL, cl95)
import System.IO (Handle, IOMode (AppendMode), hFlush, hPutStrLn, stdout, withFile)

import MCSP.System.Path (createDirectory, getCurrentTimestamp, packageRoot, (<.>), (</>))
import MCSP.System.Random (generate)

import MCSP.System.Statistics (absolute, sampleCI)
import MCSP.TestLib.Sample (StringParameters, benchParams, randomPairWith, repr)

import MCSP.TestLib.Heuristics (
    Heuristic,
    Measured,
    blocks,
    csvHeader,
    heuristics,
    measure,
    toCsvRow,
 )

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

-- | Evaluate each element in a vector and sum the results.
sumOn :: Num b => (a -> b) -> V.Vector a -> b
sumOn f = G.foldl' (\s m -> s + f m) 0

-- | Converts all the values and the output vector type.
convert :: (G.Vector v a, G.Vector w b) => (a -> b) -> v a -> w b
convert f vec = G.generate (G.length vec) (f . G.unsafeIndex vec)

-- | Run a single benchmark until the estimated confidence interval is low enough for `confLevel`.
--
-- Returns a vector of data points (@`V.Vector` `Measured`@), such that the x-axis is the number of
-- iterations for that point (given by `G.length`) and the y-axis should be a fold over the
-- measurements in the data fold.
runBenchmark :: IO Measured -> IO (V.Vector (V.Vector Measured))
runBenchmark = go (take (max $: runs) series) G.empty G.empty
  where
    go [] _ acc _ = pure acc
    go (it : iters) blks acc m = do
        -- current iteration index
        let run = G.length acc + 1

        -- run the heuristic multiple times and collect the measurements
        value <- G.replicateM it m
        let totalBlks = blks G.++ convert (fromIntegral . blocks) value

        let result = G.snoc acc value
        -- stop after a minimum number of runs and the confidence interval is smaller than 1 block
        if run >= min $: runs && absolute (sampleCI confLevel totalBlks) < 1
            then pure result
            else go iters totalBlks result m

-- --------------- --
-- Result Analysis --

-- | Make a regression of measurements against a value of the measurements.
regress :: (Measured -> Double) -> V.Vector (V.Vector Measured) -> (Double, Double)
regress f v = first G.head (olsRegress [iters] target)
  where
    iters = convert (fromIntegral . G.length) v
    target = convert (sumOn f) v

-- -------------------------- --
-- Benchmark groups and setup --

-- | Character type used for benchmarking.
type Target = Word8

-- | Creates an `IO` that generatores a pair of strings, run the heuristic and run measuments on it.
measuring :: StringParameters -> (String, Heuristic Target) -> (String -> IO ()) -> IO Measured
measuring params heuristic writeLn = do
    pair <- generate (randomPairWith params)
    let result = measure heuristic pair
    writeLn (toCsvRow result)
    pure result

-- | Run a matrix of benchmarks for each parameter set and heuristic, writing output and results to
-- the with the input writers.
report :: PutStrLn -> PutStrLn -> IO ()
report putLn writeCsv = writeCsv csvHeader >> forM_ benchParams (forM_ heuristics . run)
  where
    run params heuristic = do
        putLn $ "benchmarking " ++ repr params ++ "/" ++ fst heuristic
        -- run benchmark and analyse results
        results <- runBenchmark $ measuring params heuristic writeCsv
        let (blks, r2) = regress (fromIntegral . blocks) results
        -- formatted output
        putLn $ "blocks:     \t" ++ show blks
        putLn $ "            \t" ++ show r2 ++ " R²"
        putLn $ "data points:\t" ++ show (G.length results)
        putLn ""

-- -------------------------- --
-- Output and saving to files --

-- | A line writer.
--
-- Functions that write a single line to some output handle.
type PutStrLn = String -> IO ()

-- Open a file, run a function that writes to the file using the custom writer and closes it.
withFileWriter :: String -> (Handle -> PutStrLn) -> (PutStrLn -> IO a) -> IO a
withFileWriter filename putLn run = withFile filename AppendMode (run . putLn)

-- | Print line and flush file.
hPutStrLn' :: Handle -> String -> IO ()
hPutStrLn' file s = hPutStrLn file s >> hFlush file

-- | Configure outputs and run the benchmarks.
main :: IO ()
main = do
    let outputDir = packageRoot </> "bench" </> "output"
    createDirectory outputDir
    timestamp <- getCurrentTimestamp
    -- CSV with the results of each execution
    let csvFile = outputDir </> timestamp ++ "-blocks" <.> "csv"
    -- the same output that is shown in stdout, but saved in a file
    let reportFile = outputDir </> timestamp ++ "-blocks-report" <.> "txt"

    withFileWriter reportFile pipeToStdout (withFileWriter csvFile hPutStrLn . report)
  where
    -- write a line to both a file and to stdout
    pipeToStdout file line = hPutStrLn' stdout line >> hPutStrLn' file line
