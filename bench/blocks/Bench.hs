-- | Benchmark measuring size of partitions created by MCSP heuristics.
module Main (main) where

import Prelude

import Control.Monad (forM_)
import Data.Data (Proxy (..))
import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import Data.Vector qualified as V (Vector)
import Data.Vector.Generic qualified as G
import Data.Word (Word8)
import Numeric (showFFloat)
import Numeric.Extra (showDP)
import Statistics.Regression (bootstrapRegress, olsRegress)
import Statistics.Resampling (Estimator (..), resample)
import Statistics.Resampling.Bootstrap (bootstrapBCA)
import Statistics.Types (CL, ConfInt, Estimate (..), Sample, confidenceLevel)
import System.IO (Handle, IOMode (AppendMode), hFlush, hPutStrLn, stdout, withFile)
import System.Random.MWC (createSystemRandom)

import MCSP.Data.Pair (both, ($:))
import MCSP.System.Path (createDirectory, getCurrentTimestamp, packageRoot, (<.>), (</>))
import MCSP.System.Random (generate)
import MCSP.System.Statistics (absolute, cl99, confidenceInterval, sampleCI)
import MCSP.TestLib.Heuristics (Debug, Measured, NamedHeuristic, blocks, heuristics, measure, score)
import MCSP.TestLib.Sample (SimpleEnum, StringParameters, benchParams, randomPairWith, repr)
import MCSP.Text.CSV (headers, row)

-- ---------------- --
-- Benchmark Limits --

-- | Maximum execution time of a benchmark in seconds.
timeLimit :: Double
timeLimit = 10.0
{-# INLINE timeLimit #-}

-- | Lower and upper limit on the number of runs for a single benchmark.
runs :: (Int, Int)
runs = (4, 60)
{-# INLINE runs #-}

-- | Number of initial data points to be ignored.
--
-- Data points with small number of iterations are much more susceptible to high variance, yet they
-- have the same weight in linear regression. This is a way of reducing their impact.
skip :: Int
skip = 0
{-# INLINE skip #-}

-- | Confidence level expected for each benchmark.
confLevel :: CL Double
confLevel = cl99
{-# INLINE confLevel #-}

-- | Number of resamples for bootstrapping regression.
resamples :: Int
resamples = 1000
{-# INLINE resamples #-}

-- ----------------------------- --
-- Benchmarking and Measurements --

-- | The x-axis of the regression, counting the number of iterations to run for each data point.
series :: Integral a => [a]
series = squish $ map truncate $ iterate @Double (ratio *) 1
  where
    ratio = 1.05
    squish = foldr dropRepeated []
    dropRepeated x xs = x : dropWhile (x ==) xs

-- | Yield the elements wth its index in the list, starting from the given integer.
indexed :: Int -> [a] -> [(Int, a)]
indexed _ [] = []
indexed i (x : xs) = (i, x) : indexed (i + 1) xs

-- | Evaluate each element in a vector and sum the results.
sumOn :: Num b => (a -> b) -> V.Vector a -> b
sumOn f = G.foldl' (\s m -> s + f m) 0

-- | Converts all the values and the output vector type.
convert :: (G.Vector v a, G.Vector w b) => (a -> b) -> v a -> w b
convert f vec = G.generate (G.length vec) (f . G.unsafeIndex vec)

-- | Calculates elapsed seconds as a float, from UNIX timestamps.
elapsedSeconds :: SystemTime -> SystemTime -> Double
elapsedSeconds s e = max (secs + nsecs * 1e-9) 0
  where
    secs = fromIntegral (systemSeconds e - systemSeconds s)
    nsecs = fromIntegral (systemNanoseconds e - systemNanoseconds s)

-- | Estimate the confidence interval via traditional inference statistics.
estimateCI :: Sample -> Double
estimateCI = absolute . sampleCI confLevel

-- | Run a single benchmark until the estimated confidence interval is low enough for `confLevel`.
--
-- Returns a vector of data points (@`V.Vector` `Measured`@), such that the x-axis is the number of
-- iterations for that point (given by `G.length`) and the y-axis should be a fold over the
-- measurements in the data fold.
runBenchmark :: IO Measured -> IO (V.Vector (V.Vector Measured))
runBenchmark = go (indexed 1 $ take (max $: runs) series) G.empty 0 G.empty
  where
    go [] _ _ acc _ = pure acc
    go ((run, it) : iters) scores runningTime acc m = do
        -- run the heuristic multiple times, measure time and collect the data
        startTime <- getSystemTime
        value <- G.replicateM it m
        endTime <- getSystemTime
        let totalElapsed = runningTime + elapsedSeconds startTime endTime
        -- update collected results, skip the first high variance results
        let (scores', result) =
                if run > skip
                    then (scores G.++ convert score value, G.snoc acc value)
                    else (scores, acc)
        -- stop after either the maximum number of runs is reached or all of:
        -- \* a minimum number of runs is executed
        -- \* the time limit ran out and
        -- \* the confidence interval is less than 1% score
        if run >= (min $: runs) && (totalElapsed > timeLimit || estimateCI scores' < 0.01)
            then pure result
            else go iters scores' totalElapsed result m

-- --------------- --
-- Result Analysis --

-- | A bunch of processed information about the collected measurements.
data Estimated = Estimated
    { -- | Angular coefficient of the linear regression.
      linearCoefficient :: Estimate ConfInt Double,
      -- | Vertical intercept for the regressed line.
      yIntercept :: Estimate ConfInt Double,
      -- | Correlation coefficient squared.
      rSquared :: Estimate ConfInt Double,
      -- | Intrinsic mean of the heuristic.
      mean :: Estimate ConfInt Double,
      -- | Intrinsic standard deviation of the heuristic.
      stdDev :: Estimate ConfInt Double
    }

-- | Format a value with unit and confidence interval.
showEstimate :: String -> Estimate ConfInt Double -> String
showEstimate unit estimate@(Estimate value _) =
    withUnit value ++ "\t(" ++ lowerBound ++ " .. " ++ upperBound ++ ")"
  where
    (lowerBound, upperBound) = withUnit `both` confidenceInterval estimate
    -- formatting numbers and strings
    withUnit n = fixed n ++ " " ++ withWidth 2 ' ' unit
    fixed n = withWidth 5 '0' (showFFloat Nothing n "")
    withWidth 0 _ _ = ""
    withWidth n pad "" = replicate n pad
    withWidth n pad (x : xs) = x : withWidth (n - 1) pad xs

-- | Runs linear regression against a measured field of the collected measurements.
--
-- Returns the estimated information about the data.
regress :: Real a => (m -> a) -> V.Vector (V.Vector m) -> IO Estimated
regress f v = do
    rng <- createSystemRandom
    -- linear regression
    let iters = convert (fromIntegral . G.length) v
    let target = convert (sumOn field) v
    (coefs, r2) <- bootstrapRegress rng resamples confLevel olsRegress [iters] target
    (linear, intercept) <- unwrap "bootstrapRegress" coefs
    -- mean and standard deviation
    let values = G.convert (G.concatMap (G.map field) v)
    samples <- resample rng [Mean, StdDev] resamples values
    let estimates = bootstrapBCA confLevel values samples
    (mean, std) <- unwrap "bootstrapBCA" estimates
    pure
        Estimated
            { linearCoefficient = linear,
              yIntercept = intercept,
              rSquared = r2,
              mean = mean,
              stdDev = std
            }
  where
    field = fromRational . toRational . f
    -- used in bootstrap, where the number of parameters is know to be two
    unwrap _ [x, y] = pure (x, y)
    unwrap loc _ = fail ("unexpected mismatch in " ++ loc ++ " results")

-- -------------------------- --
-- Benchmark groups and setup --

-- | Creates an `IO` that generatores a pair of strings, run the heuristic and run measuments on it.
measuring :: (SimpleEnum a, Debug a) => StringParameters -> NamedHeuristic a -> IO Measured
measuring params heuristic = generate (randomPairWith params) >>= measure heuristic

-- | Writes the measured information in CSV format and returns it unchanged.
writeCsv :: PutStrLn -> Measured -> IO Measured
writeCsv writeLn result = writeLn (row result) >> pure result

-- | Run a matrix of benchmarks for each parameter set and heuristic, writing output and results to
-- the with the input writers.
report :: PutStrLn -> PutStrLn -> IO ()
report printLn putRow = putRow csvHeaders >> forM_ benchParams (forM_ heuristics . run)
  where
    csvHeaders = headers (Proxy @Measured)
    run params heuristic = do
        printLn $ "benchmarking " ++ repr params ++ "/" ++ fst heuristic
        -- run benchmark and analyse results
        results <- runBenchmark $ measuring @Word8 params heuristic >>= writeCsv putRow
        Estimated {..} <- regress blocks results
        -- formatted output
        printLn $ "blocks:     \t" ++ showEstimate "" linearCoefficient
        printLn $ "y-intercept:\t" ++ showEstimate "" yIntercept
        printLn $ "            \t" ++ showEstimate "R²" rSquared
        printLn $ "mean:       \t" ++ showEstimate "" mean
        printLn $ "std dev:    \t" ++ showEstimate "" stdDev
        let ci = " ci: " ++ showDP 3 (confidenceLevel confLevel)
        let n = G.length results
        printLn $ "data points:\t" ++ show n ++ " (" ++ show (n + skip) ++ ")        \t" ++ ci
        printLn ""

-- -------------------------- --
-- Output and saving to files --

-- | A line writer.
--
-- Functions that write a single line to some output handle.
type PutStrLn = String -> IO ()

-- Open a file, run a function that writes to the file using the custom writer and closes it.
withFileWriter :: String -> (Handle -> f) -> (f -> IO a) -> IO a
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
