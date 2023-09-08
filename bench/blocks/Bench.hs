-- | Benchmark measuring size of partitions created by MCSP heuristics.
module Main (main) where

import Prelude

import Control.Monad (forM_)
import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import Data.Tuple.Extra (both)
import Data.Vector qualified as V (Vector)
import Data.Vector.Generic qualified as G
import Data.Word (Word8)
import Numeric (showFFloat)
import Numeric.Extra (showDP)
import Statistics.Regression (bootstrapRegress, olsRegress)
import Statistics.Resampling (Estimator (Mean, StdDev), resample)
import Statistics.Resampling.Bootstrap (bootstrapBCA)
import Statistics.Types (CL, ConfInt, Estimate (Estimate), Sample, confidenceLevel)
import System.IO (Handle, IOMode (AppendMode), hFlush, hPutStrLn, stdout, withFile)
import System.Random.MWC (createSystemRandom)

import MCSP.System.Path (createDirectory, getCurrentTimestamp, packageRoot, (<.>), (</>))
import MCSP.System.Random (generate)
import MCSP.System.Statistics (absolute, cl99, confidenceInterval, sampleCI)
import MCSP.TestLib.Heuristics (
    Measured,
    NamedHeuristic,
    blocks,
    csvHeader,
    heuristics,
    measure,
    score,
    toCsvRow,
 )
import MCSP.TestLib.Sample (StringParameters, benchParams, randomPairWith, repr)

-- ---------------- --
-- Benchmark Limits --

-- | Maximum execution time of a benchmark in seconds.
timeLimit :: Double
timeLimit = 10.0
{-# INLINE timeLimit #-}

-- | Lower and upper limit on the number of runs for a single benchmark.
runs :: (Int, Int)
runs = (6, 60)
{-# INLINE runs #-}

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
runBenchmark = go (take maxRuns series) G.empty 0 G.empty
  where
    (minRuns, maxRuns) = (uncurry min runs, uncurry max runs)
    go [] _ _ acc _ = pure acc
    go (it : iters) scores runningTime acc m = do
        let run = G.length acc
        -- run the heuristic multiple times, measure time and collect the data
        startTime <- getSystemTime
        value <- G.replicateM it m
        endTime <- getSystemTime
        let totalElapsed = runningTime + elapsedSeconds startTime endTime
        -- update collected results, skip the first high variance results
        let scores' = scores G.++ convert score value
        let result = G.snoc acc value
        -- stop after either the maximum number of runs is reached or all of:
        -- \* a minimum number of runs is executed
        -- \* the time limit ran out and
        -- \* the confidence interval is less than 1% score
        if run >= minRuns && totalElapsed > timeLimit && estimateCI scores' < 0.01
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
    (lowerBound, upperBound) = both withUnit (confidenceInterval estimate)
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
measuring :: StringParameters -> NamedHeuristic Word8 -> IO Measured
measuring params heuristic = measure heuristic <$> generate (randomPairWith params)

-- | Writes the measured information in CSV format and returns it unchanged.
writeCsv :: PutStrLn -> Measured -> IO Measured
writeCsv writeLn result = writeLn (toCsvRow result) >> pure result

-- | Run a matrix of benchmarks for each parameter set and heuristic, writing output and results to
-- the with the input writers.
report :: PutStrLn -> PutStrLn -> IO ()
report printLn putRow = putRow csvHeader >> forM_ benchParams (forM_ heuristics . run)
  where
    run params heuristic = do
        printLn $ "benchmarking " ++ repr params ++ "/" ++ fst heuristic
        -- run benchmark and analyse results
        results <- runBenchmark $ measuring params heuristic >>= writeCsv putRow
        Estimated {..} <- regress blocks results
        -- formatted output
        printLn $ "blocks:     \t" ++ showEstimate "" linearCoefficient
        printLn $ "y-intercept:\t" ++ showEstimate "" yIntercept
        printLn $ "            \t" ++ showEstimate "R²" rSquared
        printLn $ "mean:       \t" ++ showEstimate "" mean
        printLn $ "std dev:    \t" ++ showEstimate "" stdDev
        let ci = " ci: " ++ showDP 3 (confidenceLevel confLevel)
        let n = G.length results
        printLn $ "data points:\t" ++ show n ++ "        \t" ++ ci
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
