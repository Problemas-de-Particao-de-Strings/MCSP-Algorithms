-- | Benchmark measuring size of partitions created by MCSP heuristics.
module Main (main) where

import Prelude

import Control.Monad (forM_)
import Data.Tuple.Extra (both)
import Data.Vector qualified as V (Vector)
import Data.Vector.Generic qualified as G
import Data.Word (Word8)
import Numeric (showFFloat)
import Numeric.Extra (showDP)
import Statistics.Regression (bootstrapRegress, olsRegress)
import Statistics.Types (CL, ConfInt, Estimate (Estimate), confidenceInterval, confidenceLevel)
import System.IO (Handle, IOMode (AppendMode), hFlush, hPutStrLn, stdout, withFile)
import System.Random.MWC (createSystemRandom)

import MCSP.System.Path (createDirectory, getCurrentTimestamp, packageRoot, (<.>), (</>))
import MCSP.System.Random (generate)

import MCSP.System.Statistics (absolute, cl99, sampleCI)
import MCSP.TestLib.Sample (StringParameters, benchParams, randomPairWith, repr)

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

-- ----------------------------- --
-- Benchmarking and Measurements --

-- | Confidence level expected for each benchmark.
confLevel :: CL Double
confLevel = cl99
{-# INLINE confLevel #-}

-- | Number of resamples for bootstrapping regression.
resamples :: Int
resamples = 1000

-- | Lower and upper limit on the number of runs for a single benchmark.
runs :: (Int, Int)
runs = (5, 40)
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
    go (it : iters) scores acc m = do
        -- current iteration index
        let run = G.length acc + 1

        -- run the heuristic multiple times and collect the measurements
        value <- G.replicateM it m
        let scores' = scores G.++ convert score value

        let result = G.snoc acc value
        -- stop after a minimum number of runs and the confidence interval is smaller than 1% score
        if run >= min $: runs && absolute (sampleCI confLevel scores') < 0.01
            then pure result
            else go iters scores' result m

-- --------------- --
-- Result Analysis --

-- | Non-Generic version of `Estimate`.
type Estimated = Estimate ConfInt Double

-- | Format a value with unit and confidence interval.
showEstimate :: String -> Estimated -> String
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

-- | Make a regression of measurements against a value of the measurements.
--
-- Returns both coefficients @(a, b)@ (for @y = a x + b@) of the regression and the R^2.
regress :: Real a => (m -> a) -> V.Vector (V.Vector m) -> IO ((Estimated, Estimated), Estimated)
regress f v = do
    rng <- createSystemRandom
    (coef, r2) <- bootstrapRegress rng resamples confLevel olsRegress [iters] target
    case coef of
        [a, b] -> pure ((a, b), r2)
        _ -> fail "unexpected mismatch in bootstrapRegress parameters"
  where
    iters = convert (fromIntegral . G.length) v
    target = convert (sumOn (fromRational . toRational . f)) v

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
        ((blks, _), r2) <- regress blocks results
        -- formatted output
        printLn $ "blocks:     \t" ++ showEstimate "" blks
        printLn $ "            \t" ++ showEstimate "R²" r2
        let ci = showDP 3 (confidenceLevel confLevel)
        printLn $ "data points:\t" ++ show (G.length results) ++ ", ci = " ++ ci
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
