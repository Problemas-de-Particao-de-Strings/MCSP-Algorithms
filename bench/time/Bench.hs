-- | Benchmark measuring running time of MCSP heuristics.
module Main (main) where

import Prelude

import Control.DeepSeq (NFData)
import Control.Monad (replicateM)
import Control.Monad.ST (stToIO)
import Criterion.Main (bench, bgroup, defaultMainWith, perRunEnv)
import Criterion.Types (Benchmark, Config (..), Verbosity (Verbose))
import Data.Vector.Unboxed qualified as Vector (replicateM)
import Data.Word (Word8)

import MCSP.System.Path (createDirectory, getCurrentTimestamp, packageRoot, (<.>), (</>))
import MCSP.System.Random (
    Entropy (..),
    HWEntropy (..),
    Lazy (..),
    MWC (..),
    PCG (..),
    PCGFast (..),
    PCGFastPure (..),
    PCGPure (..),
    PCGSingle (..),
    PCGUnique (..),
    Random,
    SeedableGenerator (..),
    evalRandom,
    uniform,
    uniformR,
 )
import MCSP.System.Statistics (cl95)

-- | Generates a configuration for Criterion that saves the outputs by default.
--
-- >>> import Data.Maybe (fromJust)
-- >>> lastN n = reverse . take n . reverse
-- >>> lastN 74 <$> fromJust <$> reportFile <$> getDefaultConfig
-- "MCSP-Algorithms/bench/output/time-2023-09-06T00:20:21.292946511-03:00.html"
getDefaultConfig :: IO Config
getDefaultConfig = do
    let outputDir = packageRoot
    createDirectory outputDir
    timestamp <- getCurrentTimestamp
    pure
        Config
            { confInterval = cl95,
              timeLimit = 10,
              resamples = 1000,
              regressions = [],
              rawDataFile = Nothing,
              reportFile = Just (outputDir </> timestamp ++ "-time-report" <.> "html"),
              csvFile = Just (outputDir </> timestamp ++ "-time" <.> "csv"),
              jsonFile = Nothing,
              junitFile = Nothing,
              verbosity = Verbose,
              template = packageRoot </> "bench" </> "time" </> "report-template.tpl"
            }

-- | Run Criterion with a default configuration from `getDefaultConfig`.
defaultMain :: [Benchmark] -> IO ()
defaultMain benchmarks = do
    config <- getDefaultConfig
    defaultMainWith config benchmarks

benchCase ::
    (NFData a, NFData s) =>
    String
    -> Random a
    -> IO s
    -> (Random a -> s -> IO a)
    -> Benchmark
benchCase name rand seed gen = bench name (perRunEnv seed (gen rand))

benchGen :: (Show g, NFData s) => g -> IO s -> (forall a. Random a -> s -> IO a) -> Benchmark
benchGen rng seed gen =
    bgroup
        (show rng)
        [ benchCase "10 Int uniform" (replicateM 10 uniform :: Random [Int]) seed gen,
          benchCase "100 Int uniform" (replicateM 100 uniform :: Random [Int]) seed gen,
          benchCase "1000 Int uniform" (replicateM 1000 uniform :: Random [Int]) seed gen,
          benchCase "10 Word8 uniform" (replicateM 10 uniform :: Random [Word8]) seed gen,
          benchCase "100 Word8 uniform" (replicateM 100 uniform :: Random [Word8]) seed gen,
          benchCase "1000 Word8 uniform" (replicateM 1000 uniform :: Random [Word8]) seed gen,
          benchCase "10 Float uniform" (replicateM 10 uniform :: Random [Float]) seed gen,
          benchCase "100 Float uniform" (replicateM 100 uniform :: Random [Float]) seed gen,
          benchCase "1000 Float uniform" (replicateM 1000 uniform :: Random [Float]) seed gen,
          benchCase "10 Double uniform" (replicateM 10 uniform :: Random [Double]) seed gen,
          benchCase "100 Double uniform" (replicateM 100 uniform :: Random [Double]) seed gen,
          benchCase "1000 Double uniform" (replicateM 1000 uniform :: Random [Double]) seed gen,
          benchCase "10 Int (0,100)" (replicateM 10 (uniformR 0 100) :: Random [Int]) seed gen,
          benchCase "100 Int (0,100)" (replicateM 100 (uniformR 0 100) :: Random [Int]) seed gen,
          benchCase "1000 Int (0,100)" (replicateM 1000 (uniformR 0 100) :: Random [Int]) seed gen,
          benchCase "10 Word8 (0,100)" (replicateM 10 (uniformR 0 100) :: Random [Word8]) seed gen,
          benchCase "100 Word8 (0,100)" (replicateM 100 (uniformR 0 100) :: Random [Word8]) seed gen,
          benchCase "1000 Word8 (0,100)" (replicateM 1000 (uniformR 0 100) :: Random [Word8]) seed gen
        ]

benchIt :: [Benchmark]
benchIt =
    [ benchGen Entropy (pure Entropy) evalRandom,
      benchGen HWEntropy (pure HWEntropy) evalRandom,
      benchGen MWC mwcSeed (runRandom MWC),
      benchGen PCG (evalRandom uniform Entropy) (runRandom PCG),
      benchGen PCGPure (evalRandom uniform Entropy) (runRandom PCGPure),
      benchGen PCGFast (evalRandom uniform Entropy) (runRandom PCGFast),
      benchGen PCGFastPure (evalRandom uniform Entropy) (runRandom PCGFastPure),
      benchGen PCGSingle (evalRandom uniform Entropy) (runRandom PCGSingle),
      benchGen PCGUnique (evalRandom uniform Entropy) (evalIO PCGUnique),
      benchGen (Lazy Entropy) (pure (Lazy Entropy)) evalRandom,
      benchGen (Lazy HWEntropy) (pure (Lazy HWEntropy)) evalRandom,
      benchGen (Lazy MWC) mwcSeed (runRandom (Lazy MWC)),
      benchGen (Lazy PCG) (evalRandom uniform Entropy) (runRandom (Lazy PCG)),
      benchGen (Lazy PCGPure) (evalRandom uniform Entropy) (runRandom (Lazy PCGPure)),
      benchGen (Lazy PCGFast) (evalRandom uniform Entropy) (runRandom (Lazy PCGFast)),
      benchGen (Lazy PCGFastPure) (evalRandom uniform Entropy) (runRandom (Lazy PCGFastPure)),
      benchGen (Lazy PCGSingle) (evalRandom uniform Entropy) (runRandom (Lazy PCGSingle)),
      benchGen (Lazy PCGUnique) (evalRandom uniform Entropy) (evalIO (Lazy PCGUnique))
    ]
  where
    evalIO gen r s = initialize gen s >>= evalRandom r
    runRandom gen r seed = stToIO (initialize gen seed >>= evalRandom r)
    mwcSeed = evalRandom (Vector.replicateM 258 uniform) Entropy

-- | Run a matrix of benchmarks for each parameter set and heuristic.
main :: IO ()
main = defaultMain benchIt
