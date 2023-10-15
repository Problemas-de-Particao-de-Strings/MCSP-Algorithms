-- | Benchmark measuring running time of MCSP heuristics.
module Main (main) where

import Prelude

import Control.DeepSeq (NFData)
import Control.Monad (replicateM)
import Control.Monad.ST (stToIO)
import Criterion.Main (bench, bgroup, defaultMainWith, perRunEnv)
import Criterion.Types (Benchmark, Config (..), Verbosity (Verbose))
import Data.Vector.Unboxed.Mutable (MVector)
import Data.Word (Word32, Word8)
import GHC.Generics (Generic)
import Unsafe.Coerce (unsafeCoerce)

import MCSP.System.Path (createDirectory, getCurrentTimestamp, packageRoot, (<.>), (</>))
import MCSP.System.Random (
    Entropy (..),
    Lazy (..),
    MWC (..),
    PCG (..),
    PCGFast (..),
    PCGFastPure (..),
    PCGPure (..),
    Random,
    SeedableGenerator (..),
    evalRandom,
    mwcSeed,
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
{-# INLINE benchCase #-}

benchGen :: (Show g, NFData s) => g -> IO s -> (forall a. Random a -> s -> IO a) -> Benchmark
benchGen rng seed gen =
    bgroup
        (show rng)
        [ benchCase "1000 Float uniform" (replicateM 1000 uniform :: Random [Float]) seed gen,
          benchCase "1000 Double uniform" (replicateM 1000 uniform :: Random [Double]) seed gen,
          benchCase "1000 Int (0,100)" (replicateM 1000 (uniformR 0 100) :: Random [Int]) seed gen,
          benchCase "1000 Word8 (0,100)" (replicateM 1000 (uniformR 0 100) :: Random [Word8]) seed gen
        ]
{-# INLINE benchGen #-}

benchIt :: [Benchmark]
benchIt =
    [ benchGen PCG (evalLazy uniform) (runRandom PCG),
      benchGen PCGFast (evalLazy uniform) (runRandom PCGFast),
      benchGen PCGPure (evalLazy uniform) (runRandom PCGPure),
      benchGen PCGFastPure (evalLazy uniform) (runRandom PCGFastPure),
      benchGen MWC (evalLazy mwcSeed) (runRandom MWC)
    ]
  where
    evalLazy r = evalRandom r (Lazy Entropy)
    runRandom gen r seed = stToIO (initialize gen seed >>= evalRandom r)
{-# INLINE benchIt #-}

-- | Run a matrix of benchmarks for each parameter set and heuristic.
main :: IO ()
main = defaultMain benchIt
