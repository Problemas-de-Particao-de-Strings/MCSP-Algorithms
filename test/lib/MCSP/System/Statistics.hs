module MCSP.System.Statistics (
    -- * Confidence Level
    cl90,
    cl95,
    cl99,
    mkCL,

    -- * Confidence Interval
    absolute,
    distCI,
    normalCI,
    sampleCI,
) where

import Data.Vector.Generic (length)
import GHC.Exts (Double)
import GHC.Float (sqrt)
import GHC.Num (abs, (-))
import GHC.Real (fromIntegral, (/))

import Statistics.Distribution (ContDistr, Mean, complQuantile, mean, quantile)
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Sample (meanVariance)
import Statistics.Types (
    CL,
    ConfInt,
    Estimate,
    Sample,
    cl90,
    cl95,
    cl99,
    confidenceInterval,
    estimateFromInterval,
    mkCL,
    significanceLevel,
 )

-- | The difference between upper and lower bound for an estimate.
--
-- >>> import Statistics.Distribution.Normal (standard)
-- >>> absolute (distCI (mkCL 0.6827) standard)
-- 2.0000434266459983
-- >>> absolute (sampleCI cl90 [10, 15, 20])
-- 7.753914357844494
absolute :: Estimate ConfInt Double -> Double
absolute estimate =
    let (lo, hi) = confidenceInterval estimate
     in abs (hi - lo)

-- | Calculates an estimate with confidence intervals for the given distribution.
--
-- >>> import Statistics.Distribution.Normal (standard)
-- >>> confidenceInterval (distCI (mkCL 0.6827) standard)
-- (-1.0000217133229992,1.0000217133229992)
-- >>> confidenceInterval (distCI cl95 standard)
-- (-1.9599639845400545,1.9599639845400545)
distCI :: (Mean d, ContDistr d) => CL Double -> d -> Estimate ConfInt Double
distCI cl dist = estimateFromInterval (mean dist) (lowerBound, upperBound) cl
  where
    cutoff = significanceLevel cl / 2
    lowerBound = quantile dist cutoff
    upperBound = complQuantile dist cutoff

-- | Calculates an estimate of a normal distribution from a sample.
--
-- >>> confidenceInterval (normalCI cl90 [10, 15, 20])
-- (8.284913187337764,21.715086812662236)
normalCI :: CL Double -> Sample -> Estimate ConfInt Double
normalCI cl sample = distCI cl (normalDistr u std)
  where
    (u, var) = meanVariance sample
    std = sqrt var

-- | Calculates an estimate for the expected value of a population given a sample.
--
-- Assumes a normal distribution.
--
-- >>> confidenceInterval (sampleCI cl90 [10, 15, 20])
-- (11.123042821077753,18.876957178922247)
sampleCI :: CL Double -> Sample -> Estimate ConfInt Double
sampleCI cl sample = distCI cl (normalDistr u std)
  where
    n = fromIntegral (length sample)
    (u, var) = meanVariance sample
    std = sqrt (var / n)
