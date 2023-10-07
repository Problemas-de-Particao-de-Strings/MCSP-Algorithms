module MCSP.System.TimeIt (timeIt) where

import Control.Applicative (pure)
import Control.DeepSeq (NFData, force, rnf)
import Control.Monad (fail, (>=>), (>>=))
import Data.Fixed (Pico)
import Data.Function ((.))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (Maybe (..), maybe)
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Word (Word8)
import GHC.Base (seq)
import GHC.Num ((-))
import System.IO (IO)

-- | Generate a function which applies an argument to a function a given
-- number of times, running its action and reducing the result to normal form.
nfAppIO' :: (b -> ()) -> (a -> IO b) -> a -> (Word8 -> IO ())
nfAppIO' reduce f v = go
  where
    go 0 = pure ()
    go n = do
        x <- f v
        reduce x `seq` go (n - 1)
-- from https://github.com/haskell/criterion/blob/master/criterion-measurement/src/Criterion/Measurement/Types/Internal.hs
{-# NOINLINE nfAppIO' #-}

-- | Run an IO function and measure its wall clock execution time.Debug
--
-- This might not be super precise, but should be ok for functions that take 100 ms or longer.
--
-- >>> import System.IO (print)
-- >>> timeIt print 12
-- (0.000025988000,())
timeIt :: (NFData a, NFData b) => (a -> IO b) -> a -> IO (Pico, b)
timeIt fun (force -> value) = do
    output <- newIORef Nothing
    start <- getCurrentTime
    nfAppIO' rnf (fun >=> writeIORef output . pure . force) value 1
    end <- getCurrentTime

    result <- readIORef output >>= maybe (fail "timeIt: failed to recover output") pure
    let elapsed = nominalDiffTimeToSeconds (end `diffUTCTime` start)
    pure (elapsed, result)
{-# NOINLINE timeIt #-}
