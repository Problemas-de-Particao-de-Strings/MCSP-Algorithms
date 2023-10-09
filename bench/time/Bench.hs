-- | Benchmark measuring running time of MCSP heuristics.
module Main (main) where

import Prelude hiding (String, head, lookup)

import Control.DeepSeq (NFData, force)
import Control.Monad (forM)
import Criterion.Main (bench, bgroup, defaultMainWith, perRunEnvWithCleanup)
import Criterion.Types (Benchmark, Config (..), Verbosity (Verbose))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.IntMap.Strict qualified as IntMap (IntMap, empty, insertWith, lookup)
import Data.List.NonEmpty (head)
import Data.Map.Strict qualified as Map (Map, empty, insertWith, lookup)
import Data.String qualified as Text (String)
import Data.Word (Word8)
import GHC.Generics (Generic)

import MCSP.Data.MatchingGraph (edgeSet, mergeness, solution, solutions, toPartitions)
import MCSP.Data.Pair (Pair, second, (&&&))
import MCSP.Data.String (String)
import MCSP.System.Path (createDirectory, getCurrentTimestamp, packageRoot, (<.>), (</>))
import MCSP.System.Random (generate, shuffle)
import MCSP.System.Statistics (cl99)
import MCSP.TestLib.Sample (StringParameters, benchParams, randomPairWith, repr)

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
            { confInterval = cl99,
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

lookupOrInsert ::
    (k -> m -> Maybe v)
    -> ((v -> v -> v) -> k -> v -> m -> m)
    -> k
    -> IO v
    -> IORef m
    -> IO v
lookupOrInsert lookup insert key create cacheRef = do
    cache <- readIORef cacheRef
    case lookup key cache of
        Just local -> pure local
        Nothing -> do
            value <- create
            result <- atomicModifyIORef' cacheRef $ \oldCache ->
                let newCache = insert (\_ x -> x) key value oldCache
                 in (newCache, lookup key newCache)
            maybe (fail "lookupOrInsert: key could not be inserted") pure result

type LocalCache v = IORef (IntMap.IntMap v)

type GlobalCache k v = IORef (Map.Map k (LocalCache v))

lookupGlobal :: Ord k => k -> GlobalCache k v -> IO (LocalCache v)
lookupGlobal key = lookupOrInsert Map.lookup Map.insertWith key (newIORef IntMap.empty)

lookupLocal :: Int -> IO v -> LocalCache v -> IO v
lookupLocal = lookupOrInsert IntMap.lookup IntMap.insertWith

lookupCache :: Ord k => (k -> Int -> IO v) -> GlobalCache k v -> k -> Int -> IO v
lookupCache create cache key i = lookupGlobal key cache >>= lookupLocal i (create key i)

insertCache :: Ord k => GlobalCache k v -> k -> Int -> v -> IO v
insertCache cache key i value = lookupGlobal key cache >>= lookupLocal i (pure value)

type CacheRestore k v = k -> Int -> IO v
type CacheSave k v = k -> Int -> v -> IO v

withCache :: Ord k => (k -> Int -> IO v) -> IO (CacheRestore k v, CacheSave k v)
withCache create = do
    cache <- newIORef Map.empty
    pure (lookupCache create cache, insertCache cache)

withId :: (Int -> IO a) -> IO (IO a)
withId run = do
    ids <- newIORef 0
    pure (atomicModifyIORef' ids (\i -> (i + 1, i)) >>= run)

data BenchResult a b = Result
    { idx :: Int,
      value :: a,
      output :: Maybe b
    }
    deriving stock (Generic)

-- | Creates a benchmark group running each heuristic against the given parameters.
benchWithParams ::
    (NFData a, NFData b) =>
    Text.String
    -> (a -> b)
    -> CacheRestore StringParameters a
    -> CacheSave StringParameters b
    -> IO Benchmark
benchWithParams name run getInput saveOutput = do
    benchmarks <- forM benchParams $ \params -> do
        input <- withId $ \idx -> do
            value <- getInput params idx
            newIORef Result {idx = force idx, value = force value, output = Nothing}

        pure $ bench (repr params) $ perRunEnvWithCleanup input (save params) $ \result -> do
            Result {value} <- readIORef result
            let output = force (run value)
            atomicModifyIORef' result (\r -> (r {output = Just output}, output))

    pure (bgroup name benchmarks)
  where
    save params result = do
        Result {idx, output} <- readIORef result
        value <- maybe (fail "benchWithParams: oh oh") pure output
        _ <- saveOutput params idx value
        pure ()

ignore :: CacheSave k v
ignore _ _ = pure

-- | Run a matrix of benchmarks for each parameter set and heuristic.
main :: IO ()
main = do
    (getPair, _) <- withCache $ \k _ ->
        genStrings k
    (getEdges, saveEdge) <- withCache $ \k i -> do
        pair <- getPair k i
        edges <- generate (shuffle $ edgeSet pair)
        pure (pair, edges)
    (getSolution, saveSolution) <- withCache $ \k i -> do
        (pair, edges) <- getEdges k i
        pure (pair, solution edges)

    tests <-
        sequence
            [ benchWithParams "edgeSet" (id &&& edgeSet) getPair saveEdge,
              benchWithParams "solution" (second solution) getEdges saveSolution,
              benchWithParams "head solutions" (second (head . solutions)) getEdges saveSolution,
              benchWithParams "solutions" (solutions . snd) getEdges ignore,
              benchWithParams "mergeness" (mergeness . snd) getSolution ignore,
              benchWithParams "toPartitions" (uncurry toPartitions) getSolution ignore
            ]
    defaultMain tests
  where
    genStrings params = generate (randomPairWith params) :: IO (Pair (String Word8))
