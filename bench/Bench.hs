import Prelude hiding (String)

import Control.Monad (replicateM)
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, nf)
import Data.String qualified as Text

import MCSP.Data.String (String)
import MCSP.System.Random (generate)
import MCSP.TestLib.Heuristics (Heuristic, heuristics, testHeuristic)
import MCSP.TestLib.Sample (StringParameters, benchParams, genStringPair, repr)

-- | Create a benchmark for one heuristic and one sample.
createBench :: [(String a, String a)] -> Text.String -> Heuristic a -> Benchmark
createBench sample name heuristic = bench name $ nf (testHeuristic heuristic) sample

-- | Create a benchmark group using string parameters and the size of sample.
createBenchGroup :: Int -> StringParameters -> IO Benchmark
createBenchGroup size params = do
    sample :: [(String Word, String Word)] <- generate $ replicateM size $ genStringPair params
    pure $ bgroup (repr params) (map (uncurry $ createBench sample) heuristics)

main :: IO ()
main = defaultMain =<< mapM (createBenchGroup 100) benchParams
