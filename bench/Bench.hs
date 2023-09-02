import Prelude hiding (String)

import Control.Monad (replicateM)
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, nf)
import Data.String qualified as Text

import MCSP.Data.String (String)
import MCSP.Heuristics (Heuristic, combine, combineS)
import MCSP.System.Random (generate)
import MCSP.TestLib.Heuristics (testHeuristic)
import MCSP.TestLib.Sample (StringParameters (..), genStringPair)

-- | Create a benchmark for one heuristic and one sample.
createBench :: [(String a, String a)] -> Text.String -> Heuristic a -> Benchmark
createBench sample name heuristic = bench name $ nf (testHeuristic heuristic) sample

-- | Create a benchmark group using string parameters and the size of sample.
createBenchGroup :: Text.String -> Int -> StringParameters -> IO Benchmark
createBenchGroup name size params = do
    sample :: [(String Word, String Word)] <- generate $ replicateM size $ genStringPair params
    pure $ bgroup name (map (uncurry $ createBench sample) heuristics)
  where
    heuristics =
        [ ("combine", combine),
          ("combineS", combineS)
        ]

main :: IO ()
main =
    defaultMain
        =<< sequence
            [ createBenchGroup
                "few-singletons"
                100
                StringParameters
                    { stringSize = 100,
                      alphabetSize = 2,
                      nSingletons = 5
                    },
              createBenchGroup
                "lot-of-singletons"
                100
                StringParameters
                    { stringSize = 100,
                      alphabetSize = 2,
                      nSingletons = 60
                    }
            ]
