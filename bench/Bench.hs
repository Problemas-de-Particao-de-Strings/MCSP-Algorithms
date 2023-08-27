import Prelude hiding (String)

import Control.Monad (replicateM)
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, nf)
import Data.String qualified as Text

import Strings.Data.String (String)
import Strings.Heuristics.Combine (combineHeuristic, combineHeuristicS)
import Strings.System.Random (generate)

import TestLib.Heuristics (MCSPHeuristic, StringParameters (..), genStringPair, testHeuristic)

-- | Create a benchmark for one heuristic and one sample.
createBench :: [(String a, String a)] -> Text.String -> MCSPHeuristic a -> Benchmark
createBench sample name heuristic = bench name $ nf (testHeuristic heuristic) sample

-- | Create a benchmark group using string parameters and the size of sample.
createBenchGroup :: Text.String -> Int -> StringParameters -> IO Benchmark
createBenchGroup name size params = do
    sample <- generate $ replicateM size $ genStringPair params
    pure $ bgroup name (map (uncurry $ createBench sample) heuristics)
  where
    heuristics =
        [ ("combine", combineHeuristic)
        , ("combineS", combineHeuristicS)
        ]

main :: IO ()
main =
    defaultMain
        =<< sequence
            [ createBenchGroup
                "few-singletons"
                100
                StringParameters
                    { stringSize = 100
                    , alphabetSize = 2
                    , nSingletons = 5
                    }
            , createBenchGroup
                "lot-of-singletons"
                100
                StringParameters
                    { stringSize = 100
                    , alphabetSize = 2
                    , nSingletons = 60
                    }
            ]
