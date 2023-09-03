module MCSP.TestLib.Sample (
    StringParameters (..),
    benchParams,
    randomPairWith,
    repr,
) where

import Prelude hiding (String)

import Data.String qualified as Text

import MCSP.Data.String (String)
import MCSP.System.Random (Random)
import MCSP.TestLib.Random (SimpleEnum, randomShuffledCharsWithSingletons)

-- | Parameters to generate a string of integers.
--
-- Alphabet size does not include the elements included as singletons.
data StringParameters = StringParameters
    { size :: Int,
      nReplicated :: Int,
      nSingletons :: Int
    }
    deriving stock (Show, Eq)

-- | Short and formatted representation of @StringParameters@.
--
-- >>> repr $ StringParameters 100 3 4
-- "(size=100 #rep=3 #sing=4)"
repr :: StringParameters -> Text.String
repr (StringParameters n reps sings) =
    "(size=" ++ show n ++ " #rep=" ++ show reps ++ " #sing=" ++ show sings ++ ")"

-- | Generate a pair of strings of integers using the given parameters.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> import Data.Word (Word)
-- >>> generateWith (1,2) $ randomPairWith (StringParameters 10 2 2) :: (String Word, String Word)
-- (0 0 1 3 1 0 2 1 0 1,3 1 0 0 0 0 1 2 1 1)
randomPairWith :: forall a. SimpleEnum a => StringParameters -> Random (String a, String a)
randomPairWith (StringParameters {..}) =
    randomShuffledCharsWithSingletons
        size
        (fromMinBound 0)
        (fromMinBound (nReplicated - 1))
        (fromMinBound (nReplicated - 1 + nSingletons))
  where
    fromMinBound n = toEnum (fromEnum (minBound :: a) + n)

-- | Parameters to generate strings for benchmarking.
benchParams :: [StringParameters]
benchParams =
    [ StringParameters {size = 100, nReplicated = 2, nSingletons = 5},
      StringParameters {size = 100, nReplicated = 2, nSingletons = 60}
    ]
