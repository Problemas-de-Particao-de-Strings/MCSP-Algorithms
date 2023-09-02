module MCSP.TestLib.Sample (
    StringParameters (..),
    genStringPair,
) where

import Prelude hiding (String)

import MCSP.Data.String (String)
import MCSP.System.Random (Random)
import MCSP.TestLib.Random (randomShuffledCharsWithSingletons)

-- | Parameters to generate a string of integers.
--
-- Alphabet size does not include the elements included as singletons.
data StringParameters = StringParameters
    { stringSize :: Int,
      alphabetSize :: Int,
      nSingletons :: Int
    }
    deriving stock (Eq, Show)

-- | Generate a pair of strings of integers using the given parameters.
genStringPair :: StringParameters -> Random (String Int, String Int)
genStringPair params =
    randomShuffledCharsWithSingletons
        (stringSize params)
        1
        (alphabetSize params)
        [3 .. (2 + nSingletons params)]
