module MCSP.TestLib.Sample (
    StringParameters (..),
    genStringPair,
) where

import Prelude hiding (String)

import MCSP.Data.String (String)
import MCSP.System.Random (Random)
import MCSP.TestLib.Random (SimpleEnum, randomShuffledCharsWithSingletons)

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
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> import Data.Word (Word)
-- >>> generateWith (1,2) (genStringPair (StringParameters 10 2 2)) :: (String Word, String Word)
-- (0 0 1 3 1 0 2 1 0 1,3 1 0 0 0 0 1 2 1 1)
genStringPair :: forall a. SimpleEnum a => StringParameters -> Random (String a, String a)
genStringPair params =
    randomShuffledCharsWithSingletons
        (stringSize params)
        (fromMinBound 0)
        (fromMinBound (alphabetSize params - 1))
        (fromMinBound (alphabetSize params - 1 + nSingletons params))
  where
    fromMinBound n = toEnum (fromEnum (minBound :: a) + n)
