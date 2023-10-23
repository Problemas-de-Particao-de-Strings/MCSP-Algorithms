module MCSP.TestLib.Sample (
    ShuffleMethod (..),
    StringParameters (..),
    SimpleEnum,
    benchParams,
    randomPairWith,
    repr,
) where

import Prelude hiding (String)

import Data.String qualified as Text

import MCSP.Data.String (String)
import MCSP.System.Random (Random)
import MCSP.TestLib.Random (
    SimpleEnum,
    pairShufflingBlocks,
    pairShufflingChars,
    randomWithSingletons,
 )

-- | Method of shuffling a string.
data ShuffleMethod = Chars | Blocks
    deriving stock (Show, Eq)

-- | Parameters to generate a string of integers.
--
-- Alphabet size does not include the elements included as singletons.
data StringParameters = StringParameters
    { size :: Int,
      nReplicated :: Int,
      nSingletons :: Int,
      shuffle :: ShuffleMethod
    }
    deriving stock (Show, Eq)

-- | Short and formatted representation of @StringParameters@.
--
-- >>> repr $ StringParameters 100 3 4 Chars
-- "(size=100 #rep=3 #sing=4 shuffle=Chars)"
repr :: StringParameters -> Text.String
repr (StringParameters n reps sings shuffle) =
    parenthesized
        [ "size=" ++ show n,
          "#rep=" ++ show reps,
          "#sing=" ++ show sings,
          "shuffle=" ++ show shuffle
        ]
  where
    parenthesized ws = "(" ++ unwords ws ++ ")"

-- | Generate a pair of strings of integers using the given parameters.
--
-- >>> import MCSP.System.Random (generateWith)
-- >>> import Data.Word (Word8)
-- >>> generateWith (1,2) $ randomPairWith @Word8 (StringParameters 10 2 2 Chars)
-- (0 0 0 0 1 3 1 0 1 2,1 1 0 1 2 0 0 3 0 0)
randomPairWith :: forall a. SimpleEnum a => StringParameters -> Random (String a, String a)
randomPairWith (StringParameters {..}) = do
    str <-
        randomWithSingletons
            size
            (fromMinBound 0)
            (fromMinBound (nReplicated - 1))
            (fromMinBound (nReplicated - 1 + nSingletons))
    case shuffle of
        Chars -> pairShufflingChars str
        Blocks -> pairShufflingBlocks str
  where
    fromMinBound n = toEnum (fromEnum @a minBound + n)

-- | Parameters to generate strings for benchmarking.
benchParams :: [StringParameters]
benchParams =
    [ StringParameters {size = 60, nReplicated = 5, nSingletons = 5, shuffle = Chars},
      StringParameters {size = 60, nReplicated = 5, nSingletons = 30, shuffle = Chars}
    ]
