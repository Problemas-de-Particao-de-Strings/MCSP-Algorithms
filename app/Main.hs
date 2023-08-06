module Main (main) where

import Prelude hiding (String)

import Data.Word (Word8)
import System.Random.PCG.Fast (withSystemRandom)

import Strings.Data.Pair (Pair, shuffledPartitions)
import Strings.Data.String.Deriving (derivingUnbox, safeCasts)

data Letter = A | C | G | T
    deriving stock (Show, Eq, Ord, Enum, Bounded)

letterToByte :: Letter -> Word8
byteToLetter :: Word8 -> Letter
(letterToByte, byteToLetter) = safeCasts

derivingUnbox "Letter" [t|Letter -> Word8|] [|letterToByte|] [|byteToLetter|]

gen :: IO (Pair Letter)
gen = withSystemRandom $ shuffledPartitions 200

main :: IO ()
main = do
    (s1, s2) <- gen
    print s1
    print s2
