module Main (main) where

import Prelude hiding (String)

import Data.Store (Size (ConstSize), Store (size), decodeIO, encode)
import Data.Word (Word8)

import Strings.Data.Pair (Pair, shuffledPartitions)
import Strings.Data.String.Deriving (Generic, derivingUnbox, safeCasts)
import Strings.System.Random (generate)

data Letter = A | C | G | T
    deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance Store Letter where
    size = ConstSize 1

letterToByte :: Letter -> Word8
byteToLetter :: Word8 -> Letter
(letterToByte, byteToLetter) = safeCasts

derivingUnbox "Letter" [t|Letter -> Word8|] [|letterToByte|] [|byteToLetter|]

gen :: IO (Pair Letter)
gen = generate $ shuffledPartitions 200

roundTrip :: Store a => a -> IO a
roundTrip = decodeIO . encode

main :: IO ()
main = do
    (s1, s2) <- gen
    print s1
    print s2
    s2' <- roundTrip s2
    print s2'
