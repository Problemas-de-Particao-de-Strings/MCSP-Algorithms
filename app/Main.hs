module Main (main) where

import Prelude hiding (String)

import Data.Store (Size (ConstSize), Store (size), decodeIO, encode)
import Data.Word (Word8)
import GHC.Generics (Generic)

import MCSP.Data.String (String)
import MCSP.Data.String.TH (derivingUnboxVia)
import MCSP.System.Random (generate)
import MCSP.TestLib.Random (randomShuffledBlocks)

data Letter = A | C | G | T
    deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance Store Letter where
    size = ConstSize 1

derivingUnboxVia [t|Letter -> Word8|]

gen :: IO (String Letter, String Letter)
gen = generate $ randomShuffledBlocks 200

roundTrip :: Store a => a -> IO a
roundTrip = decodeIO . encode

main :: IO ()
main = do
    (s1, s2) <- gen
    print s1
    print s2
    s2' <- roundTrip s2
    print s2'
