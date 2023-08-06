{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.Word (Word8)
import Prelude hiding (String)

import Strings

data Letter = A | C | G | T
    deriving stock (Show, Eq, Ord, Enum, Bounded)

derivingUnbox "Letter" [t|Letter -> Word8|] [|safeCast|] [|clampCast|]

gen :: IO (String Letter)
gen = generate 10

main :: IO ()
main = gen >>= print
