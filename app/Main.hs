{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Prelude hiding (String)

import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.Word (Word8)

import Strings

data Letter = A | C | G | T
    deriving stock (Show, Eq, Ord, Enum, Bounded)

derivingUnbox "Letter" [t|Letter -> Word8|] [|safeCast|] [|clampCast|]

gen :: IO (Pair Letter)
gen = generate $ shuffledPartitions 200

main :: IO ()
main = do
    (s1, s2) <- gen
    print s1
    print s2
