module Main (main) where

import Data.Function (($))
import System.IO (IO)

import Test.Tasty (defaultMain, testGroup)

import MCSP.Tests.DocTest (testDocs)
import MCSP.Tests.RadixTree (radixTreeTests)
import MCSP.Tests.StringExtra (stringExtraTests)

main :: IO ()
main =
    defaultMain $
        testGroup
            "Tests"
            [ radixTreeTests,
              stringExtraTests,
              testDocs "src"
            ]
