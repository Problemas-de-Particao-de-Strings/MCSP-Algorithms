module Main (main) where

import Data.Function (($))
import System.IO (IO)

import Test.Tasty (defaultMain, testGroup)

import MCSP.Tests.DocTest (testDocs)
import MCSP.Tests.RadixTree (radixTreeTests)

main :: IO ()
main =
    defaultMain $
        testGroup
            "Tests"
            [radixTreeTests, testDocs "src"]
