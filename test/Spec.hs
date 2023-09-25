module Main (main) where

import Data.Bifunctor (first)
import Data.Data (Typeable)
import Data.Function (($))
import Data.Functor ((<$>))
import System.IO (IO)

import Test.Tasty (TestTree, askOption, defaultMain, localOption, testGroup)
import Test.Tasty.Options (IsOption (..))
import Test.Tasty.QuickCheck (QuickCheckTests (..))

import MCSP.Tests.DocTest (testDocs)
import MCSP.Tests.RadixTree (radixTreeTests)
import MCSP.Tests.StringExtra (stringExtraTests)

main :: IO ()
main =
    defaultMain $
        modifyDefault $
            testGroup
                "Tests"
                [ radixTreeTests,
                  stringExtraTests,
                  testDocs "src"
                ]

newtype ModifiedQuickCheckTests = Tests QuickCheckTests
    deriving newtype (Typeable)

instance IsOption ModifiedQuickCheckTests where
    defaultValue = Tests (QuickCheckTests 1_000)
    parseValue text = Tests <$> parseValue text
    optionName = first Tests optionName
    optionHelp = first Tests optionHelp
    showDefaultValue (Tests t) = showDefaultValue t
    optionCLParser = Tests <$> optionCLParser

modifyDefault :: TestTree -> TestTree
modifyDefault tests = askOption $ \(Tests n) ->
    localOption n tests
