module MCSP.Tests.DocTest (
    testDocs,
) where

import Control.Applicative (pure)
import Data.Bool (Bool (..), otherwise, (||))
import Data.Data (Typeable)
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.List (filter, isPrefixOf, nub, (++))
import Data.Maybe (Maybe (..))
import Data.Ord ((>))
import Data.Proxy (Proxy (..))
import Data.String qualified as Text (String)
import System.FilePath (takeExtension)
import System.IO (FilePath, IO)
import Text.Show (show)

import Test.DocTest.Internal.Parse (extractDocTests)
import Test.DocTest.Internal.Run (Config (..), Summary (..), runDocTests)
import Test.Tasty (DependencyType (..), TestName, after_)
import Test.Tasty.Options (
    IsOption (..),
    OptionDescription (..),
    OptionSet,
    flagCLParser,
    lookupOption,
    safeReadBool,
 )
import Test.Tasty.Patterns.Types (Expr (..))
import Test.Tasty.Providers (IsTest (..), TestTree, singleTest, testFailed, testPassed)

import MCSP.System.Path (expandFiles)
import MCSP.System.Repl (getStackFlags, stackRepl)

testDocs :: FilePath -> TestTree
testDocs path = afterOthers path $ DocTest $ do
    files <- expandFiles path
    pure (filter isHaskellFile files)
  where
    isHaskellFile file = takeExtension file == ".hs"

afterOthers :: IsTest t => TestName -> t -> TestTree
afterOthers name test =
    after_
        AllFinish
        (Field (IntLit 2) `NE` StringLit name)
        (singleTest name test)

newtype DocTest = DocTest (IO [FilePath])
    deriving newtype (Typeable)

instance IsTest DocTest where
    testOptions = pure docTestOptions
    run opts (DocTest getFiles) _ = do
        let initialConfig = setOptions opts defaultConfig
        -- TODO: catch errors
        extensions <- filter ("-X" `isPrefixOf`) <$> getStackFlags
        targetFiles <- getFiles
        let config = appendFlags (extensions ++ targetFiles) initialConfig
        docTests <- extractDocTests (ghcOptions config)
        fmap analyze (runDocTests config docTests)
      where
        analyze s@Summary {..}
            | sFailures > 0 || sErrors > 0 = testFailed (show s)
            | otherwise = testPassed ""

-- --------------- --
-- DocTest Options --
-- --------------- --

-- | Default configuration for @doctest@.
defaultConfig :: Config
defaultConfig =
    Config
        { ghcOptions = [],
          fastMode = True,
          preserveIt = False,
          verbose = False,
          repl = stackRepl
        }

-- | Add GHC options to a DocTest `Config`.
appendFlags :: [Text.String] -> Config -> Config
appendFlags flags config = config {ghcOptions = options}
  where
    options = nub (ghcOptions config ++ flags)

-- | Insert Tasty options into DocTest `Config`.
setOptions :: OptionSet -> Config -> Config
setOptions opts config = config {verbose}
  where
    Verbose verbose = lookupOption opts

-- | Enable DocTest verbosity via Tasty options.
newtype DocTestVerbose = Verbose Bool
    deriving newtype (Typeable)

instance IsOption DocTestVerbose where
    defaultValue = Verbose (verbose defaultConfig)
    parseValue = fmap Verbose . safeReadBool
    optionName = pure "doctest-verbose"
    optionHelp = pure "Print each test as it is run"
    optionCLParser = flagCLParser Nothing (Verbose True)

-- | Tasty options understood by DocTest.
docTestOptions :: [OptionDescription]
docTestOptions =
    [ Option (Proxy :: Proxy DocTestVerbose)
    ]
