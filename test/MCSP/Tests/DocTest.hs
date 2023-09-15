module MCSP.Tests.DocTest (
    testDocs,
) where

import Control.Applicative (pure)
import Data.Bool (Bool (..), otherwise, (||))
import Data.Data (Typeable)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor (fmap, (<$>))
import Data.List (filter, isPrefixOf, (++))
import Data.Ord ((>))
import Data.String qualified as Text (String)
import System.FilePath (takeExtension)
import System.IO (FilePath, IO)
import Text.Show (show)

import Test.DocTest.Internal.Parse (extractDocTests)
import Test.DocTest.Internal.Run (Config (..), Summary (..), runDocTests)
import Test.Tasty.Providers (IsTest (..), TestTree, singleTest, testFailed, testPassed)

import MCSP.System.Path (expandFiles)
import MCSP.System.Repl (getStackFlags, stackRepl)

testDocs :: FilePath -> TestTree
testDocs path = singleTest path $ DocTest $ do
    files <- expandFiles path
    pure (filter isHaskellFile files)
  where
    isHaskellFile file = takeExtension file == ".hs"

newtype DocTest = DocTest (IO [FilePath])
    deriving newtype (Typeable)

instance IsTest DocTest where
    testOptions = pure []
    run _ (DocTest getFiles) _ = do
        -- TODO: catch errors
        extensions <- filter ("-X" `isPrefixOf`) <$> getStackFlags
        targetFile <- getFiles
        let config = appendFlags extensions $ appendFlags targetFile defaultConfig
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

appendFlags :: [Text.String] -> Config -> Config
appendFlags flags config = config {ghcOptions = ghcOptions config ++ flags}
