-- | Module for evoking and interacting with GHCi.
module MCSP.System.Repl (
    Repl,
    stackRepl,
    runRepl,
    getFlags,
    getStackFlags,
) where

import Control.Applicative (pure)
import Data.Bool (otherwise)
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.List (concatMap, lines, unlines, words, (++))
import Data.List.Extra (isPrefixOf, stripInfix)
import Data.Maybe (Maybe (..))
import Data.String qualified as Text (String)
import Data.Tuple (snd)
import System.IO (FilePath, IO)
import System.Process (readProcess)

import MCSP.System.Path.TH (thisFile)

-- | Represents a REPL program, used for running testdocs.
type Repl = (FilePath, [Text.String])

-- | Evoke Stack GHC as a REPL.
stackRepl :: Repl
stackRepl = ("stack", ["--silent", "ghc", "--", "--interactive"])

-- | Run a list of commands in REPL.
runRepl :: Repl -> [Text.String] -> IO Text.String
runRepl (prog, args) commands = readProcess prog args input
  where
    input = unlines (commands ++ [":quit"])

-- | List compiler flags being used by the REPL.
--
-- >>> import Data.List (take)
-- >>> take 3 <$> getFlags stackRepl
-- ["-XGHC2021","-XExtendedDefaultRules","-XNoMonomorphismRestriction"]
getFlags :: Repl -> IO [Text.String]
getFlags repl = do
    output <- runRepl repl ["", ":seti"]
    pure (concatMap (getFlag . words) (lines output))
  where
    getFlag line
        | (word : _) <- line, "-" `isPrefixOf` word = line
        | Just (lang : _) <- getBaseLang line = ["-X" ++ lang]
        | otherwise = []
    getBaseLang line = snd <$> stripInfix (words "base language is:") line

-- | List compiler flags being used by the `stackRepl`.
--
-- >>> import Data.List (filter, isPrefixOf)
-- >>> filter ("-X" `isPrefixOf`) <$> getStackFlags
-- ["-XGHC2021","-XDerivingStrategies","-XDerivingVia","-XDisambiguateRecordFields","-XExplicitNamespaces","-XExtendedDefaultRules","-XNoImplicitPrelude","-XMonoLocalBinds","-XNoMonomorphismRestriction","-XOverloadedLists","-XOverloadedStrings","-XPatternSynonyms","-XRecordWildCards","-XTemplateHaskell","-XTemplateHaskellQuotes","-XTypeFamilies","-XViewPatterns"]
getStackFlags :: IO [Text.String]
getStackFlags = getFlags stackGhci
  where
    -- Evoke Stack GHCi as a REPL (cannot receive additional GHC flags).
    stackGhci :: Repl
    stackGhci = ("stack", ["--silent", "ghci", $$thisFile])
