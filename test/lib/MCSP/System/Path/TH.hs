-- | Compile time package information.
module MCSP.System.Path.TH (
    thisFile,
    mkPackageRoot,
) where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Control.Monad.Fail (fail)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.List.Extra (stripSuffix, (++))
import Data.Maybe (maybe)
import System.Directory (makeAbsolute)
import System.IO (FilePath)

import Language.Haskell.TH (CodeQ, bindCode, litE, loc_filename, location, stringL)
import Language.Haskell.TH.Syntax (liftTyped)

-- | Path to the file that evaluates this template.
--
-- >>> import Data.List (reverse, take)
-- >>> lastN n s = reverse (take n (reverse s))
-- >>> lastN 47 $$thisFile
-- "MCSP-Algorithms/test/lib/MCSP/System/Path/TH.hs"
thisFile :: CodeQ FilePath
thisFile = bindCode (loc_filename <$> location) liftTyped

-- | Path to the root directory of the MCSP package.
--
-- >>> import Data.List (reverse, take)
-- >>> lastN n s = reverse (take n (reverse s))
-- >>> lastN 15 $$mkPackageRoot
-- "MCSP-Algorithms"
mkPackageRoot :: CodeQ FilePath
mkPackageRoot = bindCode packageRoot liftTyped
  where
    filename = $(location >>= litE . stringL . loc_filename)
    suffix = "/test/lib/MCSP/System/Path/TH.hs"
    fixme = "FIXME: file " ++ filename ++ " changed location, please update suffix to match"
    -- extract root from current file name, checking if the suffix still makes sense
    packageRoot = do
        path <- liftIO (makeAbsolute filename)
        let root = stripSuffix suffix path
        maybe (fail fixme) pure root
