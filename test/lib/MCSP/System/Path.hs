module MCSP.System.Path (
    FilePath,
    packageRoot,
    directory,
    (<.>),
    (</>),
    createDirectory,
    expandFiles,
    getCurrentTimestamp,
) where

import Control.Applicative (pure)
import Data.Bool (Bool (..), not, (&&), (||))
import Data.Eq ((==))
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.List (filter, isPrefixOf)
import Data.String (String)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (getZonedTime)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Directory.Extra (listFilesInside)
import System.FilePath (makeRelative, takeDirectory, takeFileName, (<.>), (</>))
import System.IO (FilePath, IO)

import MCSP.System.Path.TH (mkPackageRoot)

-- | Path to the root directory of the MCSP package.
packageRoot :: FilePath
packageRoot = $$mkPackageRoot

-- | The directory name of a path.
--
-- >>> directory "path/to/file"
-- "path/to"
-- >>> directory (directory "path/to/file")
-- "path"
-- >>> directory "path"
-- "."
-- >>> directory ""
-- "."
-- >>> directory "/"
-- "/"
directory :: FilePath -> FilePath
directory = takeDirectory
{-# INLINE directory #-}

-- | Creates the directory and all of its parents if missing.
createDirectory :: FilePath -> IO ()
createDirectory = createDirectoryIfMissing True

-- | List all files in a directory recursively.
--
-- Hidden files and directories are ignored.
--
-- >>> expandFiles "app"
-- ["app/Main.hs","app/hie.yaml"]
-- >>> expandFiles "app/Main.hs"
-- ["app/Main.hs"]
expandFiles :: FilePath -> IO [FilePath]
expandFiles root = do
    isFile <- doesFileExist root
    if isFile
        then pure [root]
        else filter notHidden <$> listFilesInside (pure . notHidden) root
  where
    isSpecial path = path == "." || path == ".."
    isHidden path = not (isSpecial path) && "." `isPrefixOf` path
    notHidden = not . isHidden . takeFileName . makeRelative root

-- | A string for the current time and date in ISO8601 format.
--
-- >>> getCurrentTimestamp
-- "2023-09-05T23:52:16.470667722-03:00"
getCurrentTimestamp :: IO String
getCurrentTimestamp = iso8601Show <$> getZonedTime
{-# INLINE getCurrentTimestamp #-}
