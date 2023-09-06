module MCSP.System.Path (
    FilePath,
    thisFile,
    packageRoot,
    directory,
    (<.>),
    (</>),
    createDirectory,
    getCurrentTimestamp,
) where

import Data.Bool (Bool (True))
import Data.Functor ((<$>))
import Data.String (String)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (getZonedTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO (FilePath, IO)

import MCSP.System.Path.TH (mkPackageRoot, thisFile)

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

-- | A string for the current time and date in ISO8601 format.
--
-- >>> getCurrentTimestamp
-- "2023-09-05T23:52:16.470667722-03:00"
getCurrentTimestamp :: IO String
getCurrentTimestamp = iso8601Show <$> getZonedTime
{-# INLINE getCurrentTimestamp #-}
