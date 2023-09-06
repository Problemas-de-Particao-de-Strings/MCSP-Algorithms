module MCSP.System.Path (
    FilePath,
    thisFile,
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
import Language.Haskell.TH (CodeQ, bindCode, loc_filename, location)
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO (FilePath, IO)

-- | Path to the file that evaluates this template.
--
-- >>> import Data.List (reverse, take)
-- >>> lastN n s = reverse (take n (reverse s))
-- >>> lastN 44 $$thisFile
-- "MCSP-Algorithms/test/lib/MCSP/System/Path.hs"
thisFile :: CodeQ FilePath
thisFile = bindCode (loc_filename <$> location) liftTyped

-- | Creates the directory and all of its parents if missing.
createDirectory :: FilePath -> IO ()
createDirectory = createDirectoryIfMissing True

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

-- | A string for the current time and date in ISO8601 format.
--
-- >>> getCurrentTimestamp
-- "2023-09-05T23:52:16.470667722-03:00"
getCurrentTimestamp :: IO String
getCurrentTimestamp = iso8601Show <$> getZonedTime
{-# INLINE getCurrentTimestamp #-}
