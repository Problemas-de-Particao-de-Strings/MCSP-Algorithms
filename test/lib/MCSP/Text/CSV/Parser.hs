-- | Parsing of CSV content.
module MCSP.Text.CSV.Parser (
    -- * Data Type
    CSVParser,
    CSVError,

    -- * Parser Construction
    csvColumn,
    readColumnWith,
    readColumn,
    strColumn,

    -- * Text Parsing
    parse,
    parseIO,
    parseFile,
) where

import Control.Applicative (Applicative (..), pure)
import Control.Exception (Exception (..), throwIO)
import Control.Monad (Monad (..), forM, when)
import Control.Monad.Extra (whenJust)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (..), asks)
import Data.Bool (Bool (..), otherwise)
import Data.Either (Either (..), either)
import Data.Either.Extra (eitherToMaybe, mapLeft)
import Data.Eq (Eq (..))
import Data.Foldable (length)
import Data.Function (const, id, ($), (.))
import Data.Functor (Functor (..), (<$>))
import Data.Int (Int)
import Data.List (dropWhileEnd, filter, lines, map, null, replicate, transpose)
import Data.List.Extra qualified as List (trim, zipFrom)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Map.Strict (Map, assocs, empty, fromListWith, lookup)
import Data.Maybe (Maybe (..), maybe)
import Data.Ord (Ord (..))
import Data.String (String)
import Data.Tuple (fst, snd)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Data.Vector qualified as Vector (fromListN, iforM, map, replicate, zipWith)
import GHC.Err (errorWithoutStackTrace)
import GHC.IO.Handle.Types (Handle (..))
import GHC.Num ((+))
import Safe (at, headMay)
import Safe.Exact (zipExact)
import System.IO (FilePath, IO, hGetContents)
import Text.Printf (printf)
import Text.Read (Read)
import Text.Show (Show (..), ShowS, showString, shows)

import MCSP.Text.ReadP (
    ReadP,
    char,
    eof,
    many,
    next,
    readEitherP,
    readP,
    satisfy,
    sepBy,
    trim,
    (<++),
 )

-- ------------- --
-- Basic Parsers --
-- ------------- --

-- | Parses a single trimmed CSV value.
--
-- >>> import MCSP.Text.ReadP (readP_to_S)
--
-- >>> readP_to_S readField "field ,"
-- [("field",",")]
--
-- >>> readP_to_S readField " , "
-- [("",", ")]
--
-- >>> readP_to_S readField "\"field \","
-- [("field ",",")]
readField :: ReadP String
readField = trim (readQuoted <++ readDirect) <* finish
  where
    -- quoted CSV values: "value"
    readQuoted = readP @String
    -- unquoted CSV values: value
    readDirect = many (satisfy (/= ','))
    -- end of a CSV value
    finish = eof <++ next (== ',')

-- | Parses a row of CSV values.
--
-- >>> import MCSP.Text.ReadP (readP_to_S)
--
-- >>> readP_to_S readRow " a , \"b\" "
-- [([]," a , \"b\" "),(["a"],", \"b\" "),(["a","b"],"")]
readRow :: ReadP [String]
readRow = sepBy readField (char ',')

-- -------------- --
-- Parsing errors --
-- -------------- --

-- | An error while parsing values from a CSV.
data CSVError = CSVError
    { -- | The error message.
      message :: String,
      -- | The path of the file that triggered the error, if any.
      file :: Maybe FilePath,
      -- | The column name that triggered the error, if any.
      column :: Maybe String,
      -- | The file line that triggered the error, if any.
      line :: Maybe Int
    }

instance Show CSVError where
    showsPrec _ CSVError {..} =
        maybe id (\filename -> showString filename . showString ": ") file
            . maybe id (. showString ": ") (showCoords line column)
            . showString message

-- | Shows the coordinates (column and line) if present.
--
-- >>> fmap ($ "") $ showCoords (Just 12) (Just "int")
-- Just "column int, line 13"
--
-- >>> fmap ($ "") $ showCoords (Just 12) Nothing
-- Just "line 13"
--
-- >>> fmap ($ "") $ showCoords Nothing Nothing
-- Nothing
showCoords :: Maybe Int -> Maybe String -> Maybe ShowS
showCoords line column = case (line, column) of
    (Just row, Just col) -> Just $ showColumn col . showString ", " . showLine row
    (Nothing, Just col) -> Just $ showColumn col
    (Just row, Nothing) -> Just $ showLine row
    (Nothing, Nothing) -> Nothing
  where
    showLine row = showString "line " . shows (row + 1)
    showColumn col = showString "column " . showString col

instance Exception CSVError

-- | Generates a `CSVError` with the given message.
csvError :: String -> CSVError
csvError message = CSVError {message, line = Nothing, column = Nothing, file = Nothing}

-- | Sets the `file` of the given error.
atFile :: CSVError -> FilePath -> CSVError
atFile err file = err {file = Just file}

-- | Sets the `line` of the given error.
atLine :: CSVError -> Int -> CSVError
atLine err line = err {line = Just line}

-- | Sets the `column` of the given error.
atColumn :: CSVError -> String -> CSVError
atColumn err column = err {column = Just column}

-- ----------- --
-- Untyped CSV --
-- ----------- --

-- | Parsed CSV contents as columns of strings.
data CSVData = CSVData
    { -- | Each column, organized by header name.
      columns :: {-# UNPACK #-} !(Map String (Vector String)),
      -- | The number of rows in each column.
      rows :: {-# UNPACK #-} !Int
    }
    deriving stock (Eq, Ord, Show, Read)

-- | Similiar to `lines`, but removing empty lines at the end.
--
-- >>> contentLines "a\nb\nc"
-- ["a","b","c"]
--
-- >>> contentLines "a\nb\nc\n"
-- ["a","b","c"]
--
-- >>> contentLines "a\nb\nc\n  \n  \n \n\n\n \n"
-- ["a","b","c"]
contentLines :: String -> [String]
contentLines = dropWhileEnd null . map List.trim . lines

-- | Extract the non-unique elements in the list.
--
-- >>> repeated [1, 2, 3, 2]
-- [2]
repeated :: Ord a => [a] -> [a]
repeated = map fst . filter snd . assocs . fromListWith (const $ const True) . map (,False)

-- | Parses an entire CSV file into a `Map` of string columns.
--
-- >>> parseCsv "a  , b\n 1, c\n2,x\n \n\n"
-- Right (CSVData {columns = fromList [("a",["1","2"]),("b",["c","x"])], rows = 2})
--
-- >>> parseCsv "a  , b\n 1, c, 3"
-- Left line 2: unexpected value found at 2
--
-- >>> parseCsv "a,b"
-- Right (CSVData {columns = fromList [("a",[]),("b",[])], rows = 0})
parseCsv :: String -> Either CSVError CSVData
parseCsv (contentLines -> []) = Right CSVData {columns = empty, rows = 0}
parseCsv (contentLines -> csvLines) = do
    (header :| rows) <- unwrap (csvError "missing header" `atLine` 0) $ nonEmpty csvLines
    names <- unwrap (csvError "invalid CSV header" `atLine` 0) $ parseRow header

    whenJust (headMay $ repeated names) $ \name ->
        Left (csvError "repeated header name" `atLine` 0 `atColumn` name)

    parsedRows <- forM (List.zipFrom 1 rows) $ \(row, line) -> do
        items <- unwrap (csvError "invalid CSV" `atLine` row) $ parseRow line

        when (length items < length names) $
            let name = names `at` length items
             in Left (csvError "missing column value" `atLine` row `atColumn` name)

        when (length items > length names) $
            let message = printf "unexpected value found at %d" $ length names
             in Left (csvError message `atLine` row)

        pure items

    pure $ buildCSVData names parsedRows
  where
    -- lines the contains something other than whitespace
    parseRow = eitherToMaybe . readEitherP readRow
    unwrap message = maybe (Left message) Right

-- | Restructure the data from a list of rows to a `CSVData` (a list of columns).
buildCSVData :: [String] -> [[String]] -> CSVData
buildCSVData names rows =
    CSVData
        { columns = fromListWith repeatedColumn $ zipExact names unamedColumns,
          rows = length rows
        }
  where
    unamedColumns
        | null rows = replicate (length names) []
        | otherwise = map (Vector.fromListN $ length rows) $ transpose rows
    repeatedColumn _ _ = errorWithoutStackTrace "parseCsv: unexpected repeated column"

-- | Extrac a string column, if present.
--
-- >>> let Right dat = parseCsv "a  , b\n 1, c\n2,x\n \n\n"
--
-- >>> runReaderT (getColumn "a") dat
-- Right ["1","2"]
--
-- >>> runReaderT (getColumn "c") dat
-- Left getColumn: no column "c"
getColumn :: String -> ReaderT CSVData (Either CSVError) (Vector String)
getColumn name = asks (lookup name . columns) >>= maybe noColumn pure
  where
    noColumn = lift $ Left $ csvError $ printf "getColumn: no column %s" (show name)

-- --------- --
-- CSV Monad --
-- --------- --

-- | Parses a CSV data into a vector of @a@s.
--
-- This implementation tries to use vectorization whenever possible.
newtype CSVParser a = CSVParser (ReaderT CSVData (Either CSVError) (Vector a))
    deriving newtype (Typeable)

instance Functor CSVParser where
    fmap f (CSVParser p) = CSVParser (fmap (Vector.map f) p)
    value <$ CSVParser _ = pure value

instance Applicative CSVParser where
    pure value = CSVParser $ do
        n <- asks rows
        pure (Vector.replicate n value)
    liftA2 f (CSVParser p1) (CSVParser p2) =
        CSVParser (liftA2 (Vector.zipWith f) p1 p2)
    CSVParser pf <*> CSVParser px =
        CSVParser (Vector.zipWith ($) <$> pf <*> px)
    CSVParser pA <* CSVParser pB = CSVParser (pA <* pB)
    CSVParser pA *> CSVParser pB = CSVParser (pA *> pB)

-- | Parses the contents of a string as a CSV file.
--
-- >>> let p1 = liftA2 (,) (readColumn @Int "a") (strColumn "b")
-- >>> parse p1 "a,b\n\"1\",c\n2,d"
-- Right [(1,"c"),(2,"d")]
--
-- >>> let p2 = liftA2 (,) (readColumn @Int "a") (readColumn @Int "b")
-- >>> parse p2 "a,b\n\"1\",c\n2,d"
-- Left column b, line 1: no parse on "c"
--
-- >>> let p3 = liftA2 (,) (readColumn @Int "a") (readColumn @Int "b")
-- >>> parse p3 "a,b"
-- Right []
parse :: CSVParser a -> String -> Either CSVError (Vector a)
parse (CSVParser p) text = parseCsv text >>= runReaderT p

-- | Same as `parse`, but lift throw errors in the `IO` monad.
--
-- >>> let p1 = liftA2 (,) (readColumn @Int "a") (strColumn "b")
-- >>> parseIO p1 "a,b\n\"1\",c\n2,d"
-- [(1,"c"),(2,"d")]
--
-- >>> let p2 = liftA2 (,) (readColumn @Int "a") (readColumn @Int "b")
-- >>> parseIO p2 "a,b\n\"1\",c\n2,d"
-- *** Exception: column b, line 1: no parse on "c"
parseIO :: CSVParser a -> String -> IO (Vector a)
parseIO p text = either throwIO pure (parse p text)

-- | Parses the contents of a CSV file at the given path.
--
-- >>> import System.IO (IOMode (..), writeFile, withFile)
-- >>> writeFile "/tmp/file.csv" "a,b\n\"1\",c\n2,d"
--
-- >>> let p1 = liftA2 (,) (readColumn @Int "a") (strColumn "b")
-- >>> withFile "/tmp/file.csv" ReadMode (parseFile p1)
-- [(1,"c"),(2,"d")]
--
-- >>> let p2 = liftA2 (,) (readColumn @Int "a") (readColumn @Int "b")
-- >>> withFile "/tmp/file.csv" ReadMode (parseFile p2)
-- *** Exception: /tmp/file.csv: column b, line 1: no parse on "c"
parseFile :: CSVParser a -> Handle -> IO (Vector a)
parseFile p handle = do
    text <- hGetContents handle
    case parse p text of
        Right results -> pure results
        Left err -> throwIO (err `atFile` pathOf handle)
  where
    pathOf (FileHandle path _) = path
    pathOf (DuplexHandle path _ _) = path

-- | Parses a single column from a CSV with the given parser.
--
-- >>> parse (csvColumn "a" $ \s -> pure s) "a,b\n\"1\",c\n2,d"
-- Right ["1","2"]
csvColumn :: String -> (String -> Either String a) -> CSVParser a
csvColumn header parser = CSVParser $ do
    values <- getColumn header
    Vector.iforM values $ \i value ->
        lift $ mapLeft (\message -> csvError message `atColumn` header `atLine` i) $ parser value

-- | Parses a single column from a CSV with the given `ReadP` parser.
--
-- >>> parse (readColumnWith "a" $ readP @Int) "a,b\n\"1\",c\n2,d"
-- Right [1,2]
readColumnWith :: String -> ReadP a -> CSVParser a
readColumnWith header = csvColumn header . readEitherP

-- | Parses a single column from a CSV via the default `Read` implementation.
--
-- >>> parse (readColumn @Int "a") "a,b\n\"1\",c\n2,d"
-- Right [1,2]
readColumn :: Read a => String -> CSVParser a
readColumn header = readColumnWith header readP

-- | Parses a single string column from a CSV.
--
-- >>> parse (strColumn "a") "a,b\n\"1\",c\n2,d"
-- Right ["1","2"]
strColumn :: String -> CSVParser String
strColumn header = csvColumn header pure
