-- | CSV content.
module MCSP.Text.CSV (
    -- * CSV Content
    CSVData (..),

    -- * CSV Input
    module MCSP.Text.CSV.Parser,
    parse,
    parseIO,
    parseFile,

    -- * CSV Output
    module MCSP.Text.CSV.Writer,
    headers,
    row,
) where

import Data.Either (Either)
import Data.String (String)
import Data.Vector (Vector)
import System.IO (Handle, IO)

import MCSP.Text.CSV.Parser (CSVError, CSVParser, csvColumn, readColumn, readColumnWith, strColumn)
import MCSP.Text.CSV.Parser qualified as Parser (parse, parseFile, parseIO)
import MCSP.Text.CSV.Writer (CSVWriter, mkColumn)
import MCSP.Text.CSV.Writer qualified as Writer (headers, row)

class CSVData a where
    parser :: CSVParser a
    writer :: CSVWriter a

-- | Parses the contents of a string as a CSV file.
--
-- >>> import Prelude
-- >>> instance CSVData (Int, String) where
-- >>>     parser = liftA2 (,) (readColumn @Int "a") (strColumn "b")
-- >>>     writer = $$(mkColumn [||fst||] 2 [||show||]) <> $$(mkColumn [||snd||] 3 [||id||])
--
-- >>> parse @(Int, String) "a,b\n\"1\",c\n2,d"
-- Right [(1,"c"),(2,"d")]
--
-- >>> parse @(Int, String) "a,b\n\"1\",c\nx,d"
-- Left "column \"a\", row 2: no parse on input \"x\""
--
-- >>> parse @(Int, String) "a,b"
-- Right []
parse :: CSVData a => String -> Either CSVError (Vector a)
parse = Parser.parse parser

-- | Same as `parse`, but lift throw errors in the `IO` monad.
--
-- >>> import Prelude
-- >>> instance CSVData (Int, String) where
-- >>>     parser = liftA2 (,) (readColumn @Int "a") (strColumn "b")
-- >>>     writer = $$(mkColumn [||fst||] 2 [||show||]) <> $$(mkColumn [||snd||] 3 [||id||])
--
-- >>> parseIO @(Int, String) "a,b\n\"1\",c\n2,d"
-- [(1,"c"),(2,"d")]
--
-- >>> parseIO @(Int, String) "a,b\n\"1\",c\nx,d"
-- user error (column "a", row 2: no parse on input "x")
parseIO :: CSVData a => String -> IO (Vector a)
parseIO = Parser.parseIO parser

-- | Parses the contents of a CSV file at the given path.
--
-- >>> import Prelude
-- >>> instance CSVData (Int, String) where
-- >>>     parser = liftA2 (,) (readColumn @Int "a") (strColumn "b")
-- >>>     writer = $$(mkColumn [||fst||] 2 [||show||]) <> $$(mkColumn [||snd||] 3 [||id||])
-- >>>
-- >>> import System.IO (IOMode (..), writeFile, withFile)
-- >>> writeFile "/tmp/file.csv" "a,b\n\"1\",c\n2,d"
--
-- >>> withFile "/tmp/file.csv" $ parseFile @(Int, String)
-- [(1,"c"),(2,"d")]
parseFile :: CSVData a => Handle -> IO (Vector a)
parseFile = Parser.parseFile parser

-- | Extract the headers of a CSV writer.
--
-- >>> import Prelude
-- >>> instance CSVData (Int, String) where
-- >>>     parser = liftA2 (,) (readColumn @Int "a") (strColumn "b")
-- >>>     writer = $$(mkColumn [||fst||] 2 [||show||]) <> $$(mkColumn [||snd||] 3 [||id||])
--
-- >>> headers @_ @(Int, String) parser
-- "fst,snd"
headers :: forall proxy a. CSVData a => proxy a -> String
headers _ = Writer.headers (writer @a)

-- | Format an item as a CSV row.
--
-- >>> import Prelude
-- >>> instance CSVData (Int, String) where
-- >>>     parser = liftA2 (,) (readColumn @Int "a") (strColumn "b")
-- >>>     writer = $$(mkColumn [||fst||] 2 [||show||]) <> $$(mkColumn [||snd||] 3 [||id||])
--
-- >>> row @(Int, String) (12, "x")
-- "12,x"
row :: CSVData a => a -> String
row = Writer.row writer
