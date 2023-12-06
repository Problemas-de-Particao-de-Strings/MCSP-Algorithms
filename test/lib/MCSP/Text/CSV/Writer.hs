-- | Writing CSV content.
module MCSP.Text.CSV.Writer (
    -- * Data Type
    CSVWriter,

    -- * Writer Construction
    mkColumn,

    -- * Text Output
    headers,
    row,
) where

import Control.Applicative (pure)
import Control.Monad (fail, mapM)
import Data.Bool ((||))
import Data.Char (isSpace)
import Data.Function (id, ($), (.))
import Data.Int (Int)
import Data.List (elem, intercalate, length, map, replicate, (++))
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.String (String)
import Data.Tuple (fst, snd)
import GHC.Num ((-))
import Safe (headMay, lastMay)
import Text.Printf (printf)
import Text.Show (show)

import Language.Haskell.TH (Code, Exp (..), Q, nameBase, ppr)
import Language.Haskell.TH.Syntax (examineCode, joinCode, unType)

-- | Writes @a@s in CSV format.
newtype CSVWriter a = CSVWriter [(String, a -> String)]

instance Semigroup (CSVWriter a) where
    CSVWriter colsA <> CSVWriter colsB = CSVWriter (colsA <> colsB)
    stimes _ = id

instance Monoid (CSVWriter a) where
    mempty = CSVWriter mempty

-- | Pads the output string with spaces, until it has the specified length.
--
-- >>> withMinWidth 10 "hello"
-- "     hello"
--
-- >>> withMinWidth 4 "hello"
-- "hello"
withMinWidth :: Int -> String -> String
withMinWidth minWidth str = replicate (minWidth - length str) ' ' ++ str

-- | Escape string if it would result in wrong CSV.
--
-- >>> escape "simple text"
-- "simple text"
--
-- >>> escape "with,comma"
-- "\"with,comma\""
--
-- >>> escape "with\"quotes\""
-- "\"with\\\"quotes\\\"\""
--
-- >>> escape " with spaces "
-- "\" with spaces \""
escape :: String -> String
escape str
    | (Just head, Just last) <- (headMay str, lastMay str) =
        if ',' `elem` str || '"' `elem` str || isSpace head || isSpace last
            then show str
            else str
escape str = str

-- | Writer for a single column.
columnWriter :: String -> Int -> (a -> String) -> CSVWriter a
columnWriter header width showColumn =
    CSVWriter [(header, withMinWidth width . escape . showColumn)]

-- | Extract column from named function, usually a record field.
--
-- >>> let w = $$(mkColumn [||show||] 2 [||id||]) <> $$(mkColumn [||id||] 4 [||show||])
-- >>> :t w
-- w :: CSVWriter ()
mkColumn :: Code Q (a -> b) -> Int -> Code Q (b -> String) -> Code Q (CSVWriter a)
mkColumn columnCode minWidth showColumn = joinCode $ do
    column <- examineCode columnCode
    name <- case unType column of
        VarE name -> pure (nameBase name)
        exp -> fail $ printf "invalid column expression: %s" (show $ ppr exp)

    pure [||columnWriter name minWidth ($$showColumn . $$columnCode)||]

-- | Extract the headers of a CSV writer.
--
-- >>> let w = $$(mkColumn [||show||] 2 [||id||]) <> $$(mkColumn [||id||] 4 [||show||])
-- >>> headers w
-- "show,id"
headers :: CSVWriter a -> String
headers (CSVWriter columns) = intercalate "," (map fst columns)

-- | Format an item as a CSV row.
--
-- >>> let w = $$(mkColumn [||show||] 2 [||id||]) <> $$(mkColumn [||id||] 4 [||show||])
-- >>> row w ()
-- "(),  ()"
row :: CSVWriter a -> a -> String
row (CSVWriter columns) = intercalate "," . mapM snd columns
