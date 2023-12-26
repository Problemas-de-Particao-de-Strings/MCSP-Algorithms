{-# LANGUAGE UndecidableInstances #-}

-- | Textual conversion for strings.
module MCSP.Data.String.Text (
    -- * Specializable `Show`.
    ShowString (..),
    showChars,
    showCharsWith,
    showWords,
    showWordsWith,

    -- * Specializable `Read`.
    ReadString (..),
    readChars,
    readCharsWith,
    readWords,
    readWordsWith,
) where

import Control.Monad (Functor (fmap), mapM, (>>=))
import Data.Char (Char)
import Data.Foldable (Foldable, foldr, toList)
import Data.Function (flip, id, ($), (.))
import Data.List (intersperse, singleton)
import Data.Maybe (Maybe (..))
import Data.String (String)
import Text.ParserCombinators.ReadP (ReadP)
import Text.Read (Read (..))
import Text.Show (Show (..), ShowS, showChar, shows)

import MCSP.Text.ReadP (maybeP, readMaybeP, readP, word, words)

-- ---------------------- --
-- Textual Output classes --
-- ---------------------- --

-- | Shows characters of a string separated by spaces.
--
-- >>> import Data.Int
-- >>> import Numeric
--
-- >>> showWordsWith @[] @Int showHex [1, 2, 12] ""
-- "1 2 c"
showWordsWith :: Foldable t => (a -> ShowS) -> t a -> ShowS
showWordsWith showItem =
    foldr (.) id
        . intersperse (showChar ' ')
        . fmap showItem
        . toList

-- | Shows characters of a string separated by spaces.
--
-- This implementation uses the default converter for @Show a@.
--
-- >>> import Data.Int
--
-- >>> showWords @[] @Int [1, 2, 12] ""
-- "1 2 12"
showWords :: (Foldable f, Show a) => f a -> ShowS
showWords = showWordsWith shows

-- | Shows all elements without quoting or separation.
--
-- >>> import Data.Int
-- >>> import Data.List
-- >>> import Numeric
-- >>> data DNA = A | C | G | T deriving (Show, Read)
--
-- >>> showCharsWith (\n -> head $ showHex n "") [1, 2, 12] ""
-- "12c"
--
-- >>> showCharsWith @[] @DNA (head . show) [A, C, C, A] ""
-- "ACCA"
showCharsWith :: Foldable f => (a -> Char) -> f a -> ShowS
showCharsWith showItem = flip $ foldr (showChar . showItem)

-- | Shows all elements without quoting or separation.
--
-- This implementation uses the default converter for @Show a@.
--
-- >>> import Data.Int
-- >>> import Data.List
-- >>> data DNA = A | C | G | T deriving (Show, Read)
--
-- >>> showChars @[] @Int [1, 2, 12, 3, 56] ""
-- "12\65533\&3\65533"
--
-- >>> showChars @[] @DNA [A, C, C, A] ""
-- "ACCA"
showChars :: (Foldable f, Show a) => f a -> ShowS
showChars = showCharsWith (toChar . show)
  where
    toChar [ch] = ch
    toChar _ = '�'

-- | Specializable `MCSP.Data.String.String` to text conversion.
--
-- Used for showing a string of the given character @a@.
class ShowString a where
    {-# MINIMAL showStr #-}

    -- | Shows characters of a `MCSP.Data.String.String`.
    --
    -- `Show` @(String a)@ uses this specialized implementation.
    showStr :: Foldable f => f a -> ShowS

instance {-# OVERLAPPABLE #-} Show a => ShowString a where
    showStr = showWords

-- | `MCSP.Data.String.String` `Char` represented by unseparated characters without quotes
-- (@abcd@).
instance ShowString Char where
    showStr = showCharsWith id

-- --------------------- --
-- Textual Input classes --
-- --------------------- --

-- | Reads characters of a string separated by spaces.
--
-- >>> import Data.Int
-- >>> import MCSP.Text.ReadP
-- >>> import Text.Read.Lex
--
-- >>> readP_to_S (readWordsWith @Int $ readMaybeP readHexP) "1 2 c"
-- [([],"1 2 c"),([1],"2 c"),([1,2],"c"),([1,2,12],"")]
--
-- >>> readP_to_S (readWordsWith $ readMaybeP word) " a  xy  b "
-- [([],"a  xy  b "),(["a"],"xy  b "),(["a","xy"],"b "),(["a","xy","b"],"")]
readWordsWith :: (String -> Maybe a) -> ReadP [a]
readWordsWith parse = words >>= mapM (maybeP . parse)

-- | Reads characters of a string separated by spaces.
--
-- This implementation uses the default converter for @Read a@.
--
-- >>> import Data.Int
-- >>> import MCSP.Text.ReadP
--
-- >>> readP_to_S (readWords @Int) "1 2 12"
-- [([],"1 2 12"),([1],"2 12"),([1,2],"12"),([1,2,12],"")]
readWords :: Read a => ReadP [a]
readWords = readWordsWith (readMaybeP readP)

-- | Reads all elements without quoting or separation.
--
-- >>> import Data.Int
-- >>> import MCSP.Text.ReadP
-- >>> import Text.Read.Lex
-- >>> data DNA = A | C | G | T deriving (Show, Read)
--
-- >>> readP_to_S (readCharsWith @Int (\ch -> readMaybeP readHexP [ch])) "12c"
-- [([1,2,12],"")]
--
-- >>> readP_to_S (readCharsWith @DNA (\ch -> readMaybeP readP [ch])) "TTGA"
-- [([T,T,G,A],"")]
readCharsWith :: (Char -> Maybe a) -> ReadP [a]
readCharsWith parse = word >>= mapM (maybeP . parse)

-- | Reads all elements without quoting or separation.
--
-- This implementation uses the default converter for @Read a@.
--
-- >>> import Data.Int
-- >>> import MCSP.Text.ReadP
-- >>> data DNA = A | C | G | T deriving (Show, Read)
--
-- >>> readP_to_S (readChars @Int) "1212"
-- [([1,2,1,2],"")]
--
-- >>> readP_to_S (readChars @DNA) "TTGA"
-- [([T,T,G,A],"")]
readChars :: Read a => ReadP [a]
readChars = readCharsWith (readMaybeP readP . singleton)

-- | Specializable text to `Strings.Data.String.String` conversion.
--
-- Used for reading a string of the given character @a@.
class ReadString a where
    {-# MINIMAL readStr #-}

    -- | Read characters of a `Strings.Data.String.String`.
    --
    -- `Read` @(String a)@ uses this specialized implementation.
    readStr :: ReadP [a]

instance {-# OVERLAPPABLE #-} Read a => ReadString a where
    readStr = readWords

-- | `MCSP.Data.String.String` `Char` represented by unseparated characters without quotes
-- (@abcd@).
instance ReadString Char where
    readStr = readCharsWith Just
