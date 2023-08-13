{-# LANGUAGE UndecidableInstances #-}

module Strings.Data.String.Parse (
    ReadString (..),
    readCharsPrec,
) where

import Data.Char (isSpace)
import Text.ParserCombinators.ReadP (ReadP, satisfy, skipSpaces, (<++))
import Text.ParserCombinators.ReadPrec (ReadPrec, lift, minPrec, readPrec_to_P)
import Text.Read (Read (readPrec))

-- | Specializable text to String conversion.
--
-- Used for reading a string of the given character `a`.
class ReadString a where
    {-# MINIMAL readChars #-}

    -- | Read characters of a `String a`.
    --
    -- `Read (String a)` uses this specialized implementation.
    readChars :: ReadP [a]

-- | Same a `readChars`, but lifted to the `ReadPrec` monad.
readCharsPrec :: ReadString a => ReadPrec [a]
readCharsPrec = lift readChars

-- | Unlift a `ReaP` from `ReadPrec` by giving it a default precedence.
ignorePrec :: ReadPrec a -> ReadP a
ignorePrec r = readPrec_to_P r minPrec

-- | Tries to read a list of elements, returning an empty list in case of errors.
readOrEmpty :: ReadP [a] -> ReadP [a]
readOrEmpty r = r <++ pure []

-- | Reads a list of unquoted and unseparated items.
readMany :: ReadP a -> ReadP [a]
readMany readItem = readOrEmpty $ do
    value <- readItem
    rest <- readMany readItem
    pure (value : rest)

-- | The default, reads all characters without separators.
instance {-# OVERLAPPABLE #-} Read a => ReadString a where
    readChars = readMany $ ignorePrec readPrec

-- | Reads a non-space character.
readNonSpace :: ReadP Char
readNonSpace = satisfy (not . isSpace)

-- | Specialized Char version, reading unquoted characters.
instance ReadString Char where
    readChars = skipSpaces *> readMany readNonSpace
