{-# LANGUAGE UndecidableInstances #-}

module Strings.Data.String.Parse (
    ReadString (..),
    readCharsPrec,
) where

import Data.Char (isSpace)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.Extra (firstJust, snoc)
import Text.ParserCombinators.ReadP (ReadP, gather, pfail, readP_to_S, satisfy, (<++))
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

-- | Runs a parser againts the given text, returning the first solution it finds.
--
-- Returns `Nothing` if some text is left after the match.
--
-- >>> match (satisfy (== 'A')) ""
-- Nothing
-- >>> match (satisfy (== 'A')) "A"
-- Just 'A'
-- >>> match (satisfy (== 'A')) "AA"
-- Nothing
match :: ReadP a -> String -> Maybe a
match r s = firstJust fullMatch $ readP_to_S r s
  where
    fullMatch (item, "") = Just item
    fullMatch _ = Nothing

-- | Get a single character, if it is not the end of line.
--
-- >>> readP_to_S getInLine "A"
-- [('A',"")]
-- >>> readP_to_S getInLine "\n"
-- []
getInLine :: ReadP Char
getInLine = satisfy (/= '\n')

-- | Skip whitespace, but not the end of line.
--
-- >>> readP_to_S skipInLine "   "
-- [((),"")]
-- >>> readP_to_S (skipInLine *> getInLine) "  A"
-- [('A',"")]
-- >>> readP_to_S skipInLine "  \n"
-- [((),"\n")]
skipInLine :: ReadP ()
skipInLine = readOr () (satisfy inLineSpace *> skipInLine)
  where
    inLineSpace c = isSpace c && c /= '\n'

-- | Tries to read an item, returning a default value in case of failure.
--
-- >>> readP_to_S (readOr 'X' getInLine) "A"
-- [('A',"")]
-- >>> readP_to_S (readOr 'X' getInLine) "\n"
-- [('X',"\n")]
readOr :: a -> ReadP a -> ReadP a
readOr x readItem = readItem <++ pure x

-- | Lifts the value from a `Maybe` inside the `ReadP` monad.
--
-- No text is consumed, but the parser will fail in case of `Nothing`.
--
-- >>> readP_to_S (fromJust (Just 12)) "text"
-- [(12,"text")]
-- >>> readP_to_S (fromJust (Nothing)) "text"
-- []
fromJust :: Maybe a -> ReadP a
fromJust = maybe pfail pure

-- | Creates a partial parser that consumes the minimum amount of text possible.
--
-- Parsers usually try to match on a full token at once. For Strings, partially matching on characters is more useful.
-- This parser will return a match even if it is part of a larger token.
--
-- >>> readP_to_S (readPartialMinimal $ ignorePrec readPrec :: ReadP Int) "1234 5"
-- [(1,"234 5")]
readPartialMinimal :: ReadP a -> ReadP a
readPartialMinimal readItem = readUntilFirstMatch ""
  where
    readUntilFirstMatch acc = fromJust (match readItem acc) <++ continueMatchSearch acc
    continueMatchSearch acc = getInLine >>= readUntilFirstMatch . snoc acc

-- | Creates a partial parser that tries to increase a match as much as possible.
--
-- Parsers usually try to match on a full token at once. For Strings, partially matching on characters is more useful.
-- This parser receives an initial match and the text it consumed and tries to make another match consuming more text.
--
-- >>> readP_to_S (readPartialFrom (ignorePrec readPrec :: ReadP Int) ("1", 1)) "234 5"
-- [(1234," 5")]
readPartialFrom :: ReadP a -> (String, a) -> ReadP a
readPartialFrom readItem (buffer, currentMatch) = readOr currentMatch $ do
    ch <- getInLine
    let text = snoc buffer ch
    nextMatch <- fromJust (match readItem text)
    readPartialFrom readItem (text, nextMatch)

-- | Creates a partial parser that consumes the maximum amount of text possible.
--
-- Parsers usually try to match on a full token at once. For Strings, partially matching on characters is more useful.
-- This parser changes its input parser to be character-based.
--
-- Note that this parser may be very inefficient if the input parser is not simple enough. It sould only be used for
-- simple types the can be represented with a small number of characters (like Enums).
--
--
-- >>> readP_to_S (readPartial $ ignorePrec readPrec :: ReadP Int) "1234 5"
-- [(1234," 5")]
readPartial :: ReadP a -> ReadP a
readPartial readItem = gather (readPartialMinimal readItem) >>= readPartialFrom readItem

-- | Reads a list of unquoted and unseparated items.
--
-- >>> readP_to_S (readMany (ignorePrec readPrec :: ReadP Int)) "1234 5"
-- [([1234,5],"")]
readMany :: ReadP a -> ReadP [a]
readMany readItem = readOr [] $ do
    value <- readItem
    rest <- readMany readItem
    pure (value : rest)

-- | The default, reads all characters without separators.
instance {-# OVERLAPPABLE #-} Read a => ReadString a where
    readChars = do
        skipInLine
        let readItem = ignorePrec readPrec
        readMany $ readPartial readItem

-- | Reads a non-space character.
readNonSpace :: ReadP Char
readNonSpace = satisfy (not . isSpace)

-- | Specialized Char version, reading unquoted characters.
instance ReadString Char where
    readChars = do
        skipInLine
        readMany readNonSpace

-- | Reads characters split as tokens, using the default parser for `Read a`.
--
-- >>> readP_to_S (readTokensInLine :: ReadP [Int]) "1234 5"
-- [([1234,5],"")]
readTokensInLine :: Read a => ReadP [a]
readTokensInLine = readMany (skipInLine *> ignorePrec readPrec)

instance ReadString Integer where
    readChars = readTokensInLine

instance ReadString Int where
    readChars = readTokensInLine

instance ReadString Int8 where
    readChars = readTokensInLine

instance ReadString Int16 where
    readChars = readTokensInLine

instance ReadString Int32 where
    readChars = readTokensInLine

instance ReadString Int64 where
    readChars = readTokensInLine
