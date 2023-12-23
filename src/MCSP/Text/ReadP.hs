-- | Utilities for the `ReadP` parser.
module MCSP.Text.ReadP (
    -- * Construction and application
    readP,
    maybeP,
    readEitherP,
    readMaybeP,

    -- * Additional operations
    next,
    most,

    -- ** Whitespace adapters
    skipInLine,
    trimmed,
    trim,
    eol,
    word,
    words,

    -- * Re-export
    module Text.ParserCombinators.ReadP,
) where

import Control.Applicative (Applicative (..))
import Control.Monad (unless, void)
import Data.Bool (Bool (..), not, otherwise, (&&))
import Data.Char (Char, isSpace)
import Data.Either (Either (..))
import Data.Either.Extra (eitherToMaybe)
import Data.Eq (Eq (..))
import Data.Function (($), (.))
import Data.List.Extra (length, take, (++))
import Data.Maybe (Maybe (..), maybe)
import Data.Ord (Ord (..))
import Data.String (String)
import GHC.Num ((-))
import Safe (headMay, lastMay)
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (minPrec, readPrec_to_P)
import Text.Read (Read (..))
import Text.Show (Show (..))

-- | `ReadP` parser using the `Read` instance.
--
-- >>> import Data.Int
--
-- >>> readP_to_S (readP @Int) "12"
-- [(12,"")]
--
-- >>> readP_to_S (readP @Int) "33XY"
-- [(33,"XY")]
--
-- >>> readP_to_S (readP @Int) "ZX"
-- []
readP :: Read a => ReadP a
readP = readPrec_to_P readPrec minPrec

-- | Parse a string using the given `ReadP` parser instance.
--
-- Succeeds if there is exactly one valid result. A `Left` value indicates a parse error.
--
-- >>> import Data.Int
--
-- >>> readEitherP (readP @Int) "123"
-- Right 123
--
-- >>> readEitherP (readP @Int) "hello"
-- Left "no parse on \"hello\""
readEitherP :: ReadP a -> String -> Either String a
readEitherP read text = case [value | (value, "") <- readP_to_S read text] of
    [exact] -> Right exact
    [] -> Left $ "no parse on " ++ show (prefix text)
    _ -> Left $ "ambiguous parse on " ++ show (prefix text)
  where
    maxLength = 15
    prefix s
        | length s <= maxLength = s
        | otherwise = take (maxLength - 3) s ++ "..."

-- | Parse a string using the given `ReadP` parser instance.
--
-- Succeeds if there is exactly one valid result. A `Nothing` value indicates a parse error.
--
-- >>> import Data.Int
--
-- >>> readMaybeP (readP @Int) "123"
-- Just 123
--
-- >>> readMaybeP (readP @Int) "hello"
-- Nothing
readMaybeP :: ReadP a -> String -> Maybe a
readMaybeP read text = eitherToMaybe $ readEitherP read text

-- | Lift a `Maybe` to the `ReadP` monad.
--
-- `Nothing` causes a parse error.
--
-- >>> readP_to_S (maybeP $ Just 12) ""
-- [(12,"")]
--
-- >>> readP_to_S (maybeP $ Nothing) ""
-- []
maybeP :: Maybe a -> ReadP a
maybeP = maybe pfail pure

-- | Succeeds iff the next character matches the given predicate, without consuming it.
--
-- >>> readP_to_S (next (== 'x')) "x"
-- [((),"x")]
--
-- >>> readP_to_S (next (== 'x')) "y"
-- []
--
-- >>> readP_to_S (next (== 'x')) ""
-- []
next :: (Char -> Bool) -> ReadP ()
next matches = do
    str <- look
    ch <- maybeP $ headMay str
    unless (matches ch) pfail

-- | Parses zero or more occurrences of the given parser.
--
-- Like `many`, but succeds only once, with as many matches as possible.
--
-- >>> readP_to_S (most word) " abc def ghi "
-- [(["abc","def","ghi"],"")]
--
-- >>> readP_to_S (many word) " abc def ghi "
-- [([]," abc def ghi "),(["abc"],"def ghi "),(["abc","def"],"ghi "),(["abc","def","ghi"],"")]
most :: ReadP a -> ReadP [a]
most readItem = liftA2 (:) readItem (most readItem) <++ pure []

-- | Skip whitespace, but not the end of line.
--
-- >>> readP_to_S skipInLine "   "
-- [((),"")]
--
-- >>> readP_to_S (skipInLine *> satisfy (/= '\n')) "  A"
-- [('A',"")]
--
-- >>> readP_to_S skipInLine "  \n"
-- [((),"\n")]
skipInLine :: ReadP ()
skipInLine = void (munch isInLineSpace)
  where
    isInLineSpace ch = isSpace ch && ch /= '\n'

-- | Updates the parser so it guarantees that no whitespace precedes or succedes the matched text.
--
-- >>> import Data.Int
--
-- >>> readP_to_S (readP @Int) " 12 "
-- [(12," ")]
--
-- >>> readP_to_S (trimmed $ readP @Int) " 12 "
-- []
trimmed :: ReadP a -> ReadP a
trimmed read = do
    (text, value) <- gather read
    case (headMay text, lastMay text) of
        (Just head, _) | isSpace head -> pfail
        (_, Just last) | isSpace last -> pfail
        _ -> pure value

-- | Updates the parser so it used only in a trimmed part of the text, removing outer whitespace.
--
-- >>> import Data.Int
--
-- >>> readP_to_S (readP @Int) " 12 "
-- [(12," ")]
--
-- >>> readP_to_S (trim $ readP @Int) " 12 "
-- [(12,"")]
trim :: ReadP a -> ReadP a
trim read = skipInLine *> trimmed read <* skipInLine

-- | Matches the end-of-line.
--
-- End-of-line can be `eof` or @'\n'@.
--
-- >>> readP_to_S eol ""
-- [((),"")]
--
-- >>> readP_to_S eol "a"
-- []
--
-- >>> readP_to_S eol "\na"
-- [((),"\na")]
eol :: ReadP ()
eol = eof <++ next (== '\n')

-- | Matches a single word.
--
-- >>> readP_to_S word "  abc "
-- [("abc","")]
--
-- >>> readP_to_S word "  abc def "
-- [("abc","def ")]
--
-- >>> readP_to_S word "  "
-- []
word :: ReadP String
word = trim $ munch1 (not . isSpace)

-- | Matches a list of words separated by whitespaces.
--
-- >>> readP_to_S words "12 3 4"
-- [([],"12 3 4"),(["12"],"3 4"),(["12","3"],"4"),(["12","3","4"],"")]

-- >>> readP_to_S words "  xy k abcd "
-- [([],"xy k abcd "),(["xy"],"k abcd "),(["xy","k"],"abcd "),(["xy","k","abcd"],"")]
--
-- >>> readP_to_S words ""
-- [([],"")]
words :: ReadP [String]
words = trim $ sepBy (munch1 (not . isSpace)) skipInLine
