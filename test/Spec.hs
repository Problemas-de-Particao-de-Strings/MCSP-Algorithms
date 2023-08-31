import Control.Applicative (pure)
import Control.Monad (fmap, sequence, unless)
import Data.Bool (Bool (False, True), otherwise)
import Data.Char (Char)
import Data.Eq (Eq ((==)))
import Data.Foldable (Foldable (..), and)
import Data.Functor (($>))
import Data.List (sort, (++))
import Data.String qualified as Text
import System.IO (IO, putStrLn)
import Text.Show (Show, show)

import MCSP.Data.RadixTree (construct)
import MCSP.Data.String (String, concat)
import MCSP.System.Random (generate, shuffle)

assertEq :: (Show a, Eq a) => Text.String -> a -> a -> IO Bool
assertEq function expected found
    | expected == found = pure True
    | otherwise =
        putStrLn (function ++ ": expected " ++ show expected ++ ", found " ++ show found) $> False

strs :: [String Char]
strs = sort ["test", "suite", "", "check", "fold", "order"]

foldMapToList :: Foldable t => t (String Char) -> [String Char]
foldMapToList = foldMap (: [])

foldMap'ToList :: Foldable t => t (String Char) -> [String Char]
foldMap'ToList = foldMap' (: [])

foldrToList :: Foldable t => t (String Char) -> [String Char]
foldrToList = foldr (:) []

foldlToList :: Foldable t => t (String Char) -> [String Char]
foldlToList = foldl (\acc x -> acc ++ [x]) []

foldConcat :: Foldable t => t (String Char) -> String Char
foldConcat = fold

runTests :: [IO Bool] -> IO Bool
runTests t = fmap and (sequence t)

testWith :: Foldable t => ([String Char] -> IO (t (String Char))) -> IO Bool
testWith gen = do
    target <- gen strs
    runTests
        [ assertEq "toList" strs (toList target)
        , assertEq "foldMap" strs (foldMapToList target)
        , assertEq "foldMap'" strs (foldMap'ToList target)
        , assertEq "foldr" strs (foldrToList target)
        , assertEq "foldl" strs (foldlToList target)
        , assertEq "fold" (concat strs) (foldConcat target)
        ]

test :: IO ()
test = do
    ok <- testWith pure
    unless ok (putStrLn "errors found in 'testWith [String Char]'")
    ok' <- testWith rtree
    unless ok' (putStrLn "errors found in 'testWith (RadixTree Char)")
  where
    rtree s = do
        s' <- generate (shuffle (s ++ s))
        pure (construct s')

main :: IO ()
main = test
