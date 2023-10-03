module MCSP.Tests.StringExtra (stringExtraTests) where

import Data.Bool (not, otherwise)
import Data.Char (Char)
import Data.Foldable (foldl', length, null)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (map, tails)
import Data.List.Extra qualified as List (
    isInfixOf,
    isPrefixOf,
    isSuffixOf,
    stripInfix,
    stripPrefix,
    stripSuffix,
    (++),
 )
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Ord ((<=), (>=))
import Data.Set (member, union)
import GHC.IsList (fromList, toList)
import GHC.Num ((*), (+), (-))
import GHC.Real (rem)

import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.QuickCheck (Testable, classify, testProperty, (.&&.), (=/=), (===), (==>))

import MCSP.Data.Pair (both, unzip, ($:))
import MCSP.Data.String (String (..), drop, singleton, slice, take, (++))
import MCSP.Data.String.Extra (
    chars,
    commonPrefix,
    hasOneOf,
    isInfixOf,
    isPrefixOf,
    isSuffixOf,
    longestCommonSubstring,
    repeated,
    singletons,
    splitCommonPrefix,
    stripInfix,
    stripPrefix,
    stripSuffix,
    suffixes,
 )

stringExtraTests :: TestTree
stringExtraTests =
    testGroup
        "String.Extra"
        [ charSetTests,
          prefixOpTests,
          suffixOpTests,
          infixOpTests
        ]

testStr :: Testable prop => TestName -> (String Char -> prop) -> TestTree
testStr name prop = testProperty name $ \str ->
    classify (not (null str)) "non-null" (prop str)

charSetTests :: TestTree
charSetTests =
    testGroup
        "character set functions hold"
        [ testStr "chars == toList" $ \str ->
            chars str === map singleton (toList str),
          testStr "singletons `union` repeated == fromList" $ \str ->
            singletons str `union` repeated str === fromList (toList str),
          testStr "str `hasOneOf` singletons str" $ \str ->
            not (null (singletons str)) ==> str `hasOneOf` singletons str,
          testStr "str `hasOneOf` repeated str" $ \str ->
            not (null (repeated str)) ==> str `hasOneOf` repeated str,
          testStr "count (`member` singletons str) == length (singletons str)" $ \str ->
            count (`member` singletons str) str === length (singletons str),
          testStr "count (`member` repeated str) >= 2 * length (repeated str)" $ \str ->
            count (`member` repeated str) str >= 2 * length (repeated str)
        ]
  where
    count prop = foldl' (\n x -> if prop x then n + 1 else n) 0

withRandomIndex :: (String a -> Int -> c) -> String a -> Int -> c
withRandomIndex prop str i
    | n <= 0 = prop str (-1)
    | otherwise = prop str (i `rem` n)
  where
    n = length str

prefixOpTests :: TestTree
prefixOpTests =
    testGroup
        "prefix operations hold"
        [ testStr "take n str `isPrefixOf` str" $ withRandomIndex $ \str n ->
            take n str `isPrefixOf` str,
          testStr "`isPrefixOf` == toList `isPrefixOf` toList" $ \x y ->
            x `isPrefixOf` y === toList x `List.isPrefixOf` toList y,
          testStr "stripPrefix (take n str) str == drop n str" $ withRandomIndex $ \str n ->
            stripPrefix (take n str) str === Just (drop n str),
          testStr "stripPrefix == fromList (stripPrefix toList toList)" $ \x y ->
            stripPrefix x y === (fromList <$> List.stripPrefix (toList x) (toList y)),
          testStr "commonPrefix (c ++ x) (c ++ y) == c" $ \c x y ->
            take (length c) (commonPrefix (c ++ x) (c ++ y)) === c,
          testStr "splitCommonPrefix x y == (commonPrefix x y, stripPrefix x, stripPrefix y)" $ \x y ->
            splitCommonPrefix x y
                === let c = commonPrefix x y
                     in (c, drop (length c) x, drop (length c) y)
        ]

suffixOpTests :: TestTree
suffixOpTests =
    testGroup
        "suffix operations hold"
        [ testStr "drop n str `isSuffixOf` str" $ withRandomIndex $ \str n ->
            drop n str `isSuffixOf` str,
          testStr "`isSuffixOf` == toList `isSuffixOf` toList" $ \x y ->
            x `isSuffixOf` y === toList x `List.isSuffixOf` toList y,
          testStr "stripSuffix (drop n str) str == take n str" $ withRandomIndex $ \str n ->
            stripSuffix (drop n str) str === Just (take n str),
          testStr "stripSuffix == fromList (stripSuffix toList toList)" $ \x y ->
            stripSuffix x y === (fromList <$> List.stripSuffix (toList x) (toList y)),
          testStr "suffixes ++ [empty] == tails" $ \str ->
            suffixes str List.++ [""] === (fromList <$> tails (toList str))
        ]

infixOpTests :: TestTree
infixOpTests =
    testGroup
        "infix operations hold"
        [ testStr "c `isInfixOf` (x ++ c ++ y)" $ \c x y ->
            c `isInfixOf` (x ++ c ++ y),
          testStr "`isInfixOf` == toList `isInfixOf` toList)" $ \x y ->
            x `isInfixOf` y === toList x `List.isInfixOf` toList y,
          testStr "stripInfix == fromList (stripInfix toList toList)" $ \x y ->
            stripInfix x y === stripAsList x y,
          testStr "stripInfix c (x ++ c ++ y) == List.stripInfix c (x ++ c ++ y)" $ \c x y ->
            stripInfix c (x ++ c ++ y) === stripAsList c (x ++ c ++ y),
          testStr "longestCommonSubstring `isInfixOf` str" $ withLCS $ \x y lcs ->
            lcs `isInfixOf` x .&&. lcs `isInfixOf` y,
          testStr "longestCommonSubstring is maximal" $ withLCS $ \x y lcs ->
            let (px, sx) = unzip (stripInfix lcs x)
                (py, sy) = unzip (stripInfix lcs y)
             in nullOrDistinct (lastS px) (lastS py) .&&. nullOrDistinct (headS sx) (headS sy)
        ]
  where
    stripAsList x y = both fromList <$> (List.stripInfix $: (toList `both` (x, y)))

    withLCS prop x y =
        let lcs = fromMaybe "" (longestCommonSubstring x y)
         in classify (not (null lcs)) "has-common-substring" (prop x y lcs)

    nullOrDistinct x y =
        not (null x) ==> not (null y) ==> x =/= y
    headS (Just (NonNull s)) = slice 0 1 s
    headS _ = ""
    lastS (Just (NonNull s)) = slice (length s - 1) 1 s
    lastS _ = ""
