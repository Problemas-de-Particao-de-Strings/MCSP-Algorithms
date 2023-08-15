module Strings.Heuristics.Combine (combineHeuristic, combineHeuristicS) where

import Prelude hiding (String, (++))

import Data.Bifunctor (first, second)
import Data.Set qualified as S (Set)

import Strings.Data.Partition (chars)
import Strings.Data.String (String, hasOneOf, singletons, (++))

data CombineMode
    = BothHaveSingleton
    | EitherHasSingleton
    deriving stock (Eq)

-- | Applies a function until the result converges.
converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

-- | Evaluate if two strings should be combined according to combine mode.
shouldCombine :: Ord a => CombineMode -> S.Set a -> String a -> String a -> Bool
shouldCombine mode singles xs ys = case mode of
    BothHaveSingleton -> hasOneOf xs singles && hasOneOf ys singles
    EitherHasSingleton -> hasOneOf xs singles || hasOneOf ys singles

-- | If possible, combines the first 2 blocks of a string and the first identical pair of another.
combineOne :: Eq a => ([String a], [String a]) -> Maybe ([String a], [String a])
combineOne (x1 : x2 : xs, y1 : y2 : ys)
    | (x1, x2) == (y1, y2) = Just ((x1 ++ x2) : xs, (y1 ++ y2) : ys)
    | otherwise = second (y1 :) <$> combineOne (x1 : x2 : xs, y2 : ys)
combineOne _ = Nothing

-- | If possible, combines the first 2 blocks of a string and
-- the first identical pair of another, considering combine mode.
combineOneS :: Ord a => CombineMode -> S.Set a -> ([String a], [String a]) -> Maybe ([String a], [String a])
combineOneS mode singles (x1 : x2 : xs, y1 : y2 : ys)
    | (x1, x2) == (y1, y2) && shouldCombine mode singles x1 x2 = Just ((x1 ++ x2) : xs, (y1 ++ y2) : ys)
    | otherwise = second (y1 :) <$> combineOneS mode singles (x1 : x2 : xs, y2 : ys)
combineOneS _ _ _ = Nothing

-- | Combines pairs of blocks from left to right in 2 strings.
combineAll :: Eq a => ([String a], [String a]) -> ([String a], [String a])
combineAll ([], ys) = ([], ys)
combineAll (xs, []) = (xs, [])
combineAll (x : xs, ys) =
    maybe
        combineXs
        combineAll
        (combineOne (x : xs, ys))
  where
    combineXs = first (x :) $ combineAll (xs, ys)

-- | Combines pairs of blocks from left to right in 2 strings according to combine mode.
combineAllS :: Ord a => CombineMode -> S.Set a -> ([String a], [String a]) -> ([String a], [String a])
combineAllS _ _ ([], ys) = ([], ys)
combineAllS _ _ (xs, []) = (xs, [])
combineAllS mode singles (x : xs, ys) =
    maybe
        combineXs
        (combineAllS mode singles)
        (combineOneS mode singles (x : xs, ys))
  where
    combineXs = first (x :) $ combineAllS mode singles (xs, ys)

-- | MCSP combine heuristic.
--
-- Applies combination of blocks from left to right until a maximal solution is reached.
combineHeuristic :: Ord a => String a -> String a -> ([String a], [String a])
combineHeuristic x y = converge combineAll (chars x, chars y)

-- | MSCP combine heuristic considering singleton analysis.
--
-- Applies combination of blocks from left to right until a maximal solution is reached,
-- combining first pairs in which both blocks have singletons, then pairs in which either
-- block has singletons and finally all other possible pairs.
combineHeuristicS :: Ord a => String a -> String a -> ([String a], [String a])
combineHeuristicS x y
    | null singles = converge combineAll (chars x, chars y)
    | otherwise = converge combineAll $ combineSingletons (chars x, chars y)
  where
    singles = singletons x
    combineSingletons =
        converge (combineAllS EitherHasSingleton singles)
            . converge (combineAllS BothHaveSingleton singles)
