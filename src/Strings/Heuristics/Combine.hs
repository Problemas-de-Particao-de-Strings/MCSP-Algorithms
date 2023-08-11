module Strings.Heuristics.Combine (combineHeuristic) where

import Prelude hiding (String, (++))

import Data.Bifunctor (first, second)

import Strings.Data.Partition (chars)
import Strings.Data.String (String, (++))

-- | Applies a function until the result converges.
converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

-- | If possible, combines the first 2 blocks of a string and the first identical pair of another.
combineOne :: Eq a => ([String a], [String a]) -> Maybe ([String a], [String a])
combineOne (x1 : x2 : xs, y1 : y2 : ys)
    | (x1, x2) == (y1, y2) = Just ((x1 ++ x2) : xs, (y1 ++ y2) : ys)
    | otherwise = second (y1 :) <$> combineOne (x1 : x2 : xs, y2 : ys)
combineOne _ = Nothing

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

-- | Combine heuristic for the MSCP problem.
--
-- Applies combination of blocks from left to right until a maximal solution is reached.
combineHeuristic :: Eq a => String a -> String a -> ([String a], [String a])
combineHeuristic x y = converge combineAll (chars x, chars y)
