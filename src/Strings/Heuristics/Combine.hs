module Strings.Heuristics.Combine (combineHeuristic, combineAll, combineOne) where

import Prelude hiding (String, (++))

import Data.Vector.Generic ((++))

import Strings.Data.String (String (..), splitGenes)

-- | Applies a function until the result converges.
converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

-- | If possible, combines the first 2 blocks of a string and the first identical pair of another.
combineOne :: Eq a => [String a] -> [String a] -> ([String a], [String a])
combineOne (x1@(String _) : x2 : xs) (y1 : y2 : ys) =
    if x1 == y1 && x2 == y2
        then
            let newX = (x1 ++ x2) : xs
                newY = (y1 ++ y2) : ys
             in (newX, newY)
        else
            let (newX, tailY) = combineOne (x1 : x2 : xs) (y2 : ys)
             in (newX, y1 : tailY)
combineOne x y = (x, y)

-- | Combines pairs of blocks from left to right in 2 strings.
combineAll :: Eq a => [String a] -> [String a] -> ([String a], [String a])
combineAll (x : xs) y =
    if newY == y
        then
            let (nextX, nextY) = combineAll xs y
             in (x : nextX, nextY)
        else combineAll newX newY
  where
    (newX, newY) = combineOne (x : xs) y
combineAll [] y = ([], y)

-- | Combine heuristic for the MSCP problem.
--
-- Applies combination of blocks from left to right until a maximal solution is reached.
combineHeuristic :: Eq a => String a -> String a -> ([String a], [String a])
combineHeuristic x y = converge (uncurry combineAll) (splitGenes x, splitGenes y)
