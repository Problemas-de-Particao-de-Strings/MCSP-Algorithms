-- | Combine Heuristics for solving the MCSP problem.
module MCSP.Heuristics.Combine (
    combineHeuristic,
    combineHeuristicS,
) where

import Prelude hiding (String, (++))

import Data.Bifunctor (first, second)
import Data.Set (Set)

import MCSP.Data.String (String, (++))
import MCSP.Data.String.Extra (PartitionPair, chars, hasOneOf, singletons)

-- | Applies a function until the result converges.
converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

-- | Algorithm used in @combineAll@ to decide whether to combine to matching blocks.
class CombineDecision h a where
    -- | Given two matching blocks, decide if they should be combined.
    shouldCombine :: h -> String a -> String a -> Bool

-- | Always combine two matching blocks.
data AlwaysCombine = AlwaysCombine

instance CombineDecision AlwaysCombine a where
    shouldCombine AlwaysCombine _ _ = True
    {-# INLINE shouldCombine #-}

-- | Combine blocks if both of them have a singleton.
newtype BothHaveSingleton a = BothHaveSingleton (Set a)

instance Ord a => CombineDecision (BothHaveSingleton a) a where
    shouldCombine (BothHaveSingleton singles) xs ys = hasOneOf xs singles && hasOneOf ys singles
    {-# INLINE shouldCombine #-}

-- | Combine blocks if either one of them have a singleton.
newtype EitherHasSingleton a = EitherHasSingleton (Set a)

instance Ord a => CombineDecision (EitherHasSingleton a) a where
    shouldCombine (EitherHasSingleton singles) xs ys = hasOneOf xs singles || hasOneOf ys singles
    {-# INLINE shouldCombine #-}

-- | /O(n^2)/ If possible, combines the first 2 blocks of a string and the first identical pair of another.
combineOne :: (CombineDecision h a, Eq a) => h -> PartitionPair a -> Maybe (PartitionPair a)
combineOne deicision (x1 : x2 : xs, y1 : y2 : ys)
    | (x1, x2) == (y1, y2) && shouldCombine deicision x1 x2 = Just ((x1 ++ x2) : xs, (y1 ++ y2) : ys)
    | otherwise = second (y1 :) <$> combineOne deicision (x1 : x2 : xs, y2 : ys)
combineOne _ _ = Nothing
-- Specilization for each `CombineDecision` possible.
{-# SPECIALIZE combineOne :: Eq a => AlwaysCombine -> PartitionPair a -> Maybe (PartitionPair a) #-}
{-# SPECIALIZE combineOne :: Ord a => BothHaveSingleton a -> PartitionPair a -> Maybe (PartitionPair a) #-}
{-# SPECIALIZE combineOne :: Ord a => EitherHasSingleton a -> PartitionPair a -> Maybe (PartitionPair a) #-}

-- | Combines pairs of blocks from left to right in 2 strings.
combineAll :: (CombineDecision h a, Eq a) => h -> PartitionPair a -> PartitionPair a
combineAll _ ([], ys) = ([], ys)
combineAll _ (xs, []) = (xs, [])
combineAll decision (x : xs, ys) =
    maybe
        combineXs
        (combineAll decision)
        (combineOne decision (x : xs, ys))
  where
    combineXs = first (x :) $ combineAll decision (xs, ys)
-- Specilization for each `CombineDecision` possible.
{-# SPECIALIZE combineAll :: Eq a => AlwaysCombine -> PartitionPair a -> PartitionPair a #-}
{-# SPECIALIZE combineAll :: Ord a => BothHaveSingleton a -> PartitionPair a -> PartitionPair a #-}
{-# SPECIALIZE combineAll :: Ord a => EitherHasSingleton a -> PartitionPair a -> PartitionPair a #-}

-- | MCSP combine heuristic.
--
-- Applies combination of blocks from left to right until a maximal solution is reached.
combineHeuristic :: Eq a => String a -> String a -> PartitionPair a
combineHeuristic x y = converge (combineAll AlwaysCombine) (chars x, chars y)

-- | MSCP combine heuristic considering singleton analysis.
--
-- Applies combination of blocks from left to right until a maximal solution is reached,
-- combining first pairs in which both blocks have singletons, then pairs in which either
-- block has singletons and finally all other possible pairs.
combineHeuristicS :: Ord a => String a -> String a -> PartitionPair a
combineHeuristicS x y
    | null singles = converge (combineAll AlwaysCombine) (chars x, chars y)
    | otherwise = converge (combineAll AlwaysCombine) $ combineSingletons (chars x, chars y)
  where
    singles = singletons x
    combineSingletons =
        converge (combineAll $ EitherHasSingleton singles)
            . converge (combineAll $ BothHaveSingleton singles)
