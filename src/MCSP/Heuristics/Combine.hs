-- | Combine Heuristics for solving the MCSP problem.
module MCSP.Heuristics.Combine (
    combine,
    combineS,
) where

import Prelude hiding (String, (++))

import Data.Set (Set)

import MCSP.Data.Pair (Pair, first, second, ($$))
import MCSP.Data.String (String, (++))
import MCSP.Data.String.Extra (Partition, chars, hasOneOf, singletons)

-- | Applies a function until the result converges.
converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

-- | Algorithm used in @combineAll@ to decide whether to combine to matching blocks.
class CombineDecision h a where
    -- | Given two matching blocks, decide if they should be combined.
    shouldCombine :: h a -> String a -> String a -> Bool

-- | Always combine two matching blocks.
data AlwaysCombine a = AlwaysCombine

instance CombineDecision AlwaysCombine a where
    shouldCombine AlwaysCombine _ _ = True
    {-# INLINE shouldCombine #-}

-- | Combine blocks if both of them have a singleton.
newtype BothHaveSingleton a = BothHaveSingleton (Set a)

instance Ord a => CombineDecision BothHaveSingleton a where
    shouldCombine (BothHaveSingleton singles) xs ys =
        xs `hasOneOf` singles && ys `hasOneOf` singles
    {-# INLINE shouldCombine #-}

-- | Combine blocks if either one of them have a singleton.
newtype EitherHasSingleton a = EitherHasSingleton (Set a)

instance Ord a => CombineDecision EitherHasSingleton a where
    shouldCombine (EitherHasSingleton singles) xs ys =
        xs `hasOneOf` singles || ys `hasOneOf` singles
    {-# INLINE shouldCombine #-}

-- | /O(n^2)/ If possible, combines the first 2 blocks of a string and the first identical pair of
-- another.
combineOne :: (CombineDecision h a, Eq a) => h a -> Pair (Partition a) -> Maybe (Pair (Partition a))
combineOne deicision (x1 : x2 : xs, y1 : y2 : ys)
    | (x1, x2) == (y1, y2) && shouldCombine deicision x1 x2 = Just ((x1 ++ x2) : xs, (y1 ++ y2) : ys)
    | otherwise = second (y1 :) <$> combineOne deicision (x1 : x2 : xs, y2 : ys)
combineOne _ _ = Nothing
-- Specilization for each `CombineDecision` possible.
{-# SPECIALIZE combineOne ::
    Eq a => AlwaysCombine a -> Pair (Partition a) -> Maybe (Pair (Partition a))
    #-}
{-# SPECIALIZE combineOne ::
    Ord a => BothHaveSingleton a -> Pair (Partition a) -> Maybe (Pair (Partition a))
    #-}
{-# SPECIALIZE combineOne ::
    Ord a => EitherHasSingleton a -> Pair (Partition a) -> Maybe (Pair (Partition a))
    #-}

-- | Combines pairs of blocks from left to right in 2 strings.
combineAll :: (CombineDecision h a, Eq a) => h a -> Pair (Partition a) -> Pair (Partition a)
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
{-# SPECIALIZE combineAll :: Eq a => AlwaysCombine a -> Pair (Partition a) -> Pair (Partition a) #-}
{-# SPECIALIZE combineAll ::
    Ord a => BothHaveSingleton a -> Pair (Partition a) -> Pair (Partition a)
    #-}
{-# SPECIALIZE combineAll ::
    Ord a => EitherHasSingleton a -> Pair (Partition a) -> Pair (Partition a)
    #-}

-- | MCSP combine heuristic.
--
-- Applies combination of blocks from left to right until a maximal solution is reached.
combine :: Eq a => Pair (String a) -> Pair (Partition a)
combine xy = converge (combineAll AlwaysCombine) (chars $$ xy)

-- | MSCP combine heuristic considering singleton analysis.
--
-- Applies combination of blocks from left to right until a maximal solution is reached,
-- combining first pairs in which both blocks have singletons, then pairs in which either
-- block has singletons and finally all other possible pairs.
combineS :: Ord a => Pair (String a) -> Pair (Partition a)
combineS (x, y)
    | null singles = converge (combineAll AlwaysCombine) (chars $$ (x, y))
    | otherwise = converge (combineAll AlwaysCombine) $ combineSingletons (chars $$ (x, y))
  where
    singles = singletons x
    combineSingletons =
        converge (combineAll $ EitherHasSingleton singles)
            . converge (combineAll $ BothHaveSingleton singles)
