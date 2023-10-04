-- | Operations for working with a pair of elements of the same type.
module MCSP.Data.Pair (
    -- * Data Types
    Pair,
    pattern Pair,
    left,
    right,
    pattern First,
    pattern Second,
    pattern (::|),

    -- * Operations
    module Data.Tuple,
    module Data.Tuple.Extra,
    bothM,
    zipM,
    zip,
    unzip,
    cartesian,
    liftP,
    transpose,
    ($:),

    -- * QuickCheck instances
    ShuffledPair (ShuffledPair, getPair),
) where

import Control.Applicative (Applicative, liftA2, pure)
import Data.Eq (Eq)
import Data.Function (id, ($), (.))
import Data.Int (Int)
import Data.List (length, map, sortOn, zip, zipWith)
import Data.List.NonEmpty (unzip)
import Data.Ord (Ord)
import Data.Tuple (fst, snd, swap, uncurry)
import Data.Tuple.Extra (both, dupe, first, firstM, second, secondM, (&&&), (***))
import Test.QuickCheck.Arbitrary (Arbitrary (..), Arbitrary1 (..), CoArbitrary (..))
import Test.QuickCheck.Function (Function (..), functionMap)
import Test.QuickCheck.Gen (shuffle)
import Text.Show (Show)

-- | A pair of elements of the same type @a@.
type Pair a = (a, a)

{-# COMPLETE Pair #-}
{-# COMPLETE First #-}
{-# COMPLETE Second #-}

-- | A pair of elements of the same type @a@.
pattern Pair :: a -> a -> Pair a
pattern Pair {left, right} = (left, right)
{-# INLINE CONLIKE Pair #-}

-- | Matches the first element of a pair.
--
-- >>> case ('x', 10) of First v -> v
-- 'x'
pattern First :: a -> (a, b)
pattern First x <- (x, _)
{-# INLINE CONLIKE First #-}

-- | Matches the second element of a pair.
--
-- >>> case ('x', 10) of Second v -> v
-- 10
pattern Second :: b -> (a, b)
pattern Second x <- (_, x)
{-# INLINE CONLIKE Second #-}

-- | Extracts the first two elements in a list as a pair.
--
-- >>> case [1, 2, 3] of (p ::| _) -> p
-- (1,2)
pattern (::|) :: Pair a -> [a] -> [a]
pattern p ::| xs <- (\(x : y : rest) -> ((x, y), rest) -> (p, xs))
    where
        (x, y) ::| xs = x : y : xs
{-# INLINE CONLIKE (::|) #-}

-- | Apply an action to both components of a pair.
--
-- >>> import Data.List.NonEmpty (nonEmpty)
-- >>> bothM nonEmpty ([], [1])
-- Nothing
-- >>> bothM nonEmpty ([1], [2])
-- Just (1 :| [],2 :| [])
bothM :: Applicative m => (a -> m b) -> Pair a -> m (Pair b)
bothM f (x, y) = liftA2 (,) (f x) (f y)
{-# INLINE bothM #-}

-- | Extract a pair elements from a pair of actions.
--
-- >>> import Data.Maybe (Maybe (..))
-- >>> zipM (Just 1, Nothing)
-- Nothing
-- >>> zipM (Just 1, Just 2)
-- Just (1,2)
-- >>> import Data.Int (Int)
-- >>> zipM ([1, 2, 3], [4, 5] :: [Int])
-- [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]
zipM :: Applicative m => Pair (m a) -> m (Pair a)
zipM = bothM id
{-# INLINE zipM #-}

-- | Cartesian product of the elements of two lists.
--
-- >>> cartesian [1, 2, 3] "ab"
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]
cartesian :: [a] -> [b] -> [(a, b)]
cartesian = liftA2 (,)
{-# INLINE cartesian #-}

-- | Apply a binary operator in both elements of a pair.
--
-- >>> import GHC.Num ((+))
-- >>> liftP (+) (10, 20) (3, 4)
-- (13,24)
liftP :: (a -> b -> c) -> Pair a -> Pair b -> Pair c
liftP op (x1, x2) (y1, y2) = (x1 `op` y1, x2 `op` y2)
{-# INLINE liftP #-}

-- | Transpose elements in a pair of pairs like in a square matrix.
--
-- >>> transpose (('a', 1), ('b', 2))
-- (('a','b'),(1,2))
transpose :: ((a, b), (c, d)) -> ((a, c), (b, d))
transpose ((x, y), (z, w)) = ((x, z), (y, w))
{-# INLINE transpose #-}

infixr 4 $:

-- | Spread a pair of values as arguments to a function.
--
-- Infix version of `uncurry`.
--
-- >>> import GHC.Num ((+))
-- >>> f x y = x + y
-- >>> f $: (1, 2)
-- 3
($:) :: (a -> b -> c) -> (a, b) -> c
($:) = uncurry
{-# INLINE ($:) #-}

-- -------------------- --
-- QuickCheck instances --

-- | A [QuickCheck Modifier](https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck-Modifiers.html)
-- that generates a pair of lists such that one is a permutation of the other. See `getPair`.
newtype ShuffledPair a = IndexedList {indexed :: [(Int, a)]}
    deriving newtype (Eq, Ord, Show)

-- | Extracts the permuted pair from a `ShuffledPair`.
extractPair :: ShuffledPair a -> Pair [a]
extractPair IndexedList {..} = map snd `both` (indexed, sortOn fst indexed)

{-# COMPLETE ShuffledPair #-}

-- | A [QuickCheck Modifier](https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck-Modifiers.html)
-- that generates a pair of lists such that one is a permutation of the other.
pattern ShuffledPair :: Pair [a] -> ShuffledPair a
pattern ShuffledPair {getPair} <- (extractPair -> getPair)

instance Arbitrary1 ShuffledPair where
    liftArbitrary gen = do
        elems <- liftArbitrary gen
        idx <- shuffle [1 .. length elems]
        pure (IndexedList (zip idx elems))
    liftShrink shr IndexedList {..} =
        map IndexedList (liftShrink shrinkWithIndex indexed)
      where
        shrinkWithIndex (i, x) = map (i,) (shr x)

instance Arbitrary a => Arbitrary (ShuffledPair a) where
    arbitrary = liftArbitrary arbitrary
    shrink = liftShrink shrink

instance CoArbitrary a => CoArbitrary (ShuffledPair a) where
    coarbitrary = coarbitrary . indexed

instance Function a => Function (ShuffledPair a) where
    function = functionMap indexed (IndexedList . fixIndices)
      where
        replaceIdx i (k, (_, x)) = (k, (i, x))
        fixIndices elems =
            let n = [1 .. length elems]
             in map snd $ sortOn fst $ zipWith replaceIdx n $ sortOn (fst . snd) $ zip n elems
