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
    cartesian,
    liftP,
    transpose,
    module Data.Tuple,
    module Data.Tuple.Extra,
    bothM,
    zip,
    unzip,
    ($:),
) where

import Control.Applicative (Applicative, liftA2)
import Data.Function (id)
import Data.List.NonEmpty (unzip)
import Data.Tuple (fst, snd, swap, uncurry)
import Data.Tuple.Extra (both, dupe, first, firstM, second, secondM, (&&&), (***))

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
-- >>> zip (Just 1, Nothing)
-- Nothing
-- >>> zip (Just 1, Just 2)
-- Just (1,2)
zip :: Applicative m => Pair (m a) -> m (Pair a)
zip = bothM id
{-# INLINE zip #-}

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
