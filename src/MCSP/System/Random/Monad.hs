-- | A monad that abstract randomized operations.
module MCSP.System.Random.Monad (
    Random,
    evalRandom,
    liftRandom,
    lazyRandom,
) where

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..))
import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeInterleaveST)
import Data.Function (const, ($))
import Data.Functor (Functor (..), (<$>))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Traversable (sequence)
import System.Random.PCG.Class (Generator)

-- ------------ --
-- Random Monad --
-- ------------ --

-- | A monad capable of producing random values of @a@.
newtype Random a = Random (forall g s. Generator g (ST s) => g -> ST s a)

instance Functor Random where
    fmap f (Random gena) = Random $ \rng -> do
        a <- gena rng
        pure (f a)
    {-# INLINE fmap #-}
    x <$ _ = pure x
    {-# INLINE (<$) #-}

instance Applicative Random where
    pure x = Random (const (pure x))
    {-# INLINE pure #-}
    liftA2 f (Random gena) (Random genb) = Random $ \rng -> do
        a <- gena rng
        b <- genb rng
        pure (f a b)
    {-# INLINE liftA2 #-}
    Random genf <*> Random gena = Random $ \rng -> do
        f <- genf rng
        a <- gena rng
        pure (f a)
    {-# INLINE (<*>) #-}
    Random gena *> Random genb = Random $ \rng -> do
        _ <- gena rng
        genb rng
    {-# INLINE (*>) #-}
    Random gena <* Random genb = Random $ \rng -> do
        a <- gena rng
        _ <- genb rng
        pure a
    {-# INLINE (<*) #-}

instance Monad Random where
    Random gena >>= f = Random $ \rng -> do
        a <- gena rng
        evalRandom (f a) rng
    {-# INLINE (>>=) #-}

instance Semigroup a => Semigroup (Random a) where
    (<>) = liftA2 (<>)
    {-# INLINE (<>) #-}
    sconcat xs = sconcat <$> sequence xs
    {-# INLINE sconcat #-}
    stimes n x = stimes n <$> x
    {-# INLINE stimes #-}

instance Monoid a => Monoid (Random a) where
    mempty = pure mempty
    {-# INLINE mempty #-}
    mconcat xs = mconcat <$> sequence xs
    {-# INLINE mconcat #-}

-- ---------- --
-- Evaluation --
-- ---------- --

-- | Evaluate a random computation with the given initial generator and return the final value.
evalRandom :: Generator g (ST s) => Random a -> g -> ST s a
evalRandom (Random gen) = gen
{-# INLINE evalRandom #-}

-- | Turn a standard RNG function into a `Random` monad.
liftRandom :: (forall g m. Generator g m => g -> m a) -> Random a
liftRandom gen = Random $ \rng -> gen rng
{-# INLINE liftRandom #-}

-- | Allows a `Random` monad to be evaluated lazily.
--
-- This function should be applied with care, otherwise a strict `Random` monad could still force
-- early evaluation of this lazy version.
lazyRandom :: Random a -> Random a
lazyRandom (Random gen) = Random $ \rng ->
    -- SAFETY: unsafeInterleaveST break the order of ST operations, which shouldn't matter for
    -- random operations (except of changing the output value)
    unsafeInterleaveST (gen rng)
{-# INLINE lazyRandom #-}
