-- | A monad that abstract randomized operations.
module MCSP.System.Random.Monad (
    Random,
    evalRandom,
    liftRandom,
) where

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..))
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
newtype Random a = Random (forall g m. Generator g m => g -> m a)

instance Functor Random where
    fmap f (Random gena) = liftRandom $ \rng -> do
        a <- gena rng
        pure (f a)
    {-# INLINE fmap #-}
    x <$ _ = pure x
    {-# INLINE (<$) #-}

instance Applicative Random where
    pure x = Random (const (pure x))
    {-# INLINE pure #-}
    liftA2 f (Random gena) (Random genb) = liftRandom $ \rng -> do
        a <- gena rng
        b <- genb rng
        pure (f a b)
    {-# INLINE liftA2 #-}
    Random genf <*> Random gena = liftRandom $ \rng -> do
        f <- genf rng
        a <- gena rng
        pure (f a)
    {-# INLINE (<*>) #-}
    Random gena *> Random genb = liftRandom $ \rng -> do
        _ <- gena rng
        genb rng
    {-# INLINE (*>) #-}
    Random gena <* Random genb = liftRandom $ \rng -> do
        a <- gena rng
        _ <- genb rng
        pure a
    {-# INLINE (<*) #-}

instance Monad Random where
    Random gena >>= f = liftRandom $ \rng -> do
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
evalRandom :: Generator g m => Random a -> g -> m a
evalRandom (Random gen) = gen
{-# INLINE evalRandom #-}

-- | Turn a standard RNG function into a `Random` monad.
liftRandom :: (forall g m. Generator g m => g -> m a) -> Random a
liftRandom = Random
{-# INLINE liftRandom #-}
