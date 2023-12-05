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
import Control.Monad.Trans.Reader (ReaderT (..), mapReaderT, runReaderT)
import Data.Function (($), (.))
import Data.Functor (Functor (..), (<$>))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Traversable (sequence)
import System.Random.PCG.Class (Generator)

-- ------------ --
-- Random Monad --
-- ------------ --

-- | A monad capable of producing random values of @a@.
newtype Random a = Random (forall g s. Generator g (ST s) => ReaderT g (ST s) a)

instance Functor Random where
    fmap f (Random gen) = Random (fmap f gen)
    {-# INLINE fmap #-}
    x <$ _ = pure x
    {-# INLINE (<$) #-}

instance Applicative Random where
    pure x = Random (pure x)
    {-# INLINE pure #-}
    liftA2 f (Random genA) (Random genB) = Random (liftA2 f genA genB)
    {-# INLINE liftA2 #-}
    Random genF <*> Random genA = Random (genF <*> genA)
    {-# INLINE (<*>) #-}
    Random genA *> Random genB = Random (genA *> genB)
    {-# INLINE (*>) #-}
    Random genA <* Random genB = Random (genA <* genB)
    {-# INLINE (<*) #-}

instance Monad Random where
    Random genA >>= f = Random (genA >>= getRandom . f)
      where
        getRandom (Random gen) = gen
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
evalRandom (Random gen) = runReaderT gen
{-# INLINE evalRandom #-}

-- | Turn a standard RNG function into a `Random` monad.
liftRandom :: (forall g m. Generator g m => g -> m a) -> Random a
liftRandom gen = Random $ ReaderT gen
{-# INLINE liftRandom #-}

-- | Allows a `Random` monad to be evaluated lazily.
--
-- This function should be applied with care, otherwise a strict `Random` monad could still force
-- early evaluation of this lazy version.
lazyRandom :: Random a -> Random a
lazyRandom (Random gen) =
    -- SAFETY: unsafeInterleaveST break the order of ST operations, which shouldn't matter for
    -- random operations (except of changing the output value)
    Random $ mapReaderT unsafeInterleaveST gen
{-# INLINE lazyRandom #-}
