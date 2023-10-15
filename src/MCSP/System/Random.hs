-- | Randomized operations using a `Random` monad for "PCG" operations.
module MCSP.System.Random (
    -- * Random Monad
    Random,

    -- * Evaluation
    evalRandom,
    liftRandom,
    generate,
    generateWith,
    randomSeed,
    showSeed,
    readSeed,
    Generator (..),
    SeedableGenerator (..),
    RandomGenerator (..),
    MWC (..),
    PCG (..),
    PCGFast (..),
    PCGFastPure (..),
    PCGPure (..),
    PCGSingle (..),
    PCGUnique (..),
    Entropy (..),
    HWEntropy (..),
    Lazy (..),

    -- * Random Values
    Uniform (..),
    uniform,
    uniformR,
    uniformE,
    uniformRE,
    choose,
    shuffle,
    partitions,
) where

import Control.Applicative (Applicative (..))
import Control.Exception.Extra (errorWithoutStackTrace)
import Control.Monad (Monad (..), mapM)
import Control.Monad.ST (runST)
import Data.Bits (complement)
import Data.Foldable (length)
import Data.Function (const, ($))
import Data.Functor (Functor (..), (<$>))
import Data.Int (Int)
import Data.List ((++))
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.String qualified as Text (String)
import Data.Traversable (sequence)
import Data.Vector.Generic (Vector, indexM, splitAt)
import Data.Vector.Generic qualified as Vector (length)
import Data.Word (bitReverse64)
import GHC.Enum (Bounded (..), Enum (..))
import GHC.Exts (IsList (..))
import GHC.Num ((-))
import Numeric (readHex, showHex)
import System.IO (IO)
import System.Random.Shuffle qualified as Shuffle (shuffle)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S, readS_to_P, skipSpaces)

import MCSP.System.Random.Generator (
    Entropy (..),
    Generator (..),
    HWEntropy (..),
    Lazy (..),
    MWC (..),
    PCG (..),
    PCGFast (..),
    PCGFastPure (..),
    PCGPure (..),
    PCGSingle (..),
    PCGUnique (..),
    RandomGenerator (..),
    SeedableGenerator (..),
 )
import MCSP.System.Random.Uniform (Uniform (..))

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

-- | Use given seed to generate value.
--
-- >>> generateWith (100,200) uniform :: Int
-- 3081816684322452293
generateWith :: Seed PCG -> Random a -> a
generateWith (s1, s2) r = runST (initialize PCG seed >>= evalRandom r)
  where
    seed = (complement s2, bitReverse64 s1)
{-# INLINE generateWith #-}

-- | Use random seed to generate value in IO.
--
-- >>> generate uniform :: IO Int
-- 3295836545219376626  -- Could be any Int
generate :: Random a -> IO a
generate r = do
    seed <- randomSeed
    pure (generateWith seed r)
{-# INLINE generate #-}

-- | Generate a new random seed.
--
-- >>> randomSeed
-- (7193915830657461549,13617428908513093874) -- Could be any seed
randomSeed :: IO (Seed PCG)
randomSeed = do
    l <- genUniform Entropy
    r <- genUniform Entropy
    pure (l, r)
{-# INLINE randomSeed #-}

-- | String representing the RNG seed in hex.
--
-- Inverse of `readSeed`.
--
-- >>> showSeed (0, 1)
-- "0 1"
showSeed :: Seed PCG -> Text.String
showSeed (x, y) = showHex x " " ++ showHex y ""
{-# INLINE showSeed #-}

-- | Parser combinator for reading seeds.
readSeedP :: ReadP (Seed PCG)
readSeedP = do
    l <- readS_to_P readHex
    skipSpaces
    r <- readS_to_P readHex
    pure (l, r)

-- | Read a seed in hexadecimal format.
--
-- Inverse of `showSeed`.
--
-- >>> readSeed "0 1"
-- (0,1)
-- >>> readSeed (showSeed (5, 10))
-- (5,10)
-- >>> readSeed "75f9fea579c63117 8a3a15e4c0a7029f"
-- (8501105758304612631,9960297598112170655)
readSeed :: Text.String -> Seed PCG
readSeed str = case readP_to_S readSeedP str of
    [(seed, "")] -> seed
    [] -> errorWithoutStackTrace "readSeed: no parse"
    _ -> errorWithoutStackTrace "readSeed: ambiguous parse"

-- ------------- --
-- Random Values --
-- ------------- --

-- | /O(1)/ Generate a uniformly distributed random variate.
--
-- * Use entire range for integral types.
-- * Use (0,1] range for floating types.
--
-- >>> import GHC.Float (Double)
-- >>> generateWith (1,2) uniform :: Double
-- 0.5000000002320554
uniform :: Uniform a => Random a
uniform = liftRandom genUniform
{-# INLINE uniform #-}

-- | /O(1)/ Generate a uniformly distributed random variate in the given range.
--
-- * Use inclusive range for integral types.
-- * Use (a,b] range for floating types.
--
-- >>> generateWith (1,2) $ uniformR 10 50 :: Int
-- 16
uniformR :: Uniform a => a -> a -> Random a
uniformR lo hi = liftRandom $ genUniformR (lo, hi)
{-# INLINE uniformR #-}

-- | /O(1)/ Generate a uniformly distributed random variate.
--
-- It should be equivalent to @`uniformR` (`minBound`, `maxBound`)@, but for non-`PCG.Variate`.
--
-- >>> import Text.Show (Show)
-- >>> data T = A | B | C | D deriving (Enum, Bounded, Show)
-- >>> generateWith (1,3) uniformE :: T
-- C
uniformE :: (Enum a, Bounded a) => Random a
uniformE = uniformRE minBound maxBound
{-# INLINE uniformE #-}

-- | /O(1)/ Generate a uniformly distributed random variate in the given inclusive range.
--
-- It should be equivalent to `uniformR`, but for non-`PCG.Variate`.
--
-- >>> generateWith (1,3) $ uniformRE 0 10 :: Int
-- 5
-- >>> generateWith (1,3) $ uniformRE 'a' 'z'
-- 'n'
uniformRE :: Enum a => a -> a -> Random a
uniformRE lo hi = do
    let loNum = fromEnum lo
    let hiNum = fromEnum hi
    value <- uniformR loNum hiNum
    pure (toEnum value)
{-# INLINE uniformRE #-}

-- | /O(1)/ Choose a single random value from a vector.
--
-- >>> import Data.Vector as V
-- >>> generateWith (1,2) $ choose (V.fromList ["hi", "hello", "ola"])
--  "hi"
choose :: Vector v a => v a -> Random a
choose v = do
    let n = Vector.length v
    idx <- uniformR 0 (n - 1)
    indexM v idx
{-# INLINE choose #-}

-- | /O(n)/ Generates indices for `S.shuffle`.
--
-- See [random-shuffle](https://hackage.haskell.org/package/random-shuffle-0.0.4/docs/System-Random-Shuffle.html#v:shuffle).
treeIndices :: Int -> Random [Int]
treeIndices n = mapM (uniformR 0) [n - 1, n - 2 .. 1]
{-# INLINE treeIndices #-}

-- | /O(?)/ Shuffles a non-empty list.
--
-- This simple implementation runs forever with empty lists.
shuffleNonEmpty :: IsList l => NonEmpty (Item l) -> Random l
shuffleNonEmpty (h :| rest) = do
    let xs = h : rest
    let n = length xs
    idx <- treeIndices n
    let ys = Shuffle.shuffle xs idx
    pure (fromListN n ys)
{-# INLINE shuffleNonEmpty #-}

-- | /O(?)/ Shuffles a list randomly.
--
-- >>> generateWith (1,2) $ shuffle [1..5] :: [Int]
-- [1,4,2,5,3]
shuffle :: IsList l => l -> Random l
shuffle values = case nonEmpty (toList values) of
    Just xs -> shuffleNonEmpty xs
    Nothing -> pure values
{-# INLINEABLE shuffle #-}

-- | /O(n)/ Generate random partitions of a vector.
--
-- >>> import Data.Vector as V
-- >>> generateWith (1,4) $ partitions (V.fromList [1..10])
-- [[1,2],[3],[4,5,6,7,8],[9],[10]]
partitions :: Vector v a => v a -> Random [v a]
partitions xs = case Vector.length xs of
    0 -> pure []
    1 -> pure [xs]
    n -> do
        idx <- uniformR 1 n
        let (part, rest) = splitAt idx xs
        parts <- partitions rest
        pure (part : parts)
{-# INLINEABLE partitions #-}
