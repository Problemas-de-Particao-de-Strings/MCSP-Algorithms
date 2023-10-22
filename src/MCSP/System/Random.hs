-- | Randomized operations using a `Random` monad for "PCG" operations.
module MCSP.System.Random (
    -- * Random Monad
    Random,
    evalRandom,
    liftRandom,
    lazyRandom,
    generate,
    generateFast,

    -- * Evaluation
    Seed,
    readSeed,
    showSeed,
    generateFastWith,
    generateWith,
    randomSeed,
    (=~=),

    -- * Random Values
    PCG.Variate,
    uniform,
    uniformR,
    uniformB,
    uniformE,
    uniformRE,
    choice,
    weightedChoice,
    shuffle,
    partitions,
    repeatR,
    iterateR,
) where

import Control.Applicative (pure)
import Control.Monad (mapM)
import Control.Monad.ST (runST)
import Data.Eq (Eq)
import Data.Foldable (length, sum)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.List (map)
import Data.List.NonEmpty (NonEmpty ((:|)), head, nonEmpty, (<|))
import Data.List.NonEmpty.Extra ((!?))
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Tuple (fst, snd)
import Data.Vector (Vector, (!))
import Data.Vector.Algorithms.Search (binarySearchL)
import Data.Vector.Generic qualified as Vector (Vector, init, length, splitAt, unsafeThaw)
import Data.Vector.Unboxed qualified as Unboxed (Vector)
import Data.Word (Word32)
import GHC.Enum (Bounded (..), Enum (..))
import GHC.Exts (IsList (..))
import GHC.Float (Double)
import GHC.Num ((*), (+), (-))
import GHC.Real (fromIntegral, truncate, (/))
import System.Random.PCG qualified as PCG (Variate (..))
import System.Random.Shuffle qualified as Shuffle (shuffle)
import Test.QuickCheck (Property, property, (===))
import Text.Show (Show)

import MCSP.System.Random.Generate (
    Seed,
    generate,
    generateFast,
    generateFastWith,
    generateWith,
    randomSeed,
    readSeed,
    showSeed,
 )
import MCSP.System.Random.Monad (
    Random,
    evalRandom,
    lazyRandom,
    liftRandom,
 )

-- ------------- --
-- Random Values --
-- ------------- --

-- | /O(1)/ Generate a uniformly distributed random variate.
--
-- * Use entire range for integral types.
-- * Use (0,1] range for floating types.
--
-- >>> import Prelude (Double)
-- >>> generateWith (1,2) uniform :: Double
-- 0.6502342391751404
uniform :: PCG.Variate a => Random a
uniform = liftRandom PCG.uniform
{-# INLINE uniform #-}

-- | /O(1)/ Generate a uniformly distributed random variate in the given range.
--
-- * Use inclusive range for integral types.
-- * Use (a,b] range for floating types.
--
-- >>> generateWith (1,2) $ uniformR 10 50 :: Int
-- 16
uniformR :: PCG.Variate a => a -> a -> Random a
uniformR lo hi = liftRandom $ PCG.uniformR (lo, hi)
{-# INLINE uniformR #-}

-- | /O(1)/ Generate a uniformly distributed random variate in the range [0,b).
--
-- * For integral types the bound must be less than the max bound of `Data.Word.Word32`
-- (4294967295). Behaviour is undefined for negative bounds.
--
-- >>> generateWith (1,2) $ uniformB 200 :: Int
-- 143
uniformB :: PCG.Variate a => a -> Random a
uniformB b = liftRandom $ PCG.uniformB b
{-# INLINE uniformB #-}

-- | /O(1)/ Generate a uniformly distributed random variate.
--
-- It should be equivalent to @`uniformR` (`minBound`, `maxBound`)@, but for non-`PCG.Variate`.
--
-- >>> import Prelude (Show)
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

-- | /O(1)/ Choose a single random value from a non-empty list.
--
-- >>> generateWith (1,2) $ choice ["hi", "hello", "ola"]
-- "hello"
choice :: NonEmpty a -> Random a
choice values = do
    i <- uniformB (length values)
    pure $ fromMaybe (headNE values) (values !? i)
  where
    headNE = head
{-# INLINE choice #-}

-- | /O(n)/ Generate weighted access table for a list of values.
--
-- >>> tabulate [(1, "hi"), (10, "hello")]
-- ([390451572],["hi","hello"])
tabulate :: NonEmpty (Double, a) -> (Unboxed.Vector Word32, Vector a)
tabulate (toList -> values) =
    ( Vector.init (fromList (map (toW32 . fst) values)),
      fromList (map snd values)
    )
  where
    maxWeight = sum (map fst values)
    maxW32 = fromIntegral (maxBound :: Word32)
    toW32 x = truncate (x * maxW32 / maxWeight)
{-# INLINE tabulate #-}

-- | /O(n)/ Choose a single random value from a non-empty with probability proportional to weight.
--
-- >>> import Control.Monad (replicateM)
-- >>> generateWith (1,2) $ replicateM 10 $ weightedChoice [(1, "hi"), (10, "hello")]
-- ["hello","hello","hello","hello","hi","hello","hello","hello","hello","hello"]
weightedChoice :: NonEmpty (Double, a) -> Random a
weightedChoice (tabulate -> (positions, values)) = do
    weight <- uniform
    let idx = binarySearch positions weight
    pure (values ! idx)
  where
    binarySearch v x = runST $ do
        -- SAFETY: binarySearchL does NOT modify the vector,
        -- I don't know why they chose to expose a mutable API only
        mv <- Vector.unsafeThaw v
        binarySearchL mv x
{-# INLINE weightedChoice #-}

-- | /O(n)/ Generates indices for `S.shuffle`.
--
-- See [random-shuffle](https://hackage.haskell.org/package/random-shuffle-0.0.4/docs/System-Random-Shuffle.html#v:shuffle).
treeIndices :: Int -> Random [Int]
treeIndices n = mapM uniformB [n, n - 1 .. 2]
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
-- [4,1,2,3,5]
shuffle :: IsList l => l -> Random l
shuffle values = case nonEmpty (toList values) of
    Just xs -> shuffleNonEmpty xs
    Nothing -> pure values
{-# INLINEABLE shuffle #-}

-- | /O(n)/ Generate random partitions of a vector.
--
-- >>> generateWith (1,4) $ partitions ([1..10] :: Vector Int)
-- [[1,2,3],[4,5,6,7,8],[9],[10]]
partitions :: Vector.Vector v a => v a -> Random [v a]
partitions xs = case Vector.length xs of
    0 -> pure []
    1 -> pure [xs]
    n -> do
        idx <- uniformB n
        let (part, rest) = Vector.splitAt (idx + 1) xs
        parts <- partitions rest
        pure (part : parts)
{-# INLINEABLE partitions #-}

-- | Generates an infinite list of random values.
--
-- >>> import Prelude (take, (<$>))
-- >>> generateWith (1,3) (take 3 <$> repeatR (uniform :: Random Int))
-- [-8858759364290496978,-2446124676014956441,1387719708863309077]
repeatR :: Random a -> Random [a]
repeatR r = lazyRandom $ do
    value <- r
    rest <- repeatR r
    pure (value : rest)
{-# INLINEABLE repeatR #-}

-- | Random version of `Data.List.iterate`.
--
-- >>> import Prelude ((<$>), (.), liftA2)
-- >>> import Data.List.NonEmpty (take)
-- >>> generateWith (1,3) $ take 3 <$> iterateR (liftA2 (+) uniform . pure) 2
-- [2.0,2.0197656927151786,2.3871610084947057]
iterateR :: (a -> Random a) -> a -> Random (NonEmpty a)
iterateR next value = lazyRandom $ do
    newValue <- next value
    rest <- iterateR next newValue
    pure (value <| rest)
{-# INLINEABLE iterateR #-}

-- ---------- --
-- QuickCheck --
-- ---------- --

infix 4 =~=

(=~=) :: (Eq a, Show a) => Random a -> Random a -> Property
genA =~= genB = property $ \seed ->
    generateWith seed genA === generateWith seed genB
