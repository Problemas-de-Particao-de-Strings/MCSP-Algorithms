-- | Generic strings using backed by a contiguous array of unboxed characters.
module Strings.Data.String (
    -- * Unboxed string
    String (.., Unboxed, Null, NonNull, Head, Last, Singleton, (:<), (:>), (:<:), (:>:)),
    Unbox,

    -- ** Text IO
    ShowString (..),
    ReadString (..),

    -- * Accessors

    -- ** Length information
    Strings.Data.String.length,
    Strings.Data.String.null,

    -- ** Indexing
    (!),
    (!?),
    head,
    last,
    unsafeIndex,
    single,
    indexM,
    headM,
    lastM,

    -- ** Substrings (slicing)
    slice,
    init,
    tail,
    take,
    drop,
    splitAt,
    uncons,
    unsnoc,
    unsafeSlice,

    -- * Construction

    -- ** Initialisation
    empty,
    singleton,
    replicate,
    generate,

    -- ** Monadic initialisation
    replicateM,
    generateM,
    create,

    -- ** Unfolding
    unfoldr,
    unfoldrExactN,
    unfoldrM,
    unfoldrExactNM,

    -- ** Enumeration
    enumFromN,
    enumFromStepN,

    -- ** Concatenation
    cons,
    snoc,
    (++),
    concat,
    concatNE,

    -- ** Restricting memory usage
    force,

    -- * Modifying vectors

    -- ** Bulk updates
    (//),
    update,

    -- ** Accumulations
    accum,
    accumulate,

    -- ** Permutations
    reverse,
    backpermute,

    -- ** Safe destructive updates
    modify,

    -- * Elementwise operations

    -- ** Indexing
    indexed,

    -- ** Mapping
    map,
    map_,
    imap,
    imap_,
    concatMap,
    concatMap_,
    mapM,
    mapM_,
    forM,
    iforM,

    -- ** Zipping
    zipWith,
    zipWith3,
    zip,
    zip3,
    zipWithM,
    zipWithM_,
    unzip,
    unzip3,

    -- * Working with predicates

    -- ** Filtering
    filter,
    ifilter,
    filterM,
    uniq,
    mapMaybe,
    mapMaybeM,
    takeWhile,
    dropWhile,

    -- ** Partitioning
    partition,
    partitionWith,
    unstablePartition,
    span,
    break,
    groupBy,
    group,

    -- ** Searching
    Strings.Data.String.elem,
    notElem,
    find,
    findIndex,
    findIndexR,
    findIndices,
    elemIndex,
    elemIndices,

    -- * Utilities
    eqBy,
    cmpBy,
    convert,
) where

import Control.Applicative (Alternative, (<$>))
import Control.Applicative qualified as Applicative (empty)
import Control.Arrow ((&&&))
import Control.Monad (Monad)
import Control.Monad.ST (ST)
import Data.Bool (Bool (False, True), otherwise, (&&))
import Data.Char (Char)
import Data.Data (Typeable)
import Data.Either (Either)
import Data.Eq (Eq (..))
import Data.Foldable (Foldable (..))
import Data.Function (id, (.))
import Data.Int (Int)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid (Monoid (..))
import Data.Ord (Ord (..), Ordering)
import Data.Semigroup (Semigroup (..), Sum (..))
import Data.Store (Size (..), Store (..))
import Data.String (IsString (..))
import Data.Word (Word8)
import GHC.Base (undefined, ($!))
import GHC.IsList (IsList (..))
import GHC.Num (Num, (+), (-))
import Text.Read (Read (readListPrec, readPrec), readListPrecDefault)
import Text.Show (Show (showsPrec))

import Data.Vector.Generic qualified as Generic
import Data.Vector.Generic.Mutable qualified as Mutable
import Data.Vector.Unboxed (MVector, Unbox, Vector)

import Strings.Data.String.Text (ReadString (..), ShowString (..), readCharsPrec)

-- --------------- --
-- Data definition --
-- --------------- --

-- | An unboxed string of characters @a@.
--
-- Implemented as a contiguous vector of unboxed characters.
data String a where
    -- | Construct a string from a unboxed vector.
    --
    -- Note that `Unbox` is only required for constructing the string. All other operations should be possible
    -- without that constraint.
    String :: Unbox a => !(Vector a) -> String a
    deriving newtype (Typeable)

{-# COMPLETE Unboxed #-}
{-# COMPLETE Null, NonNull #-}
{-# COMPLETE Null, Head #-}
{-# COMPLETE Null, Last #-}
{-# COMPLETE Null, (:<) #-}
{-# COMPLETE Null, (:>) #-}
{-# COMPLETE Null, (:<:) #-}
{-# COMPLETE Null, (:>:) #-}

-- | Proves `Unbox` from an already constructed string.
--
-- This pattern is useful for matching in operations where @Unbox a@ is required.
--
-- >>> import GHC.Base (asTypeOf)
-- >>> emptyLike s = asTypeOf empty s
-- >>> :t emptyLike
-- emptyLike :: Unbox a => String a -> String a
-- >>> emptyLike' s@Unboxed = asTypeOf empty s
-- >>> :t emptyLike'
-- emptyLike' :: String a -> String a
pattern Unboxed :: () => Unbox a => String a
pattern Unboxed <- (id -> String _)
{-# INLINE Unboxed #-}

-- | /O(1)/ Matches the `empty` string.
--
-- >>> [s | s@Null <- ["", "a", "ab", "", "abc"]]
-- [,]
pattern Null :: () => Unbox a => String a
pattern Null <- (Strings.Data.String.null &&& id -> (True, Unboxed))
{-# INLINE Null #-}

-- | /O(1)/ Matches any non-`empty` string.
--
-- >>> [s | NonNull s <- ["", "a", "ab", "", "abc"]]
-- [a,ab,abc]
pattern NonNull :: () => Unbox a => String a -> String a
pattern NonNull s <- (Strings.Data.String.null &&& id -> (False, s@Unboxed))
    where
        NonNull = id
{-# INLINE NonNull #-}

-- | /O(1)/ Matches the first character in a string.
--
-- >>> [c | Head c <- ["", "a", "ab", "", "abc"]]
-- "aaa"
pattern Head :: () => Unbox a => a -> String a
pattern Head c <- (headM &&& id -> (Just c, Unboxed))
{-# INLINE Head #-}

-- | /O(1)/ Matches the last character in a string.
--
-- >>> [c | Last c <- ["", "a", "ab", "", "abc"]]
-- "abc"
pattern Last :: () => Unbox a => a -> String a
pattern Last c <- (lastM &&& id -> (Just c, Unboxed))
{-# INLINE Last #-}

-- | /O(1)/ Matches a string composed of a single character.
--
-- >>> [c | Singleton c <- ["", "a", "ab", "", "abc"]]
-- "a"
pattern Singleton :: () => Unbox a => a -> String a
pattern Singleton c <- (single &&& id -> (Just c, Unboxed))
{-# INLINE Singleton #-}

-- | /O(1)/ Matches `head` and `tail` of a string, if present.
--
-- >>> [(h,t) | h :< t <- ["", "a", "ab", "", "abc"]]
-- [('a',),('a',b),('a',bc)]
pattern (:<) :: () => Unbox a => a -> String a -> String a
pattern x :< xs <- (uncons -> Just (x, xs@Unboxed))
    where
        x :< xs = cons x xs
{-# INLINE (:<) #-}

-- | /O(1)/ Matches `init` and `last` of a string, if present.
--
-- >>> [(i,l) | i :> l <- ["", "a", "ab", "", "abc"]]
-- [(,'a'),(a,'b'),(ab,'c')]
pattern (:>) :: () => Unbox a => String a -> a -> String a
pattern xs :> x <- (unsnoc -> Just (xs@Unboxed, x))
    where
        xs :> x = snoc xs x
{-# INLINE (:>) #-}

-- | /O(1)/ Stringified `:<`, matching `head` and `tail`.
--
-- >>> [(h,t) | h :<: t <- ["", "a", "ab", "", "abc"]]
-- [(a,),(a,b),(a,bc)]
pattern (:<:) :: () => Unbox a => String a -> String a -> String a
pattern x :<: xs <- (splitAtHead -> Just (x@Unboxed, xs))
{-# INLINE (:<:) #-}

-- | /O(1)/ Stringified `:>`, matching `init` and `last`.
--
-- >>> [(i,l) | i :>: l <- ["", "a", "ab", "", "abc"]]
-- [(,a),(a,b),(ab,c)]
pattern (:>:) :: () => Unbox a => String a -> String a -> String a
pattern xs :>: x <- (splitAtLast -> Just (xs, x@Unboxed))
{-# INLINE (:>:) #-}

-- -------------------------- --
-- Pattern matching functions --
-- -------------------------- --

-- | (INTERNAL) /O(1)/ Stringified version of `uncons`.
--
-- >>> splitAtHead "acgt"
-- Just (a,cgt)
splitAtHead :: String a -> Maybe (String a, String a)
splitAtHead s@Unboxed
    -- SAFETY: n > 0 guarantees head (0 .. 1) and tail (1 .. n) are present
    | n > 0 = Just (Generic.unsafeSlice 0 1 s, Generic.unsafeSlice 1 (n - 1) s)
    | otherwise = Nothing
  where
    n = Generic.length $! s
{-# INLINE splitAtHead #-}

-- | (INTERNAL) /O(1)/ Stringified version of `unsnoc`.
--
-- >>> splitAtLast "acgt"
-- Just (acg,t)
splitAtLast :: String a -> Maybe (String a, String a)
splitAtLast s@Unboxed
    -- SAFETY: n > 0 guarantees init (0 .. n-1) and last (n-1 .. n) are present
    | n > 0 = Just (Generic.unsafeSlice 0 (n - 1) s, Generic.unsafeSlice (n - 1) 1 s)
    | otherwise = Nothing
  where
    n = Generic.length $! s
{-# INLINE splitAtLast #-}

-- ----------------------------------- --
-- List-like and String-like instances --
-- ----------------------------------- --

instance Eq a => Eq (String a) where
    {-# SPECIALIZE instance Eq (String Char) #-}
    {-# SPECIALIZE instance Eq (String Int) #-}
    {-# SPECIALIZE instance Eq (String Word8) #-}
    (String lhs) == (String rhs) = lhs == rhs
    {-# INLINE (==) #-}
    (String lhs) /= (String rhs) = lhs /= rhs
    {-# INLINE (/=) #-}

instance Ord a => Ord (String a) where
    {-# SPECIALIZE instance Ord (String Char) #-}
    {-# SPECIALIZE instance Ord (String Int) #-}
    {-# SPECIALIZE instance Ord (String Word8) #-}
    (String lhs) `compare` (String rhs) = lhs `compare` rhs
    {-# INLINE compare #-}
    (String lhs) < (String rhs) = lhs < rhs
    {-# INLINE (<) #-}
    (String lhs) <= (String rhs) = lhs <= rhs
    {-# INLINE (<=) #-}
    (String lhs) > (String rhs) = lhs > rhs
    {-# INLINE (>) #-}
    (String lhs) >= (String rhs) = lhs >= rhs
    {-# INLINE (>=) #-}
    max (String lhs) (String rhs) = String (max lhs rhs)
    {-# INLINE max #-}
    min (String lhs) (String rhs) = String (min lhs rhs)
    {-# INLINE min #-}

instance Unbox a => IsList (String a) where
    {-# SPECIALIZE instance IsList (String Char) #-}
    {-# SPECIALIZE instance IsList (String Int) #-}
    {-# SPECIALIZE instance IsList (String Word8) #-}
    type Item (String a) = a
    fromList = Generic.fromList
    {-# INLINE fromList #-}
    fromListN = Generic.fromListN
    {-# INLINE fromListN #-}
    toList = Generic.toList
    {-# INLINE toList #-}

-- | `String` `Char` can be written using `Prelude.String` syntax (@"abcd"@).
instance IsString (String Char) where
    {-# SPECIALIZE instance IsString (String Char) #-}
    fromString = fromList
    {-# INLINE fromString #-}

instance Foldable String where
    fold s@Unboxed = Generic.foldMap id s
    {-# INLINE fold #-}
    foldMap f s@Unboxed = Generic.foldMap f s
    {-# INLINE foldMap #-}
    foldMap' f s@Unboxed = Generic.foldMap' f s
    {-# INLINE foldMap' #-}
    foldr f x s@Unboxed = Generic.foldr f x s
    {-# INLINE foldr #-}
    foldr' f x s@Unboxed = Generic.foldr' f x s
    {-# INLINE foldr' #-}
    foldl f x s@Unboxed = Generic.foldl f x s
    {-# INLINE foldl #-}
    foldl' f x s@Unboxed = Generic.foldl' f x s
    {-# INLINE foldl' #-}
    foldr1 f s@Unboxed = Generic.foldr1 f s
    {-# INLINE foldr1 #-}
    foldl1 f s@Unboxed = Generic.foldl1 f s
    {-# INLINE foldl1 #-}
    toList s@Unboxed = Generic.toList s
    {-# INLINE toList #-}
    null s@Unboxed = Generic.null s
    {-# INLINE null #-}
    length s@Unboxed = Generic.length s
    {-# INLINE length #-}
    elem x s@Unboxed = Generic.elem x s
    {-# INLINE elem #-}
    maximum s@Unboxed = Generic.maximum s
    {-# INLINE maximum #-}
    minimum s@Unboxed = Generic.minimum s
    {-# INLINE minimum #-}
    sum s@Unboxed = Generic.sum s
    {-# INLINE sum #-}
    product s@Unboxed = Generic.product s
    {-# INLINE product #-}

-- | `Semigroup` based on concatenation (@"a" <> "b" == "ab"@).
instance Semigroup (String a) where
    {-# SPECIALIZE instance Semigroup (String Char) #-}
    {-# SPECIALIZE instance Semigroup (String Int) #-}
    {-# SPECIALIZE instance Semigroup (String Word8) #-}
    (<>) = (++)
    {-# INLINE (<>) #-}
    sconcat = concatNE
    {-# INLINE sconcat #-}

-- | `Monoid` based on concatenation (@mempty == ""@).
instance Unbox a => Monoid (String a) where
    {-# SPECIALIZE instance Monoid (String Char) #-}
    {-# SPECIALIZE instance Monoid (String Int) #-}
    {-# SPECIALIZE instance Monoid (String Word8) #-}
    mempty = Generic.empty
    {-# INLINE mempty #-}
    mconcat = concat
    {-# INLINE mconcat #-}

-- ---------------------- --
-- Textual Input / Output --
-- ---------------------- --

instance ShowString a => Show (String a) where
    {-# SPECIALIZE instance Show (String Char) #-}
    {-# SPECIALIZE instance Show (String Int) #-}
    {-# SPECIALIZE instance Show (String Word8) #-}
    showsPrec _ s@Unboxed = showStr s
    {-# INLINE showsPrec #-}

instance (ReadString a, Unbox a) => Read (String a) where
    {-# SPECIALIZE instance Read (String Char) #-}
    {-# SPECIALIZE instance Read (String Int) #-}
    {-# SPECIALIZE instance Read (String Word8) #-}
    readPrec = fromList <$> readCharsPrec
    {-# INLINE readPrec #-}
    readListPrec = readListPrecDefault
    {-# INLINE readListPrec #-}

-- --------------------------------- --
-- Binary Input and Output instances --
-- --------------------------------- --

instance (Store a, Unbox a) => Store (String a) where
    {-# SPECIALIZE instance Store (String Char) #-}
    {-# SPECIALIZE instance Store (String Int) #-}
    {-# SPECIALIZE instance Store (String Word8) #-}
    size = VarSize calcSize
      where
        calcSize s = sizeOf size (Strings.Data.String.length s) + sizeSum s
        sizeOf (ConstSize n) _ = n
        sizeOf (VarSize f) x = f x
        sizeSum s@Unboxed = getSum (Generic.foldMap (Sum . sizeOf size) s)
    {-# INLINE size #-}
    poke s = do
        poke (Generic.length s)
        Generic.forM_ s poke
    {-# INLINE poke #-}
    peek = do
        n <- peek
        Generic.replicateM n peek
    {-# INLINE peek #-}

-- --------------------------------- --
-- Generic Vector instance and types --
-- --------------------------------- --

-- | Mutable variant of `String`, so that it can implement the `Generic Vector` interface.
newtype MString s a = MString {mContents :: MVector s a}

instance Unbox a => Mutable.MVector MString a where
    {-# SPECIALIZE instance Mutable.MVector MString Char #-}
    {-# SPECIALIZE instance Mutable.MVector MString Int #-}
    {-# SPECIALIZE instance Mutable.MVector MString Word8 #-}
    basicLength (MString v) = Mutable.basicLength v
    {-# INLINE basicLength #-}
    basicUnsafeSlice s n (MString v) = MString (Mutable.basicUnsafeSlice s n v)
    {-# INLINE basicUnsafeSlice #-}
    basicOverlaps (MString lhs) (MString rhs) = Mutable.basicOverlaps lhs rhs
    {-# INLINE basicOverlaps #-}
    basicUnsafeNew n = MString <$> Mutable.basicUnsafeNew n
    {-# INLINE basicUnsafeNew #-}
    basicInitialize (MString v) = Mutable.basicInitialize v
    {-# INLINE basicInitialize #-}
    basicUnsafeReplicate n x = MString <$> Mutable.basicUnsafeReplicate n x
    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeRead (MString v) = Mutable.basicUnsafeRead v
    {-# INLINE basicUnsafeRead #-}
    basicUnsafeWrite (MString v) = Mutable.basicUnsafeWrite v
    {-# INLINE basicUnsafeWrite #-}
    basicClear (MString v) = Mutable.basicClear v
    {-# INLINE basicClear #-}
    basicSet (MString v) = Mutable.basicSet v
    {-# INLINE basicSet #-}
    basicUnsafeCopy (MString tgt) (MString src) = Mutable.basicUnsafeCopy tgt src
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeMove (MString tgt) (MString src) = Mutable.basicUnsafeMove tgt src
    {-# INLINE basicUnsafeMove #-}
    basicUnsafeGrow (MString v) n = MString <$> Mutable.basicUnsafeGrow v n
    {-# INLINE basicUnsafeGrow #-}

type instance Generic.Mutable String = MString

instance Unbox a => Generic.Vector String a where
    {-# SPECIALIZE instance Generic.Vector String Char #-}
    {-# SPECIALIZE instance Generic.Vector String Int #-}
    {-# SPECIALIZE instance Generic.Vector String Word8 #-}
    basicUnsafeFreeze (MString v) = String <$> Generic.basicUnsafeFreeze v
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeThaw (String v) = MString <$> Generic.basicUnsafeThaw v
    {-# INLINE basicUnsafeThaw #-}
    basicLength (String v) = Generic.basicLength v
    {-# INLINE basicLength #-}
    basicUnsafeSlice s n (String v) = String (Generic.basicUnsafeSlice s n v)
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeIndexM (String v) = Generic.basicUnsafeIndexM v
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeCopy (MString mv) (String v) = Generic.basicUnsafeCopy mv v
    {-# INLINE basicUnsafeCopy #-}
    elemseq _ = Generic.elemseq (undefined :: Vector a)
    {-# INLINE elemseq #-}

-- --------------------------------------- --
-- Operations with lifted Unbox constraint --
-- --------------------------------------- --

-- ------------------ --
-- Length information --

-- | /O(1)/ Yield the length of the string.
length :: String a -> Int
length s@Unboxed = Generic.length s

-- | /O(1)/ Test whether a string is empty.
null :: String a -> Bool
null s@Unboxed = Generic.null s

-- -------- --
-- Indexing --

-- | /O(1)/ Indexing.
--
-- >>> "abc" ! 1
-- 'b'
(!) :: String a -> Int -> a
s@Unboxed ! i = s Generic.! i
{-# INLINE (!) #-}

-- | /O(1)/ Safe indexing.
--
-- >>> "abc" !? 1
-- Just 'b'
-- >>> "abc" !? 3
-- Nothing
(!?) :: String a -> Int -> Maybe a
s@Unboxed !? i = s Generic.!? i
{-# INLINE (!?) #-}

-- | /O(1)/ First character.
--
-- >>> head "hello"
-- 'h'
head :: String a -> a
head s@Unboxed = Generic.head s
{-# INLINE head #-}

-- | /O(1)/ Last character.
--
-- >>> last "hello"
-- 'o'
last :: String a -> a
last s@Unboxed = Generic.last s
{-# INLINE last #-}

-- | /O(1)/ Unsafe indexing without bounds checking.
unsafeIndex :: String a -> Int -> a
unsafeIndex s@Unboxed = Generic.unsafeIndex s
{-# INLINE unsafeIndex #-}

-- | /O(1)/ The character of a singleton string.
--
-- >>> single ""
-- Nothing
-- >>> single "x"
-- Just 'x'
-- >>> single "xy"
-- Nothing
single :: String a -> Maybe a
single s@Unboxed
    -- SAFETY: string with n == 1 has a character at index 0
    | n == 1 = Generic.unsafeIndexM s 0
    | otherwise = Applicative.empty
  where
    n = Generic.length $! s
{-# INLINE single #-}

-- | /O(1)/ Indexing in a monad.
--
-- See [Data.Vactor.Unbox](https://hackage.haskell.org/package/vector-0.13.0.0/docs/Data-Vector-Unboxed.html#v:indexM).
--
-- >>> indexM "xyz" 5 :: Maybe Char
-- Nothing
indexM :: (Alternative m, Monad m) => String a -> Int -> m a
indexM s@Unboxed i
    -- SAFETY: index i was checked manually
    | 0 <= i && i < n = Generic.unsafeIndexM s i
    | otherwise = Applicative.empty
  where
    n = Generic.length $! s
{-# INLINE indexM #-}

-- | /O(1)/ First character of a string in a monad.
--
-- See [Data.Vactor.Unbox](https://hackage.haskell.org/package/vector-0.13.0.0/docs/Data-Vector-Unboxed.html#v:indexM).
--
-- >>> headM "" :: Maybe Char
-- Nothing
headM :: (Alternative m, Monad m) => String a -> m a
headM s@Unboxed
    -- SAFETY: index 0 was checked manually
    | n > 0 = Generic.unsafeIndexM s 0
    | otherwise = Applicative.empty
  where
    n = Generic.length $! s
{-# INLINE headM #-}

-- | /O(1)/ Last character of a string in a monad.
--
-- See [Data.Vactor.Unbox](https://hackage.haskell.org/package/vector-0.13.0.0/docs/Data-Vector-Unboxed.html#v:indexM).
--
-- >>> lastM "" :: Maybe Char
-- Nothing
lastM :: (Alternative m, Monad m) => String a -> m a
lastM s@Unboxed
    -- SAFETY: positive n implies n-1 is a valid index
    | n > 0 = Generic.unsafeIndexM s (n - 1)
    | otherwise = Applicative.empty
  where
    n = Generic.length $! s
{-# INLINE lastM #-}

-- -------------------- --
-- Substrings (slicing) --

-- | /O(1)/ Yield the slice `s[i:i+n]` of the string without copying it.
--
-- The string must contain at least `i+n` characters.
--
-- >>> slice 2 3 "genome"
-- nom
slice :: Int -> Int -> String a -> String a
slice i n s@Unboxed = Generic.slice i n s
{-# INLINE slice #-}

-- | /O(1)/ Yield all but the last character without copying.
--
-- The string may not be empty.
--
-- >>> init "genome"
-- genom
init :: String a -> String a
init s@Unboxed = Generic.init s
{-# INLINE init #-}

-- | /O(1)/ Yield all but the first character without copying.
--
-- The string may not be empty.
--
-- >>> tail "genome"
-- enome
tail :: String a -> String a
tail s@Unboxed = Generic.tail s
{-# INLINE tail #-}

-- | /O(1)/ Yield at the first @n@ characters without copying.
--
-- The string may contain less than @n@ characters, in which case it is returned unchanged.
--
-- >>> take 2 "hello"
-- he
-- >>> take 10 "hello"
-- hello
take :: Int -> String a -> String a
take n s@Unboxed = Generic.take n s
{-# INLINE take #-}

-- | /O(1)/ Yield all but the first @n@ characters without copying.
--
-- The string may contain less than @n@ characters, in which case an empty string is returned.
--
-- >>> drop 2 "hello"
-- llo
-- >>> drop 10 "hello"
-- <BLANKLINE>
drop :: Int -> String a -> String a
drop n s@Unboxed = Generic.drop n s
{-# INLINE drop #-}

-- | /O(1)/ Yield the first @n@ characters paired with the remainder, without copying.
--
-- Note that `splitAt n s` is equivalent to `(take n s, drop n s)`, but slightly more efficient.
--
-- >>> splitAt 6 "babushka"
-- (babush,ka)
splitAt :: Int -> String a -> (String a, String a)
splitAt n s@Unboxed = Generic.splitAt n s
{-# INLINE splitAt #-}

-- | /O(1)/ Yield the `head` and `tail` of the string, or `Nothing` if it is empty.
--
-- >>> uncons "acgt"
-- Just ('a',cgt)
uncons :: String a -> Maybe (a, String a)
uncons s@Unboxed = Generic.uncons s
{-# INLINE uncons #-}

-- | /O(1)/ Yield the `init` and `last` of the string, or `Nothing` if it is empty.
--
-- >>> unsnoc "acgt"
-- Just (acg,'t')
unsnoc :: String a -> Maybe (String a, a)
unsnoc s@Unboxed = Generic.unsnoc s
{-# INLINE unsnoc #-}

-- | /O(1)/ Yield a slice of the string without copying.
--
-- The string must contain at least `i+n` characters, but this is not checked.
unsafeSlice :: Int -> Int -> String a -> String a
unsafeSlice i n s@Unboxed = Generic.unsafeSlice i n s
{-# INLINE unsafeSlice #-}

-- -------------- --
-- Initialisation --

-- | /O(1)/ The empty string.
--
-- >>> empty == ""
-- True
empty :: Unbox a => String a
empty = Generic.empty
{-# INLINE empty #-}

-- | /O(1)/ A string with exactly one character.
--
-- >>> singleton 's'
-- s
singleton :: Unbox a => a -> String a
singleton = Generic.singleton
{-# INLINE singleton #-}

-- | /O(n)/ A string of the given length with the same character in each position.
--
-- >>> replicate 10 'a'
-- aaaaaaaaaa
replicate :: Unbox a => Int -> a -> String a
replicate = Generic.replicate
{-# INLINE replicate #-}

-- | /O(n)/ Construct a string of the given length by applying the function to each index.
--
-- >>> import Prelude (head)
-- >>> generate 5 (\i -> Prelude.head (show i))
-- 01234
generate :: Unbox a => Int -> (Int -> a) -> String a
generate = Generic.generate
{-# INLINE generate #-}

-- ---------------------- --
-- Monadic initialisation --

-- | /O(n)/ Execute the monadic action the given number of times and store the results in a string.
--
-- >>> replicateM 4 (Just 'v')
-- Just vvvv
replicateM :: (Unbox a, Monad m) => Int -> m a -> m (String a)
replicateM = Generic.replicateM
{-# INLINE replicateM #-}

-- | /O(n)/ Construct a string of the given length by applying the monadic action to each index.
--
-- >>> generateM 5 (\i -> Just i)
-- Just 0 1 2 3 4
generateM :: (Unbox a, Monad m) => Int -> (Int -> m a) -> m (String a)
generateM = Generic.generateM
{-# INLINE generateM #-}

-- | /O(f(n))/ Execute the monadic action and freeze the resulting string.
--
-- >>> import Control.Applicative (pure)
-- >>> import Data.Vector.Unboxed.Mutable (new, write)
-- >>> create (do v <- new 2; write v 0 'a'; write v 1 'b'; pure v)
-- ab
create :: Unbox a => (forall s. ST s (MVector s a)) -> String a
create f = Generic.create (MString <$> f)
{-# INLINE create #-}

-- --------- --
-- Unfolding --

-- | /O(n)/ Construct a string by repeatedly applying the generator function to a seed.
--
-- The generator function yields -- `Just` the next character and the new seed or `Nothing` if there are no more
-- character.
--
-- >>> unfoldr (\n -> if n == 0 then Nothing else Just (n, n-1)) (10 :: Int)
-- 10 9 8 7 6 5 4 3 2 1
unfoldr :: Unbox a => (b -> Maybe (a, b)) -> b -> String a
unfoldr = Generic.unfoldr
{-# INLINE unfoldr #-}

-- | /O(n)/ Construct a vector with exactly @n@ characters by repeatedly applying the generator function to a seed.
--
-- The generator function yields the next character and the new seed.
--
-- >>> unfoldrExactN 3 (\n -> (n, n-1)) (10 :: Int)
-- 10 9 8
unfoldrExactN :: Unbox a => Int -> (b -> (a, b)) -> b -> String a
unfoldrExactN = Generic.unfoldrExactN
{-# INLINE unfoldrExactN #-}

-- | /O(n)/ Construct a string by repeatedly applying the generator function to a seed.
--
-- The generator function yields -- `Just` the next character and the new seed or `Nothing` if there are no more
-- character.
--
-- >>> unfoldr (\n -> if n == 0 then Nothing else Just (n, n-1)) (10 :: Int)
-- 10 9 8 7 6 5 4 3 2 1
unfoldrM :: (Unbox a, Monad m) => (b -> m (Maybe (a, b))) -> b -> m (String a)
unfoldrM = Generic.unfoldrM
{-# INLINE unfoldrM #-}

-- | /O(n)/ Construct a string with exactly @n@ characters by repeatedly applying the generator function to a seed.
--
-- The generator function yields the next character and the new seed.
--
-- >>> unfoldrExactN 3 (\n -> (n, n-1)) (10 :: Int)
-- 10 9 8
unfoldrExactNM :: (Unbox a, Monad m) => Int -> (b -> m (a, b)) -> b -> m (String a)
unfoldrExactNM = Generic.unfoldrExactNM
{-# INLINE unfoldrExactNM #-}

-- ----------- --
-- Enumeration --

-- | /O(n)/ Yield a string of the given length, containing the characters @x@, @x+1@ etc.
--
-- This operation is usually more efficient than `Data.Vector.Generic.enumFromTo`.
--
-- >>> enumFromN 5 3 :: String Int
-- 5 6 7
enumFromN :: (Unbox a, Num a) => a -> Int -> String a
enumFromN = Generic.enumFromN
{-# INLINE enumFromN #-}

-- | /O(n)/ Yield a string of the given length, containing the characters @x@, @x+y@, @x+y+y@ etc.
--
-- This operations is usually more efficient than `Data.Vector.Generic.enumFromThenTo`.
--
-- >>> enumFromStepN 1 2 5 :: String Int
-- 1 3 5 7 9
enumFromStepN :: (Unbox a, Num a) => a -> a -> Int -> String a
enumFromStepN = Generic.enumFromStepN
{-# INLINE enumFromStepN #-}

-- ------------- --
-- Concatenation --

-- | /O(n)/ Prepend a character.
--
-- >>> cons 'e' "xtrem"
-- extrem
cons :: a -> String a -> String a
cons ch s@Unboxed = Generic.cons ch s
{-# INLINE cons #-}

-- | /O(n)/ Append a character.
--
-- >>> snoc "xtrem" 'a'
-- xtrema
snoc :: String a -> a -> String a
snoc s@Unboxed = Generic.snoc s
{-# INLINE snoc #-}

-- | /O(m+n)/ Concatenate two strings.
--
-- >>> "abc" ++ "xyz"
-- abcxyz
(++) :: String a -> String a -> String a
l@Unboxed ++ r = l Generic.++ r
{-# INLINE (++) #-}

-- | /O(n)/ Concatenate all strings in the list.
--
-- This is the simplest variant, but requires `Unbox a`.
--
-- >>> concat ["abc", "123", "def"]
-- abc123def
-- >>> concat ([] :: [String Char])
-- <BLANKLINE>
concat :: Unbox a => [String a] -> String a
concat = Generic.concat
{-# INLINE concat #-}

-- | /O(n)/ Concatenate all strings in the non-empty list.
--
-- >>> concatNE ("abc" :| ["123", "def"])
-- abc123def
concatNE :: NonEmpty (String a) -> String a
concatNE strs@(Unboxed :| _) = Generic.concatNE strs
{-# INLINE concatNE #-}

-- ------------------------ --
-- Restricting memory usage --

-- | /O(n)/ Yield the argument, but force it not to retain any extra memory, possibly by copying it.
--
-- See [Data.Vector.Unbox](https://hackage.haskell.org/package/vector-0.13.0.0/docs/Data-Vector-Unboxed.html#v:force).
force :: String a -> String a
force s@Unboxed = Generic.force s
{-# INLINE force #-}

-- ------------ --
-- Bulk updates --

-- | /O(m+n)/ For each pair `(i,a)` from the list of index/value pairs, replace the character at position @i@ by @a@.
--
-- >>> "test" // [(2,'x'),(0,'y'),(2,'z')]
-- yezt
(//) :: String a -> [(Int, a)] -> String a
s@Unboxed // idx = s Generic.// idx
{-# INLINE (//) #-}

-- | /O(m+min(n1,n2))/ For each index @i@ from the index list and the corresponding value a from another string,
-- replace the character of the initial string at position @i@ by @a@.
--
-- >>> update "test" [2,0,2] "xyz"
-- yezt
update :: String a -> [Int] -> String a -> String a
update s@Unboxed idx = Generic.update_ s (fromList idx)
{-# INLINE update #-}

-- ------------- --
-- Accumulations --

-- | /O(m+n)/ For each pair @(i,b)@ from the list, replace the character at position @i@ by @f a b@.
--
-- >>> accum (+) [1000,2000,3000] [(2,4),(1,6),(0,3),(1,10)] :: String Int
-- 1003 2016 3004
accum :: (a -> b -> a) -> String a -> [(Int, b)] -> String a
accum f s@Unboxed = Generic.accum f s
{-# INLINE accum #-}

-- | /O(m+min(n1,n2))/ For each index @i@ from the index list and the corresponding value @b@ from the string,
-- replace the character of the initial string at position @i@ by @f a b@.
--
-- >>> accumulate (+) [5,9,2] [2,1,0,1] [4,6,3,7] :: String Int
-- 8 22 6
accumulate :: (a -> b -> a) -> String a -> [Int] -> String b -> String a
accumulate f s@Unboxed idx v@Unboxed = Generic.accumulate_ f s (fromList idx) v
{-# INLINE accumulate #-}

-- ------------ --
-- Permutations --

-- | /O(n)/ Reverse a string.
--
-- >>> reverse "abc123"
-- 321cba
reverse :: String a -> String a
reverse s@Unboxed = Generic.reverse s
{-# INLINE reverse #-}

-- | /O(n)/ Yield the string obtained by replacing each element @i@ of the index list by `xs!i`.
--
-- This is equivalent to `map (xs!)` is, but is often much more efficient.
--
-- >>> backpermute "abcd" [0,3,2,3,1,0]
-- adcdba
backpermute :: String a -> [Int] -> String a
backpermute s@Unboxed idx = Generic.backpermute s (Generic.fromList idx)
{-# INLINE backpermute #-}

-- ------------------------ --
-- Safe destructive updates --

-- | /O(f(n))/ Apply a destructive operation to a string.
--
-- The operation will be performed in place if it is safe to do so and will modify a copy of the vector otherwise.
--
-- >>> import Data.Vector.Unboxed.Mutable (write)
-- >>> modify (\v -> write v 3 'X') (replicate 10 'a')
-- aaaXaaaaaa
modify :: (forall s. MVector s a -> ST s ()) -> String a -> String a
modify f s@Unboxed = Generic.modify (f . mContents) s
{-# INLINE modify #-}

-- -------- --
-- Indexing --

-- | /O(n)/ Pair each character in a string with its index.
--
-- >>> indexed "greedy"
-- (0,'g')(1,'r')(2,'e')(3,'e')(4,'d')(5,'y')
indexed :: String a -> String (Int, a)
indexed s@Unboxed = Generic.indexed s
{-# INLINE indexed #-}

-- ------- --
-- Mapping --

-- | /O(n)/ Map a function over a string.
--
-- >>> import Data.Char (ord)
-- >>> map ord "genome"
-- 103 101 110 111 109 101
map :: Unbox b => (a -> b) -> String a -> String b
map f s@Unboxed = Generic.map f s
{-# INLINE map #-}

-- | /O(n)/ Map an endofunction over a string.
--
-- >>> import Data.Char (toUpper)
-- >>> map_ toUpper "genome"
-- GENOME
map_ :: (a -> a) -> String a -> String a
map_ f s@Unboxed = Generic.map f s
{-# INLINE map_ #-}

-- | /O(n)/ Apply a function to every character of a string and its index.
--
-- >>> import Data.Char (ord)
-- >>> imap (\i c -> i + ord c) "genome"
-- 103 102 112 114 113 106
imap :: Unbox b => (Int -> a -> b) -> String a -> String b
imap f s@Unboxed = Generic.imap f s
{-# INLINE imap #-}

-- | /O(n)/ Apply an endofunction to every character of a string and its index.
--
-- >>> import Data.Char (chr, ord)
-- >>> imap_ (\i c -> chr (i + ord c)) "genome"
-- gfprqj
imap_ :: (Int -> a -> a) -> String a -> String a
imap_ f s@Unboxed = Generic.imap f s
{-# INLINE imap_ #-}

-- | /O(?)/ Map a function over a string and concatenate the results.
--
-- >>> concatMap (\c -> [c, c]) "genome"
-- ggeennoommee
concatMap :: Unbox b => (a -> [b]) -> String a -> String b
concatMap f s@Unboxed = Generic.concatMap (fromList . f) s
{-# INLINE concatMap #-}

-- | /O(?)/ Map a function over a string and concatenate the resulting strings.
--
-- >>> concatMap_ (\c -> replicate 3 c) "gen"
-- gggeeennn
concatMap_ :: (a -> String a) -> String a -> String a
concatMap_ f s@Unboxed = Generic.concatMap f s
{-# INLINE concatMap_ #-}

-- --------------- --
-- Monadic mapping --

-- | /O(n)/ Apply the monadic action to all characters of the string, yielding a string of results.
mapM :: (Monad m, Unbox b) => (a -> m b) -> String a -> m (String b)
mapM f s@Unboxed = Generic.mapM f s
{-# INLINE mapM #-}

-- | /O(n)/ Apply the monadic action to all characters of the string, yielding a string of results.
mapM_ :: Monad m => (a -> m a) -> String a -> m (String a)
mapM_ f s@Unboxed = Generic.mapM f s
{-# INLINE mapM_ #-}

-- | /O(n)/ Apply the monadic action to all characters of a string and ignore the results.
forM :: Monad m => String a -> (a -> m b) -> m ()
forM s@Unboxed = Generic.forM_ s
{-# INLINE forM #-}

-- | /O(n)/ Apply the monadic action to all characters of a string and their indices and ignore the results.
iforM :: Monad m => String a -> (Int -> a -> m b) -> m ()
iforM s@Unboxed = Generic.iforM_ s
{-# INLINE iforM #-}

-- ------- --
-- Zipping --

-- | /O(min(m,n))/ Zip two strings with the given function.
zipWith :: Unbox c => (a -> b -> c) -> String a -> String b -> String c
zipWith f sa@Unboxed sb@Unboxed = Generic.zipWith f sa sb
{-# INLINE zipWith #-}

-- | /O(min(m,n,k))/ Zip three strings with the given function.
zipWith3 :: Unbox d => (a -> b -> c -> d) -> String a -> String b -> String c -> String d
zipWith3 f sa@Unboxed sb@Unboxed sc@Unboxed = Generic.zipWith3 f sa sb sc
{-# INLINE zipWith3 #-}

-- | /O(min(m,n))/ Zip two strings.
zip :: String a -> String b -> String (a, b)
zip sa@Unboxed sb@Unboxed = Generic.zip sa sb
{-# INLINE zip #-}

-- | /O(min(m,n))/ Zip three strings.
zip3 :: String a -> String b -> String c -> String (a, b, c)
zip3 sa@Unboxed sb@Unboxed sc@Unboxed = Generic.zip3 sa sb sc
{-# INLINE zip3 #-}

-- --------------- --
-- Monadic zipping --

-- | /O(min(m,n))/ Zip the two strings with the monadic action and yield a vector of results.
zipWithM :: (Monad m, Unbox c) => (a -> b -> m c) -> String a -> String b -> m (String c)
zipWithM f sa@Unboxed sb@Unboxed = Generic.zipWithM f sa sb
{-# INLINE zipWithM #-}

-- | /O(min(m,n))/ Zip the two strings with the monadic action and ignore the results.
zipWithM_ :: Monad m => (a -> b -> m c) -> String a -> String b -> m ()
zipWithM_ f sa@Unboxed sb@Unboxed = Generic.zipWithM_ f sa sb
{-# INLINE zipWithM_ #-}

-- --------- --
-- Unzipping --

-- | /O(n)/ Unzip a string of pairs.
unzip :: (Unbox a, Unbox b) => String (a, b) -> (String a, String b)
unzip = Generic.unzip
{-# INLINE unzip #-}

-- | /O(n)/ Unzip a string of triples.
unzip3 :: (Unbox a, Unbox b) => String (a, b) -> (String a, String b)
unzip3 = Generic.unzip
{-# INLINE unzip3 #-}

-- --------- --
-- Filtering --

-- | /O(n)/ Drop all characters that do not satisfy the predicate.
--
-- >>> import Data.Char (isUpper)
-- >>> filter isUpper "ABCdefGHI"
-- ABCGHI
filter :: (a -> Bool) -> String a -> String a
filter f s@Unboxed = Generic.filter f s
{-# INLINE filter #-}

-- | /O(n)/ Drop all characters that do not satisfy the predicate which is applied to the values and their indices.
ifilter :: (Int -> a -> Bool) -> String a -> String a
ifilter f s@Unboxed = Generic.ifilter f s
{-# INLINE ifilter #-}

-- | /O(n)/ Drop all characters that do not satisfy the monadic predicate.
filterM :: Monad m => (a -> m Bool) -> String a -> m (String a)
filterM f s@Unboxed = Generic.filterM f s
{-# INLINE filterM #-}

-- | /O(n)/ Drop repeated adjacent characters.
--
-- >>> uniq "aaaabbbcccaabc"
-- abcabc
uniq :: Eq a => String a -> String a
uniq s@Unboxed = Generic.uniq s
{-# INLINE uniq #-}

-- | /O(n)/ Map the values and collect the `Just` results.
mapMaybe :: Unbox b => (a -> Maybe b) -> String a -> String b
mapMaybe f s@Unboxed = Generic.mapMaybe f s
{-# INLINE mapMaybe #-}

-- | /O(n)/ Apply the monadic function to each element of the string and discard characters returning `Nothing`.
mapMaybeM :: (Monad m, Unbox b) => (a -> m (Maybe b)) -> String a -> m (String b)
mapMaybeM f s@Unboxed = Generic.mapMaybeM f s
{-# INLINE mapMaybeM #-}

-- | /O(n)/ Yield the longest prefix of characters satisfying the predicate.
--
-- The current implementation is not copy-free, unless the result string is fused away.
takeWhile :: (a -> Bool) -> String a -> String a
takeWhile f s@Unboxed = Generic.takeWhile f s
{-# INLINE takeWhile #-}

-- | /O(n)/ Drop the longest prefix of characters that satisfy the predicate without copying.
dropWhile :: (a -> Bool) -> String a -> String a
dropWhile f s@Unboxed = Generic.takeWhile f s
{-# INLINE dropWhile #-}

-- ------------ --
-- Partitioning --

-- | /O(n)/ Split the string in two parts, the first one containing those characters that satisfy the predicate and the
-- second one those that don't.
--
-- The relative order of the characters is preserved at the cost of a sometimes reduced performance compared to
-- `unstablePartition`.
partition :: (a -> Bool) -> String a -> (String a, String a)
partition f s@Unboxed = Generic.partition f s
{-# INLINE partition #-}

-- | /O(n)/ Split the string into two parts, the first one containing the `Prelude.Left` characters and the second
-- containing the `Prelude.Right` characters.
--
-- The relative order of the characters is preserved.
partitionWith :: (Unbox b, Unbox c) => (a -> Either b c) -> String a -> (String b, String c)
partitionWith f s@Unboxed = Generic.partitionWith f s
{-# INLINE partitionWith #-}

-- | /O(n)/ Split the string in two parts, the first one containing those characters that satisfy the predicate and
-- the second one those that don't.
--
-- The order of the characters is not preserved, but the operation is often faster than `partition`.
unstablePartition :: (a -> Bool) -> String a -> (String a, String a)
unstablePartition f s@Unboxed = Generic.unstablePartition f s
{-# INLINE unstablePartition #-}

-- | /O(n)/ Split the string into the longest prefix of characters that satisfy the predicate and the rest without
-- copying.
span :: (a -> Bool) -> String a -> (String a, String a)
span f s@Unboxed = Generic.span f s
{-# INLINE span #-}

-- | /O(n)/ Split the string into the longest prefix of characters that do not satisfy the predicate and the rest
-- without copying.
break :: (a -> Bool) -> String a -> (String a, String a)
break f s@Unboxed = Generic.break f s
{-# INLINE break #-}

-- | /O(n)/ Split a string into a list of slices.
--
-- The concatenation of this list of slices is equal to the argument string, and each slice contains only equal
-- characters, as determined by the equality predicate function.
--
-- >>> import Data.Char (isUpper)
-- >>> groupBy (\x y -> isUpper x == isUpper y) "Rio Grande"
-- [R,io ,G,rande]
groupBy :: (a -> a -> Bool) -> String a -> [String a]
groupBy f s@Unboxed = Generic.groupBy f s
{-# INLINE groupBy #-}

-- | /O(n)/ Split a string into a list of slices.
--
-- The concatenation of this list of slices is equal to the argument string, and each slice contains only equal
-- characters.
--
-- This is the equivalent of 'groupBy (==)'.
--
-- >>> group "Mississippi"
-- [M,i,ss,i,ss,i,pp,i]
group :: Eq a => String a -> [String a]
group s@Unboxed = Generic.group s
{-# INLINE group #-}

-- --------- --
-- Searching --

-- | /O(n)/ Check if the string contains a character.
elem :: Eq a => a -> String a -> Bool
elem c s@Unboxed = Generic.elem c s
{-# INLINE elem #-}

-- | /O(n)/ Check if the string does not contain a character (inverse of `Strings.Data.String.elem`).
notElem :: Eq a => a -> String a -> Bool
notElem c s@Unboxed = Generic.notElem c s
{-# INLINE notElem #-}

-- | /O(n)/ Yield `Just` the first character matching the predicate or `Nothing` if no such character exists.
find :: (a -> Bool) -> String a -> Maybe a
find f s@Unboxed = Generic.find f s
{-# INLINE find #-}

-- | /O(n)/ Yield `Just` the index of the first character matching the predicate or `Nothing` if no such character
-- exists.
findIndex :: (a -> Bool) -> String a -> Maybe Int
findIndex f s@Unboxed = Generic.findIndex f s
{-# INLINE findIndex #-}

-- | /O(n)/ Yield `Just` the index of the last character matching the predicate or `Nothing` if no such character
-- exists.
findIndexR :: (a -> Bool) -> String a -> Maybe Int
findIndexR f s@Unboxed = Generic.findIndexR f s
{-# INLINE findIndexR #-}

-- | /O(n)/ Yield the indices of character satisfying the predicate in ascending order.
findIndices :: (a -> Bool) -> String a -> String Int
findIndices f s@Unboxed = Generic.findIndices f s
{-# INLINE findIndices #-}

-- | /O(n)/ Yield `Just` the index of the first occurrence of the given character or `Nothing` if the vector does not
-- contain the character.
--
-- This is a specialised version of `findIndex`.
elemIndex :: Eq a => a -> String a -> Maybe Int
elemIndex c s@Unboxed = Generic.elemIndex c s
{-# INLINE elemIndex #-}

-- | /O(n)/ Yield the indices of all occurrences of the given character in ascending order.
--
-- This is a specialised version of `findIndices`.
elemIndices :: Eq a => a -> String a -> String Int
elemIndices c s@Unboxed = Generic.elemIndices c s
{-# INLINE elemIndices #-}

-- ----------- --
-- Comparisons --

-- | /O(n)/ Check if two strings are equal using the supplied equality predicate.
--
-- >>> import Data.Char (toLower)
-- >>> eqBy (\x y -> toLower x == toLower y) "ABcd" "abcD"
-- True
eqBy :: (a -> b -> Bool) -> String a -> String b -> Bool
eqBy f l@Unboxed r@Unboxed = Generic.eqBy f l r
{-# INLINE eqBy #-}

-- | /O(n)/ Compare two strings using the supplied comparison function for characters.
--
-- Comparison works the same as for lists.
cmpBy :: (a -> b -> Ordering) -> String a -> String b -> Ordering
cmpBy f l@Unboxed r@Unboxed = Generic.cmpBy f l r
{-# INLINE cmpBy #-}

-- ------------------ --
-- Other vector types --

-- | /O(n)/ Convert to a vector of characters.
convert :: Generic.Vector v a => String a -> v a
convert s@Unboxed = Generic.convert s
{-# INLINE convert #-}
