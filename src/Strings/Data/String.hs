{-# LANGUAGE NoImplicitPrelude #-}

module Strings.Data.String (
    -- * Unboxed string
    String (.., Unboxed, Null, NonNull, Head, Last, Singleton, (:<), (:>), (:<:), (:>:)),
    Unbox,

    -- ** Text IO
    ShowString (..),
    ReadString (..),

    -- * Accessors

    -- ** Indexing
    (!),
    (!?),
    head,
    last,
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

    -- * Creation
    replicateM,

    -- * Modification

    -- ** Concatenation
    cons,
    snoc,
    (++),
    concat,
    concatNE,
    maybeConcat,

    -- ** Restricting memory usage
    force,

    -- ** Permutations
    reverse,
    backpermute,

    -- ** Updates
    modify,

    -- * Information
    frequency,
    singletons,
    hasOneOf,

    -- * Other operations
    eqBy,
    cmpBy,
    convert,
    empty,
) where

import Control.Applicative (Alternative, (<$>))
import Control.Applicative qualified as Applicative (empty)
import Control.Monad (Monad)
import Control.Monad.ST (ST)
import Data.Bool (Bool, otherwise, (&&))
import Data.Char (Char)
import Data.Data (Typeable)
import Data.Eq (Eq (..))
import Data.Foldable (Foldable (..), any)
import Data.Function (id, ($), (.))
import Data.Int (Int)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Map.Strict qualified as Map (Map, alter, empty, foldrWithKey')
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Data.Monoid (Monoid (..))
import Data.Ord (Ord (..), Ordering)
import Data.Semigroup (Semigroup (..), Sum (..))
import Data.Set qualified as Set (Set, empty, insert, member)
import Data.Store (Size (..), Store (..))
import Data.String (IsString (..))
import Data.Type.Equality (type (~))
import Data.Word (Word8)
import GHC.Base (undefined, ($!))
import GHC.IsList (IsList (..))
import GHC.Num ((+), (-))
import Text.Read (Read (readListPrec, readPrec), readListPrecDefault)
import Text.Show (Show (show, showsPrec))

import Data.Vector.Generic qualified as Generic
import Data.Vector.Generic.Mutable qualified as Mutable
import Data.Vector.Unboxed (MVector, Unbox, Vector)

import Strings.Data.String.Text (ReadString (..), ShowString (..), readCharsPrec)

-- --------------- --
-- Data definition --
-- --------------- --

-- | An unboxed string of characters `a`.
--
-- Implemented as a unboxed vector.
data String a where
    -- | Construct a string from a unboxed vector.
    --
    -- Note that `Unbox a` is only required for constructing the string. All other operations should be possible
    -- without that constraint.
    String :: Unbox a => !(Vector a) -> String a
    deriving newtype (Typeable)

{-# COMPLETE Unboxed #-}
{-# COMPLETE Null, NonNull #-}
{-# COMPLETE Null, (:<) #-}
{-# COMPLETE Null, (:>) #-}
{-# COMPLETE Null, (:<:) #-}
{-# COMPLETE Null, (:>:) #-}

-- | Proves `Unbox a` from an already constructed string.
--
-- This pattern is useful for matching in operations where `Unbox a` is required.
--
-- >>> import GHC.Base (asTypeOf)
-- >>> emptyLike s = asTypeOf empty s
-- >>> :t emptyLike
-- emptyLike :: Unbox a => String a -> String a
-- >>> emptyLike' s@Unboxed = asTypeOf empty s
-- >>> :t emptyLike'
-- emptyLike' :: String a -> String a
pattern Unboxed :: () => Unbox a => String a
pattern Unboxed <- (id -> (String _))
{-# INLINE Unboxed #-}

-- | Matches the `empty` string.
--
-- >>> [s | s@Null <- ["", "a", "ab", "", "abc"]]
-- [,]
pattern Null :: () => Unbox a => String a
pattern Null <- (nullish -> Just Unboxed)
{-# INLINE Null #-}

-- | Matches any non-`empty` string.
--
-- >>> [s | NonNull s <- ["", "a", "ab", "", "abc"]]
-- [a,ab,abc]
pattern NonNull :: () => Unbox a => String a -> String a
pattern NonNull s <- (nonNull -> Just s@Unboxed)
    where
        NonNull = id
{-# INLINE NonNull #-}

-- | Matches the first character in a string.
--
-- >>> [c | Head c <- ["", "a", "ab", "", "abc"]]
-- "aaa"
pattern Head :: () => Unbox a => a -> String a
pattern Head c <- (headId -> (Just c, Unboxed))
{-# INLINE Head #-}

-- | Matches the last character in a string.
--
-- >>> [c | Last c <- ["", "a", "ab", "", "abc"]]
-- "abc"
pattern Last :: () => Unbox a => a -> String a
pattern Last c <- (lastId -> (Just c, Unboxed))
{-# INLINE Last #-}

-- | Matches a string composed of a single character.
--
-- >>> [c | Singleton c <- ["", "a", "ab", "", "abc"]]
-- "a"
pattern Singleton :: () => Unbox a => a -> String a
pattern Singleton c <- (singleId -> (Just c, Unboxed))
{-# INLINE Singleton #-}

-- | Matches `head` and `tail` of a string, if present.
--
-- >>> [(h,t) | h :< t <- ["", "a", "ab", "", "abc"]]
-- [('a',),('a',b),('a',bc)]
pattern (:<) :: () => Unbox a => a -> String a -> String a
pattern x :< xs <- (uncons -> Just (x, xs@Unboxed))
    where
        x :< xs = cons x xs
{-# INLINE (:<) #-}

-- | Matches `init` and `last` of a string, if present.
--
-- >>> [(i,l) | i :> l <- ["", "a", "ab", "", "abc"]]
-- [(,'a'),(a,'b'),(ab,'c')]
pattern (:>) :: () => Unbox a => String a -> a -> String a
pattern xs :> x <- (unsnoc -> Just (xs@Unboxed, x))
    where
        xs :> x = snoc xs x
{-# INLINE (:>) #-}

-- | Stringified `:<`, matching `head` and `tail`.
--
-- >>> [(h,t) | h :<: t <- ["", "a", "ab", "", "abc"]]
-- [(a,),(a,b),(a,bc)]
pattern (:<:) :: () => Unbox a => String a -> String a -> String a
pattern x :<: xs <- (splitAtHead -> Just (x@Unboxed, xs))
{-# INLINE (:<:) #-}

-- | Stringified `:>`, matching `init` and `last`.
--
-- >>> [(i,l) | i :>: l <- ["", "a", "ab", "", "abc"]]
-- [(,a),(a,b),(ab,c)]
pattern (:>:) :: () => Unbox a => String a -> String a -> String a
pattern xs :>: x <- (splitAtLast -> Just (xs, x@Unboxed))
{-# INLINE (:>:) #-}

-- -------------------------- --
-- Pattern matching functions --
-- -------------------------- --

-- | (INTERNAL) `Just` the empty string .
--
-- >>> nullish "text"
-- Nothing
-- >>> nullish "" == Just empty
-- True
nullish :: String a -> Maybe (String a)
nullish s@Unboxed
    | Generic.null s = Just s
    | otherwise = Nothing
{-# INLINE nullish #-}

-- | (INTERNAL) Yield the string, if not empty, `Nothing` otherwise.
--
-- >>> nonNull "text"
-- Just text
-- >>> nonNull ""
-- Nothing
nonNull :: String a -> Maybe (String a)
nonNull s@Unboxed
    | Generic.null s = Nothing
    | otherwise = Just s
{-# INLINE nonNull #-}

-- | (INTERNAL) Stringified version of `uncons`.
--
-- >>> splitAtHead "acgt"
-- Just (a,cgt)
splitAtHead :: String a -> Maybe (String a, String a)
splitAtHead s@Unboxed
    | n > 0 = Just (Generic.unsafeSlice 0 1 s, Generic.unsafeSlice 1 (n - 1) s)
    | otherwise = Nothing
  where
    n = Generic.length $! s
{-# INLINE splitAtHead #-}

-- | (INTERNAL) Stringified version of `unsnoc`.
--
-- >>> splitAtLast "acgt"
-- Just (acg,t)
splitAtLast :: String a -> Maybe (String a, String a)
splitAtLast s@Unboxed
    | n > 0 = Just (Generic.unsafeSlice 0 (n - 1) s, Generic.unsafeSlice (n - 1) 1 s)
    | otherwise = Nothing
  where
    n = Generic.length $! s
{-# INLINE splitAtLast #-}

-- | (INTERNAL) Extracts `head` and `id` for pattern matching.
--
-- >>> headId "texting"
-- (Just 't',texting)
headId :: String a -> (Maybe a, String a)
headId s = (headM s, s)
{-# INLINE headId #-}

-- | (INTERNAL) Extracts `tail` and `id` for pattern matching.
--
-- >>> lastId "texting"
-- (Just 'g',texting)
lastId :: String a -> (Maybe a, String a)
lastId s = (lastM s, s)
{-# INLINE lastId #-}

-- | (INTERNAL) Extracts `singleM` and `id` for pattern matching.
--
-- >>> singleId "texting"
-- (Nothing,texting)
singleId :: String a -> (Maybe a, String a)
singleId s = (single s, s)
{-# INLINE singleId #-}

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

instance a ~ Char => IsString (String a) where
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

instance Semigroup (String a) where
    {-# SPECIALIZE instance Semigroup (String Char) #-}
    {-# SPECIALIZE instance Semigroup (String Int) #-}
    {-# SPECIALIZE instance Semigroup (String Word8) #-}
    (<>) = (++)
    {-# INLINE (<>) #-}
    sconcat = concatNE
    {-# INLINE sconcat #-}

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
    show s@Unboxed = showStr s ""
    {-# INLINE show #-}

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
        calcSize s = sizeOf size (length s) + sizeSum s
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

-- ----------------------------- --
-- String information extraction --
-- ----------------------------- --

-- | /O(n lg n)/ Extracts the frequency count of each character in a string.
--
-- >>> frequency "aabacabd"
-- fromList [('a',4),('b',2),('c',1),('d',1)]
frequency :: Ord a => String a -> Map.Map a Int
frequency = foldr' (Map.alter $ Just . maybe 1 (+ 1)) Map.empty

-- | /O(n lg n)/ Extracts the set of singleton characters in a string.
--
-- >>> singletons "aabacabd"
-- fromList "cd"
singletons :: Ord a => String a -> Set.Set a
singletons = Map.foldrWithKey' insertSingleton Set.empty . frequency
  where
    insertSingleton k 1 = Set.insert k
    insertSingleton _ _ = id

-- | /O(n lg m)/ Check if at least one of the character of string is present in the given set.
--
-- >>> import Data.Set (fromList)
-- >>> hasOneOf "abca" (Data.Set.fromList "bdf")
-- True
-- >>> import Data.Set (fromList)
-- >>> hasOneOf "xxx" (Data.Set.fromList "bdf")
-- False
hasOneOf :: Ord a => String a -> Set.Set a -> Bool
hasOneOf str ls = any hasLetter str
  where
    hasLetter ch = Set.member ch ls

-- --------------------------------------- --
-- Operations with lifted Unbox constraint --
-- --------------------------------------- --

-- Indexing
-- --------

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
    | n > 0 = Generic.unsafeIndexM s (n - 1)
    | otherwise = Applicative.empty
  where
    n = Generic.length $! s
{-# INLINE lastM #-}

-- Substrings (slicing)
-- --------------------

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

-- | /O(1)/ Yield at the first `n` characters without copying.
--
-- The string may contain less than `n` characters, in which case it is returned unchanged.
--
-- >>> take 2 "hello"
-- he
-- >>> take 10 "hello"
-- hello
take :: Int -> String a -> String a
take n s@Unboxed = Generic.take n s
{-# INLINE take #-}

-- | /O(1)/ Yield all but the first `n` characters without copying.
--
-- The string may contain less than `n` characters, in which case an empty string is returned.
--
-- >>> drop 2 "hello"
-- llo
-- >>> drop 10 "hello"
-- <BLANKLINE>
drop :: Int -> String a -> String a
drop n s@Unboxed = Generic.drop n s
{-# INLINE drop #-}

-- | /O(1)/ Yield the first `n` characters paired with the remainder, without copying.
--
-- Note that `splitAt n v` is equivalent to `(take n v, drop n v)`, but slightly more efficient.
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

-- Concatenation
-- -------------

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

-- | /O(n)/ Concatenate all strings in the list, if non-empty.
--
-- Returns `Nothing` if the list is empty.
--
-- >>> maybeConcat ["abc", "123", "def"]
-- Just abc123def
-- >>> maybeConcat ([] :: [String Char])
-- Nothing
maybeConcat :: [String a] -> Maybe (String a)
maybeConcat strs = concatNE <$> nonEmpty strs
{-# INLINE maybeConcat #-}

-- Restricting memory usage
-- ------------------------

-- | /O(n)/ Yield the argument, but force it not to retain any extra memory, possibly by copying it.
--
-- See [Data.Vector.Unbox](https://hackage.haskell.org/package/vector-0.13.0.0/docs/Data-Vector-Unboxed.html#v:force).
force :: String a -> String a
force s@Unboxed = Generic.force s
{-# INLINE force #-}

-- Permutations
-- ------------

-- | /O(n)/ Reverse a string.
--
-- >>> reverse "abc123"
-- 321cba
reverse :: String a -> String a
reverse s@Unboxed = Generic.reverse s
{-# INLINE reverse #-}

-- | O(n) Yield the string obtained by replacing each element `i` of the index list by `xs!i`.
--
-- This is equivalent to `map (xs!)` is, but is often much more efficient.
--
-- >>> backpermute "abcd" [0,3,2,3,1,0]
-- adcdba
backpermute :: String a -> [Int] -> String a
backpermute s@Unboxed idx = Generic.backpermute s (Generic.fromList idx)
{-# INLINE backpermute #-}

-- Updates
-- -------

-- | Apply a destructive operation to a string.
--
-- The operation will be performed in place if it is safe to do so and will modify a copy of the vector otherwise.
modify :: (forall s. MVector s a -> ST s ()) -> String a -> String a
modify f s@Unboxed = Generic.modify (f . mContents) s
{-# INLINE modify #-}

-- Comparisons
-- -----------

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

-- Other vector types
-- ------------------

-- | /O(n)/ Convert to a vector of characters.
convert :: Generic.Vector v a => String a -> v a
convert s@Unboxed = Generic.convert s
{-# INLINE convert #-}

-- --------------------------- --
-- Other reexported operations --
-- --------------------------- --

-- | /O(n)/ Execute the monadic action the given number of times and store the results in a string.
--
-- >>> replicateM 4 (Just 'v')
-- Just vvvv
replicateM :: (Unbox a, Monad m) => Int -> m a -> m (String a)
replicateM = Generic.replicateM
{-# INLINE replicateM #-}

-- | /O(1)/ The empty string.
--
-- >>> empty == ""
-- True
empty :: Unbox a => String a
empty = Generic.empty
{-# INLINE empty #-}

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
