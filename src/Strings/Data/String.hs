{-# LANGUAGE UndecidableInstances #-}

module Strings.Data.String (
    Character,
    chars,

    -- * Unboxed string
    String (..),

    -- * Accessors

    -- ** Indexing
    (!),
    (!?),
    head,
    last,
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

    -- * Modification

    -- ** Concatenation
    cons,
    snoc,
    (++),
    concatNE,
    concat,

    -- ** Restricting memory usage
    force,

    -- ** Permutations
    reverse,
    backpermute,

    -- ** Updates
    modify,

    -- * Other operations
    eqBy,
    cmpBy,
    convert,
) where

import Prelude hiding (String, concat, drop, head, init, last, readList, reverse, splitAt, tail, take, (++))

import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Data (Typeable)
import Data.Foldable (Foldable (..))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup (Semigroup (..), Sum (..))
import Data.Store (Size (..), Store (..))
import Data.String (IsString (..))
import GHC.Exts (IsList (..))
import GHC.Read (Read (readPrec))
import Text.ParserCombinators.ReadPrec (ReadPrec, get, (<++))

import Control.Monad.ST (ST)
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as M
import Data.Vector.Unboxed (Unbox)
import Data.Vector.Unboxed qualified as U

-- | Common constraints for a character.
type Character a = (Enum a, Bounded a, Unbox a)

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
    String :: Unbox a => !(U.Vector a) -> String a
    deriving newtype (Typeable)

-- | Extract the inner contents of a `String`.
contents :: String a -> U.Vector a
contents (String v) = v

instance Eq a => Eq (String a) where
    (String lhs) == (String rhs) = lhs == rhs
    (String lhs) /= (String rhs) = lhs /= rhs

instance Ord a => Ord (String a) where
    (String lhs) `compare` (String rhs) = lhs `compare` rhs
    (String lhs) < (String rhs) = lhs < rhs
    (String lhs) <= (String rhs) = lhs <= rhs
    (String lhs) > (String rhs) = lhs > rhs
    (String lhs) >= (String rhs) = lhs >= rhs
    max (String lhs) (String rhs) = String (max lhs rhs)
    min (String lhs) (String rhs) = String (min lhs rhs)

-- ----------------------------------- --
-- List-like and String-like instances --
-- ----------------------------------- --

instance Unbox a => IsList (String a) where
    type Item (String a) = a
    fromList = String . U.fromList
    fromListN n = String . U.fromListN n
    toList (String v) = U.toList v

instance a ~ Char => IsString (String a) where
    fromString = fromList

instance Foldable String where
    fold (String v) = U.foldMap id v
    foldMap f (String v) = U.foldMap f v
    foldMap' f (String v) = U.foldMap' f v
    foldr f x (String v) = U.foldr f x v
    foldr' f x (String v) = U.foldr' f x v
    foldl f x (String v) = U.foldl f x v
    foldl' f x (String v) = U.foldl' f x v
    foldr1 f (String v) = U.foldr1 f v
    foldl1 f (String v) = U.foldl1 f v
    toList (String v) = U.toList v
    null (String v) = U.null v
    length (String v) = U.length v
    elem x (String v) = U.elem x v
    maximum (String v) = U.maximum v
    minimum (String v) = U.minimum v
    sum (String v) = U.sum v
    product (String v) = U.product v

instance Semigroup (String a) where
    (<>) = (++)
    sconcat = concatNE

instance Unbox a => Monoid (String a) where
    mempty = String U.empty

-- ---------------------------------------------- --
-- Input and Output instances, Textual and Binary --
-- ---------------------------------------------- --

-- | Show value without extra decorators.
--
-- Useful for showing list of characters.
class ShowSimple a where
    -- | Similar to `showsPrec`.
    showSimple :: Int -> a -> ShowS

instance {-# OVERLAPPABLE #-} Show a => ShowSimple a where
    showSimple = showsPrec

instance ShowSimple Char where
    showSimple _ = showChar

instance ShowSimple a => Show (String a) where
    showsPrec d s t = foldr' (showSimple d) t s

-- | Read value without extra decorators.
--
-- Useful for reading list of characters.
class ReadSimple a where
    -- | Similar to `readPrec`.
    readSimple :: ReadPrec a

instance {-# OVERLAPPABLE #-} Read a => ReadSimple a where
    readSimple = readPrec

instance ReadSimple Char where
    readSimple = get

-- | Reads a list of unquoted and unseparated items.
--
-- Note that this function will never return an error. If no item is parseable, an empty list is returned.
readList :: ReadSimple a => ReadPrec [a]
readList = tryReadList <++ pure []
  where
    tryReadList = do
        value <- readSimple
        rest <- readList
        pure (value : rest)

instance (ReadSimple a, Unbox a) => Read (String a) where
    readPrec = fromList <$> readList

instance (Store a, Unbox a) => Store (String a) where
    size = VarSize calcSize
      where
        calcSize s = sizeOf size (length s) + sizeSum s
        sizeOf (ConstSize n) _ = n
        sizeOf (VarSize f) x = f x
        sizeSum v = getSum $ foldMap (Sum . sizeOf size) v
    poke (String v) = do
        poke $ U.length v
        U.forM_ v poke
    peek = do
        n <- peek
        v <- U.replicateM n peek
        pure $ String v

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
(String v) ! n = v U.! n

-- | /O(1)/ Safe indexing.
--
-- >>> "abc" !? 1
-- Just 'b'
-- >>> "abc" !? 3
-- Nothing
(!?) :: String a -> Int -> Maybe a
(String v) !? n = v U.!? n

-- | /O(1)/ First character.
--
-- >>> head "hello"
-- 'h'
head :: String a -> a
head (String v) = U.head v

-- | /O(1)/ Last character.
--
-- >>> last "hello"
-- 'o'
last :: String a -> a
last (String v) = U.last v

-- | /O(1)/ Indexing in a monad.
--
-- See [Data.Vactor.Unbox](https://hackage.haskell.org/package/vector-0.13.0.0/docs/Data-Vector-Unboxed.html#v:indexM).
indexM :: Monad m => String a -> Int -> m a
indexM (String v) = U.indexM v

-- | /O(1)/ First character of a string in a monad.
--
-- See [Data.Vactor.Unbox](https://hackage.haskell.org/package/vector-0.13.0.0/docs/Data-Vector-Unboxed.html#v:indexM).
headM :: Monad m => String a -> m a
headM (String v) = U.headM v

-- | /O(1)/ Last character of a string in a monad.
--
-- See [Data.Vactor.Unbox](https://hackage.haskell.org/package/vector-0.13.0.0/docs/Data-Vector-Unboxed.html#v:indexM).
lastM :: Monad m => String a -> m a
lastM (String v) = U.lastM v

-- Substrings (slicing)
-- --------------------

-- | /O(1)/ Yield the slice `s[i:i+n]` of the string without copying it.
--
-- The string must contain at least `i+n` characters.
--
-- >>> slice 2 3 "genome"
-- nom
slice :: Int -> Int -> String a -> String a
slice i n (String v) = String $ U.slice i n v

-- | /O(1)/ Yield all but the last character without copying.
--
-- The string may not be empty.
--
-- >>> init "genome"
-- genom
init :: String a -> String a
init (String v) = String $ U.init v

-- | /O(1)/ Yield all but the first character without copying.
--
-- The string may not be empty.
--
-- >>> tail "genome"
-- enome
tail :: String a -> String a
tail (String v) = String $ U.tail v

-- | /O(1)/ Yield at the first `n` characters without copying.
--
-- The string may contain less than `n` characters, in which case it is returned unchanged.
--
-- >>> take 2 "hello"
-- he
-- >>> take 10 "hello"
-- hello
take :: Int -> String a -> String a
take n (String v) = String $ U.take n v

-- | /O(1)/ Yield all but the first `n` characters without copying.
--
-- The string may contain less than `n` characters, in which case an empty string is returned.
--
-- >>> drop 2 "hello"
-- llo
-- >>> drop 10 "hello"
-- <BLANKLINE>
drop :: Int -> String a -> String a
drop n (String v) = String $ U.drop n v

-- | /O(1)/ Yield the first `n` characters paired with the remainder, without copying.
--
-- Note that `splitAt n v` is equivalent to `(take n v, drop n v)`, but slightly more efficient.
--
-- >>> splitAt 6 "babushka"
-- (babush,ka)
splitAt :: Int -> String a -> (String a, String a)
splitAt n (String v) = bimap String String $ U.splitAt n v

-- | /O(1)/ Yield the `head` and `tail` of the string, or `Nothing` if it is empty.
--
-- >>> uncons "acgt"
-- Just ('a',cgt)
uncons :: String a -> Maybe (a, String a)
uncons (String v) = second String <$> U.uncons v

-- | /O(1)/ Yield the `last` and `init` of the string, or `Nothing` if it is empty.
--
-- >>> unsnoc "acgt"
-- Just (acg,'t')
unsnoc :: String a -> Maybe (String a, a)
unsnoc (String v) = first String <$> U.unsnoc v

-- Concatenation
-- -------------

-- | /O(n)/ Prepend a character.
--
-- >>> cons 'e' "xtrem"
-- extrem
cons :: a -> String a -> String a
cons ch (String v) = String $ U.cons ch v

-- | /O(n)/ Append a character.
--
-- >>> snoc "xtrem" 'a'
-- xtrema
snoc :: String a -> a -> String a
snoc (String v) ch = String $ U.snoc v ch

-- | /O(m+n)/ Concatenate two strings.
--
-- >>> "abc" ++ "xyz"
-- abcxyz
(++) :: String a -> String a -> String a
(String v1) ++ (String v2) = String (v1 U.++ v2)

-- | /O(n)/ Concatenate all strings in the non-empty list.
concatNE :: NonEmpty (String a) -> String a
concatNE (String v0 :| rest) = String $ U.concat (v0 : map contents rest)

-- | /O(n)/ Concatenate all strings in the list, if non-empty.
--
-- Returns `Nothing` if the list is empty.
concat :: [String a] -> Maybe (String a)
concat [] = Nothing
concat (str : rest) = Just $ concatNE (str :| rest)

-- Restricting memory usage
-- ------------------------

-- | /O(n)/ Yield the argument, but force it not to retain any extra memory, possibly by copying it.
--
-- See [Data.Vector.Unbox](https://hackage.haskell.org/package/vector-0.13.0.0/docs/Data-Vector-Unboxed.html#v:force).
force :: String a -> String a
force (String v) = String $ U.force v

-- Permutations
-- ------------

-- | /O(n)/ Reverse a string.
--
-- >>> reverse "abc123"
-- 321cba
reverse :: String a -> String a
reverse (String v) = String $ U.reverse v

-- | O(n) Yield the string obtained by replacing each element `i` of the index list by `xs!i`.
--
-- This is equivalent to `map (xs!)` is, but is often much more efficient.
--
-- >>> backpermute "abcd" [0,3,2,3,1,0]
-- adcdba
backpermute :: String a -> [Int] -> String a
backpermute (String v0) idx = String $ U.backpermute v0 (U.fromList idx)

-- Updates
-- -------

-- | Apply a destructive operation to a string.
--
-- The operation will be performed in place if it is safe to do so and will modify a copy of the vector otherwise.
modify :: (forall s. U.MVector s a -> ST s ()) -> String a -> String a
modify f (String v) = String $ U.modify f v

-- Comparisons
-- -----------

-- | /O(n)/ Check if two strings are equal using the supplied equality predicate.
--
-- >>> import Data.Char (toLower)
-- >>> eqBy (\x y -> toLower x == toLower y) "ABcd" "abcD"
-- True
eqBy :: (a -> b -> Bool) -> String a -> String b -> Bool
eqBy f (String va) (String vb) = U.eqBy f va vb

-- | /O(n)/ Compare two strings using the supplied comparison function for characters.
--
-- Comparison works the same as for lists.
cmpBy :: (a -> b -> Ordering) -> String a -> String b -> Ordering
cmpBy f (String va) (String vb) = U.cmpBy f va vb

-- Other vector types
-- ------------------

-- | /O(n)/ Convert to a vector of characters.
convert :: G.Vector v a => String a -> v a
convert (String vs) = U.convert vs

-- | Split the `String` in substrings of 1 char each.
chars :: String a -> [String a]
chars str
    | null str = []
    | otherwise = ch : chars rest
  where
    (ch, rest) = splitAt 1 str

-- --------------------------------- --
-- Generic Vector instance and types --
-- --------------------------------- --

-- | Mutable variant of `String`, so that it can implement the `Generic Vector` interface.
newtype MString s a = MString {mContents :: U.MVector s a}

instance Unbox a => M.MVector MString a where
    basicLength = M.basicLength . mContents
    basicUnsafeSlice s n = MString . M.basicUnsafeSlice s n . mContents
    basicOverlaps lhs rhs = M.basicOverlaps (mContents lhs) (mContents rhs)
    basicUnsafeNew n = MString <$> M.basicUnsafeNew n
    basicInitialize = M.basicInitialize . mContents
    basicUnsafeReplicate n x = MString <$> M.basicUnsafeReplicate n x
    basicUnsafeRead = M.basicUnsafeRead . mContents
    basicUnsafeWrite = M.basicUnsafeWrite . mContents
    basicClear = M.basicClear . mContents
    basicSet = M.basicSet . mContents
    basicUnsafeCopy tgt src = M.basicUnsafeCopy (mContents tgt) (mContents src)
    basicUnsafeMove tgt src = M.basicUnsafeMove (mContents tgt) (mContents src)
    basicUnsafeGrow v n = MString <$> M.basicUnsafeGrow (mContents v) n

type instance G.Mutable String = MString

instance Unbox a => G.Vector String a where
    basicUnsafeFreeze (MString v) = String <$> G.basicUnsafeFreeze v
    basicUnsafeThaw (String v) = MString <$> G.basicUnsafeThaw v
    basicLength (String v) = G.basicLength v
    basicUnsafeSlice s n (String v) = String $ G.basicUnsafeSlice s n v
    basicUnsafeIndexM (String v) = G.basicUnsafeIndexM v
    basicUnsafeCopy (MString mv) (String v) = G.basicUnsafeCopy mv v
    elemseq _ = G.elemseq (undefined :: U.Vector a)
