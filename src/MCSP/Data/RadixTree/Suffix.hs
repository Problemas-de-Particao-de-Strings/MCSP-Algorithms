-- | A compressed trie of string suffixes.
module MCSP.Data.RadixTree.Suffix (
    SuffixTree,
    construct,
    findMax,
) where

import Data.Eq (Eq (..))
import Data.Foldable (foldl')
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord (Ord (..))
import Data.Semigroup (Semigroup (..))
import Safe.Foldable (maximumMay)
import Text.Show (Show)

import MCSP.Data.RadixTree.Map qualified as Map
import MCSP.Data.String (String, length)
import MCSP.Data.String.Extra.Radix (stripSuffix, suffixes)

-- --------------- --
-- Data definition --
-- --------------- --

data LeafKind = First | Second | Both deriving stock (Eq, Ord, Show)

instance Semigroup LeafKind where
    Both <> _ = Both
    _ <> Both = Both
    First <> Second = Both
    Second <> First = Both
    x <> _ = x
    {-# INLINE (<>) #-}

-- | Represents a single suffix in a tree.
data Suffix a = Suffix {-# UNPACK #-} !LeafKind {-# UNPACK #-} !(String a)
    deriving stock (Show)

-- | /O(1)/ Key for comparing suffixes, using the string length.
cmpKey :: Suffix a -> (LeafKind, Int, String a)
cmpKey (Suffix l s) = (l, length s, s)
{-# INLINE cmpKey #-}

-- | Compare giving preference to longer strings.
instance Eq a => Eq (Suffix a) where
    lhs == rhs = cmpKey lhs == cmpKey rhs
    {-# INLINE (==) #-}
    lhs /= rhs = cmpKey lhs == cmpKey rhs
    {-# INLINE (/=) #-}

-- | Compare giving preference to longer strings.
instance Ord a => Ord (Suffix a) where
    compare lhs rhs = compare (cmpKey lhs) (cmpKey rhs)
    {-# INLINE compare #-}
    lhs < rhs = cmpKey lhs < cmpKey rhs
    {-# INLINE (<) #-}
    lhs <= rhs = cmpKey lhs <= cmpKey rhs
    {-# INLINE (<=) #-}
    lhs > rhs = cmpKey lhs > cmpKey rhs
    {-# INLINE (>=) #-}
    lhs >= rhs = cmpKey lhs >= cmpKey rhs
    {-# INLINE (>) #-}

-- | A set of suffixes for a pair of strings.
--
-- Represented by a [suffix tree](https://en.wikipedia.org/wiki/Generalized_suffix_tree).
type SuffixTree a = Map.RadixTreeMap a (Suffix a)

-- --------------- --
-- Tree operations --
-- --------------- --

-- | /O(1)/ Marks a new leaf for a suffix.
markLeaf :: Eq a => LeafKind -> String a -> String a -> Maybe (Suffix a) -> Maybe (Suffix a)
markLeaf m _ _ (Just (Suffix l s)) = Just (Suffix (m <> l) s)
markLeaf m k s Nothing = Suffix m <$> stripSuffix s k

-- | /O(1)/ Marks leaves when merging suffixes.
mergeSuffix :: Suffix a -> Suffix a -> Suffix a
mergeSuffix (Suffix xl xs) (Suffix yl _) = Suffix (xl <> yl) xs

-- | /O(?)/ Inserts all non-empty `suffixes` of a pair of strings.
insert :: Ord a => String a -> String a -> SuffixTree a -> SuffixTree a
insert s1 s2 = insertAllSuffixes Second s2 . insertAllSuffixes First s1
  where
    insertAllSuffixes l s t = foldl' (insertSuffix l) t (suffixes s)
    insertSuffix l t s = Map.insertWith mergeSuffix s (Suffix l s) t
{-# INLINEABLE insert #-}

-- | /O(?)/ Mark the leaves for all `suffixes` of a pair of strings.
mark :: Ord a => String a -> String a -> SuffixTree a -> SuffixTree a
mark s1 s2 = markAll Second s2 . markAll First s1
  where
    markAll l s t = foldl' (markPrefixes l) t (suffixes s)
    markPrefixes l t s = Map.updatePath (markLeaf l s) s t

-- | /O(?)/ Constructs suffix tree for a pair of strings.
--
-- >>> construct "aba" "ba"
-- Tree (Suffix Both ) [a :~> Tree (Suffix Both a) [ba :~> Tree (Suffix First aba) []],ba :~> Tree (Suffix Both ba) []]
construct :: Ord a => String a -> String a -> SuffixTree a
construct s1 s2 = mark s1 s2 (insert s1 s2 Map.empty)
{-# INLINE construct #-}

-- | /O(n log r)/ Retrieves the maximum common prefix of all suffixes.
--
-- >>> findMax (construct "abab" "baba")
-- Just bab
findMax :: Ord a => SuffixTree a -> Maybe (String a)
findMax t | Just (Suffix Both s) <- maximumMay t = Just s
findMax _ = Nothing
{-# INLINE findMax #-}
