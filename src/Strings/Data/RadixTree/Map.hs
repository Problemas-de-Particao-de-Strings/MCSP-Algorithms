{-# LANGUAGE NoImplicitPrelude #-}

-- | A map of `String` keys represented by a radix tree.
module Strings.Data.RadixTree.Map (
    -- * Data Types
    RadixTreeMap (..),
    Edge (..),
    EdgeSet,

    -- * Construction
    empty,
    construct,

    -- * Query
    lookup,
    member,

    -- * Modification
    insert,
) where

import Data.Bool (Bool (True), (&&))
import Data.Eq (Eq)
import Data.Foldable (Foldable (..))
import Data.Function (flip, ($), (.))
import Data.List (map)
import Data.Maybe (Maybe (Just, Nothing), isJust, isNothing)
import Data.Monoid ((<>))
import Data.Ord (Ord, (>))
import Data.Tuple (snd, uncurry)
import Text.Show (Show (showsPrec), showChar, showParen, showString, shows)

import Data.Map.Strict qualified as Map

import Strings.Data.String (ShowString, String (..))
import Strings.Data.String.Prefix (splitCommonPrefix, stripPrefix)

-- --------------- --
-- Data definition --
-- --------------- --

-- | A map of `String` keys represented by a [radix tree](https://en.wikipedia.org/wiki/Radix_tree).
--
-- The tree structure uses the maximal substring possible to label edges so that each internal node has at least two
-- children. Even though a substrings are used, the trie property is maintained, that is, a node has at most @r@
-- children, where @r@ is the number of possible values for @a@. Given this property, substring are stored using a
-- sorted `Map.Map` which may improve performance.
data RadixTreeMap a v
    = -- | A subtree or node in the map.
      --
      -- Each node may contain a value and is considered a terminating node in that case, or it may be only a radix
      -- for its children. Note that all leaves must be terminating, except for empty tree maps.
      Tree
      { value :: {-# UNPACK #-} !(Maybe v)
    -- ^ The value for a terminating node.
      , edges :: {-# UNPACK #-} !(EdgeSet a v)
    -- ^ The set of labelled edges to its children.
      }
    deriving stock (Eq, Ord)

-- | /O(1)/ Matches a tree node with value but no edges.
pattern Leaf :: v -> RadixTreeMap a v
pattern Leaf x = Tree (Just x) NullSet
{-# INLINE CONLIKE Leaf #-}

-- | /O(1)/ Matches an empty `RadixTreeMap`.
pattern Empty :: RadixTreeMap a v
pattern Empty = Tree Nothing NullSet
{-# INLINE CONLIKE Empty #-}

-- | A collection of uniquely labelled edges.
--
-- This could be @`Map.Map` (`String` a) (`Edge` a v)@, using the entire label as key, but the
-- [trie property](https://en.wikipedia.org/wiki/Trie) ensures that the first character of the label must also be
-- unique. This enables us to use a faster implementation and also helps to ensure the trie property.
type EdgeSet a v = Map.Map a (Edge a v)

-- | /O(1)/ Matches an empty `EdgeSet`.
pattern NullSet :: EdgeSet a v
pattern NullSet <- (Map.null -> True)
    where
        NullSet = Map.empty
{-# INLINE CONLIKE NullSet #-}

-- | A labelled edge.
--
-- A simple pair @(label `:~>` subtree)@ representing an edge of a `RadixTreeMap`.
data Edge a v = {-# UNPACK #-} !(String a) :~> {-# UNPACK #-} !(RadixTreeMap a v)
    deriving stock (Eq, Ord)

-- | /O(1)/ Extracts the subtree of an edge.
subtree :: Edge a v -> RadixTreeMap a v
subtree (_ :~> t) = t

-- -------------- --
-- Map operations --
-- -------------- --

-- | /O(1)/ The empty map.
empty :: RadixTreeMap a v
empty = Empty

-- | /O(?)/ Build a map from a list of key/value pairs.
construct :: Ord a => [(String a, v)] -> RadixTreeMap a v
construct = foldr' (uncurry insert) empty

-- | /O(log r)/ Find the edge where the given key may be found.
--
-- Returns `Just` the edge with the first character matching the string key, or `Nothing` if no such label exists.
edge :: Ord a => String a -> RadixTreeMap a v -> Maybe (Edge a v)
edge (Head !h) t = Map.lookup h (edges t)
edge Null _ = Nothing

-- | /O(n log r)/ Lookup the value at a key in the map.
lookup :: Ord a => String a -> RadixTreeMap a v -> Maybe v
lookup Null t = value t
lookup !k t = do
    prefix :~> subt <- edge k t
    rest <- stripPrefix prefix k
    lookup rest subt

-- | /O(n log r)/ Check if there is an associated value for the key.
member :: Ord a => String a -> RadixTreeMap a v -> Bool
member k t = isJust (lookup k t)

-- | /O(log r)/ Insert or replace the edge starting with the same character.
--
-- If the key is the empty string, replace the value inside the tree (as if replacing the entire subtree).
replace :: Ord a => Edge a v -> RadixTreeMap a v -> RadixTreeMap a v
replace e@(Head !h :~> _) (Tree val es) = Tree val $ Map.insert h e es
replace (Null :~> Tree newVal _) (Tree _ es) = Tree newVal es

-- | /O(1)/ Constructs a root node with the given edge as its single child.
node :: Edge a v -> RadixTreeMap a v
node (kx@(Head h) :~> t) = Tree Nothing (Map.singleton h (kx :~> t))
node (Null :~> t) = t

-- | /O(?)/ Insert a new key and value in the map.
--
--  If the key is already present in the map, the associated value is replaced with the supplied value.
insert :: Ord a => String a -> v -> RadixTreeMap a v -> RadixTreeMap a v
insert !kx !x t = case edge kx t of
    Just (oldK :~> subt) ->
        let (prefix, rok, rkx) = splitCommonPrefix oldK kx
            subt' = node (rok :~> subt)
         in replace (prefix :~> insert rkx x subt') t
    Nothing -> replace (kx :~> Leaf x) t

-- ---------------- --
-- Text conversions --
-- ---------------- --

instance (ShowString a, Show v) => Show (RadixTreeMap a v) where
    showsPrec d (Tree val e) = showParen (d > 10) $ case val of
        Just x -> showString "Tree" . showSpace . showVal x . showSpace . showEdges
        Nothing -> showString "Tree" . showSpace . showEdges
      where
        showSpace = showChar ' '
        showVal x = showParen True (shows x)
        showEdges = shows (map snd $ Map.toAscList e)

instance (ShowString a, Show v) => Show (Edge a v) where
    showsPrec _ (s :~> t) = shows s . showString " :~> " . shows t

-- -------------------- --
-- Collection instances --
-- -------------------- --

instance Foldable (RadixTreeMap s) where
    fold (Tree val es) = fold val <> foldMap fold es
    foldMap f (Tree val es) = foldMap f val <> foldMap (foldMap f) es
    foldMap' f (Tree val es) = foldMap' f val <> foldMap' (foldMap' f) es
    foldr f x (Tree val es) = foldr (flip $ foldr f) (foldr f x val) es
    foldr' f x (Tree val es) = foldr' (flip $ foldr' f) (foldr' f x val) es
    foldl f x (Tree val es) = foldl (foldl f) (foldl f x val) es
    foldl' f x (Tree val es) = foldl' (foldl' f) (foldl' f x val) es
    null (Tree val es) = isNothing val && Map.null es

-- | Delegate `Foldable` to its `subtree`.
instance Foldable (Edge s) where
    fold = fold . subtree
    foldMap f = foldMap f . subtree
    foldMap' f = foldMap' f . subtree
    foldr f x = foldr f x . subtree
    foldr' f x = foldr' f x . subtree
    foldl f x = foldl f x . subtree
    foldl' f x = foldl' f x . subtree
    foldr1 f = foldr1 f . subtree
    foldl1 f = foldl1 f . subtree
    toList = toList . subtree
    null = null . subtree
    length = length . subtree
    elem x = elem x . subtree
    maximum = maximum . subtree
    minimum = minimum . subtree
    sum = sum . subtree
