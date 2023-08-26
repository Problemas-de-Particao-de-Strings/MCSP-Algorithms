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

import Control.Monad (fmap, (<$!>))
import Data.Bool (Bool (False, True))
import Data.Eq (Eq)
import Data.Foldable (foldMap, foldMap', foldl, foldl', foldr, foldr', toList)
import Data.Foldable qualified as Foldable (Foldable (..))
import Data.Foldable1 (Foldable1 (..), foldl1, foldr1)
import Data.Function (flip, ($), (.))
import Data.List (map)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (Maybe (Just, Nothing), isJust)
import Data.Monoid (mempty, (<>))
import Data.Ord (Ord, (>))
import Data.String qualified as Text (String)
import Data.Tuple (snd, uncurry)
import GHC.Base (($!))
import GHC.Err (error, errorWithoutStackTrace, undefined)
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
--
-- Note that the subtre pointed by an edge must never be empty!
data Edge a v = {-# UNPACK #-} !(String a) :~> {-# UNPACK #-} !(RadixTreeMap a v)
    deriving stock (Eq, Ord)

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

-- | Extracts the element out of a `Just` and throws an error if its argument is `Nothing`.
--
-- This is an adaptation of `Data.Maybe.fromJust` without capturing the call stack for traces. It should only be used
-- when the error should never happen, like an `Edge` pointing to a an empty subtree.
unwrap :: Text.String -> Maybe a -> a
unwrap _ (Just x) = x
unwrap message Nothing = errorWithoutStackTrace message

-- | Strict version of `unwrap`.
unwrap' :: Text.String -> Maybe a -> a
unwrap' !_ (Just !x) = x
unwrap' !message Nothing = errorWithoutStackTrace message

-- | Strict version of `fmap` for `Maybe`.
fmap' :: (a -> b) -> Maybe a -> Maybe b
fmap' = (<$!>)

-- | Delegate `Foldable` to its `Edge`s.
instance Foldable.Foldable (RadixTreeMap s) where
    fold Empty = mempty
    fold t = fold1 (undefined :~> t)
    foldMap _ Empty = mempty
    foldMap f t = foldMap1 f (undefined :~> t)
    foldMap' _ Empty = mempty
    foldMap' f t = foldMap1' f (undefined :~> t)
    foldr _ x Empty = x
    foldr f x t = foldr f x (undefined :~> t)
    foldr' _ x Empty = x
    foldr' f x t = foldr' f x (undefined :~> t)
    foldl _ x Empty = x
    foldl f x t = foldl f x (undefined :~> t)
    foldl' _ x Empty = x
    foldl' f x t = foldl' f x (undefined :~> t)
    foldr1 _ Empty = error "foldr1.RadixTreeMap: empty tree"
    foldr1 f t = foldr1 f (undefined :~> t)
    foldl1 _ Empty = error "foldl1.RadixTreeMap: empty tree"
    foldl1 f t = foldl1 f (undefined :~> t)
    toList Empty = []
    toList t = toList (undefined :~> t)
    null Empty = True
    null _ = False
    length Empty = 0
    length t = Foldable.length (undefined :~> t)
    elem _ Empty = False
    elem x t = Foldable.elem x (undefined :~> t)
    maximum Empty = error "maximum.RadixTreeMap: empty tree"
    maximum t = maximum (undefined :~> t)
    minimum Empty = error "minimum.RadixTreeMap: empty tree"
    minimum t = minimum (undefined :~> t)
    sum Empty = 0
    sum t = Foldable.sum (undefined :~> t)
    product Empty = 1
    product t = Foldable.product (undefined :~> t)

-- | Implementation based on @`Foldable1` (`Edge` s)@
instance Foldable.Foldable (Edge s) where
    fold = fold1
    foldMap = foldMap1
    foldMap' = foldMap1'
    foldr f x (_ :~> Tree val es) = Map.foldr (flip $ foldr f) (foldr f x val) es
    foldr' f x (_ :~> Tree !val !es) = Map.foldr' (flip $! foldr' f) (foldr' f x val) es
    foldl f x (_ :~> Tree val es) = foldl f (Map.foldl (foldl f) x es) val
    foldl' f x (_ :~> Tree !val !es) = foldl' f (Map.foldl' (foldl' f) x es) val
    foldr1 = foldr1
    foldl1 = foldl1
    toList (_ :~> Tree (Just x) es) = x : foldMap toList es
    toList (_ :~> Tree Nothing es) = foldMap toList es
    null _ = False
    maximum = maximum
    minimum = minimum

instance Foldable1 (Edge s) where
    fold1 = unwrap "Edge.fold1: unexpected empty subtree" . go
      where
        go (_ :~> Tree val es) = val <> foldMap go es
    foldMap1 f = unwrap "Edge.foldMap1: unexpected empty subtree" . go f
      where
        go fs (_ :~> Tree val es) = fmap fs val <> foldMap (go fs) es
    foldMap1' f = unwrap' "Edge.foldMap1': unexpected empty subtree" . go f
      where
        go fs (_ :~> Tree !val !es) = fmap' fs val <> foldMap' (go fs) es
    toNonEmpty = unwrap "Edge.toNonEmpty: unexpected empty subtree" . nonEmpty . toList
    head (_ :~> Tree (Just !x) _) = x
    head (_ :~> Tree Nothing es) = head $ snd $ Map.findMin es
    last (_ :~> Leaf !x) = x
    last (_ :~> Tree _ es) = last $ snd $ Map.findMax es
