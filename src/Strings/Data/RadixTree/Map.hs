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
    lookupMin,
    lookupMax,
    member,

    -- * Modification
    insert,
    union,
    delete,
) where

import Control.Applicative (liftA2)
import Control.Monad ((<$!>))
import Data.Bool (Bool (False, True))
import Data.Eq (Eq ((==)))
import Data.Foldable (foldMap, foldMap', foldl, foldl', foldr, foldr', toList)
import Data.Foldable qualified as Foldable (Foldable (..))
import Data.Foldable1 (Foldable1 (..), foldl1, foldr1)
import Data.Function (flip, id, ($), (.))
import Data.Functor (Functor (fmap, (<$)), (<$>))
import Data.List (map)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (Maybe (Just, Nothing), isJust)
import Data.Monoid (mempty, (<>))
import Data.Ord (Ord, (>))
import Data.String qualified as Text (String)
import Data.Traversable (Traversable (..))
import Data.Tuple (snd, uncurry)
import GHC.Base (($!))
import GHC.Err (error, errorWithoutStackTrace)
import Text.Show (Show (showsPrec), showChar, showParen, showString, shows)

import Data.Map.Strict qualified as Map
import Data.Map.Strict.Internal (Map (Bin))

import Strings.Data.String (ShowString, String (..), Unbox, (++))
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

{-# COMPLETE Leaf, Empty, WithSomeKey #-}

-- | /O(1)/ Matches a tree node with value but no edges.
pattern Leaf :: v -> RadixTreeMap a v
pattern Leaf x = Tree (Just x) NullSet
{-# INLINE CONLIKE Leaf #-}

-- | /O(1)/ Matches an empty `RadixTreeMap`.
pattern Empty :: RadixTreeMap a v
pattern Empty = Tree Nothing NullSet
{-# INLINE CONLIKE Empty #-}

-- | /O(1)/ Matches any key inside a `RadixTreeMap`.
pattern WithSomeKey :: () => Unbox a => String a -> RadixTreeMap a v
pattern WithSomeKey s <- (edges -> Bin _ _ (s@Unboxed :~> _) _ _)
{-# INLINE CONLIKE WithSomeKey #-}

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

-- | /O(1)/ Matches an `EdgeSet` with a single edge.
pattern Single :: () => Unbox a => Edge a v -> EdgeSet a v
pattern Single e <- (id -> Bin 1 _ e@(Unboxed :~> _) _ _)
{-# INLINE CONLIKE Single #-}

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

-- ------------ --
-- Construction --

-- | /O(1)/ The empty map.
empty :: RadixTreeMap a v
empty = Empty

-- | /O(?)/ Build a map from a list of key/value pairs.
construct :: Ord a => [(String a, v)] -> RadixTreeMap a v
construct = foldr' (uncurry insert) empty

-- ----- --
-- Query --

-- | /O(n log r)/ Lookup the value at a key in the map.
lookup :: Ord a => String a -> RadixTreeMap a v -> Maybe v
lookup Null t = value t
lookup k@(Head h) t = do
    prefix :~> subt <- Map.lookup h (edges t)
    rest <- stripPrefix prefix k
    lookup rest subt

-- | /O(n log r)/ Extract the value associated with the minimal key in the map.
lookupMin :: RadixTreeMap a v -> Maybe v
lookupMin (Tree (Just !x) _) = Just x
lookupMin (Tree Nothing es) = do
    (_, _ :~> t) <- Map.lookupMin es
    lookupMin t

-- | /O(n log r)/ Extract the value associated with the maximal key in the map.
lookupMax :: RadixTreeMap a v -> Maybe v
lookupMax (Leaf !x) = Just x
lookupMax (Tree _ es) = do
    (_, _ :~> t) <- Map.lookupMax es
    lookupMax t

-- | /O(n log r)/ Check if there is an associated value for the key.
member :: Ord a => String a -> RadixTreeMap a v -> Bool
member k t = isJust (lookup k t)

-- --------- --
-- Insertion --

-- | /O(?)/ Merge two edges into a single edge by their common prefix.
--
-- There are basically three situations:
-- * both edge labels are distinct, but share a common prefix; then the edge is replaced by a new edge with both
-- subtrees as children.
-- * one label is a prefix of the other; then the other subtree is inserted into the prefix edge.
-- * both label are equal; then the edge is replaced by the union of the subtrees.
merge :: Ord a => Edge a v -> Edge a v -> Edge a v
merge (kx :~> tx@(Tree vx ex)) (ky :~> ty@(Tree vy ey)) =
    prefix :~> case (rkx, rky) of
        -- kx == prefix == ky
        (Null, Null) -> tx `union` ty
        -- prefix + rky == kx + rky == ky
        (Null, Head hy) -> Tree vx (Map.insertWith merge hy (rky :~> ty) ex)
        -- prefix + rkx == kx == ky + rkx
        (Head hx, Null) -> Tree vy (Map.insertWith merge hx (rkx :~> tx) ey)
        -- rkx != rky
        (Head hx, Head hy) -> Tree Nothing (Map.fromList [(hx, rkx :~> tx), (hy, rky :~> ty)])
  where
    (prefix, rkx, rky) = splitCommonPrefix kx ky

-- | /O(?)/ Insert a new key and value in the map.
--
--  If the key is already present in the map, the associated value is replaced with the supplied value.
insert :: Ord a => String a -> v -> RadixTreeMap a v -> RadixTreeMap a v
insert Null x (Tree _ es) = Tree (Just x) es
insert kx@(Head h) x (Tree val es) = Tree val (Map.insertWith merge h (kx :~> Leaf x) es)

-- | O(?)/ The expression (union t1 t2) takes the left-biased union of t1 and t2.
--
-- It prefers t1 when duplicate keys are encountered.
union :: Ord a => RadixTreeMap a v -> RadixTreeMap a v -> RadixTreeMap a v
union (Tree vx ex) (Tree _ ey) = Tree vx (Map.unionWith merge ex ey)

-- -------- --
-- Deletion --

-- | /O(?)/ Remove a key from a subtree.
--
-- Returns `Just` the updated subtree or `Nothing` if the edge should be removed.
remove :: Ord a => String a -> Edge a v -> Maybe (Edge a v)
remove k (label :~> t@(Tree val es)) = case stripPrefix label k of
    -- k == label, remove the current value
    Just Null -> nonEmptyEdge Nothing es
    -- k == label + rk, continue searching in rk
    Just rk@(Head h) -> nonEmptyEdge val (Map.update (remove rk) h es)
    -- k != label, key is not present in map
    Nothing -> Just (label :~> t)
  where
    -- remove edges to empty subtrees
    nonEmptyEdge Nothing NullSet = Nothing
    -- merge edges with a single child
    nonEmptyEdge Nothing (Single (ln :~> tn)) = Just (label ++ ln :~> tn)
    -- just reconstruct the edge otherwise
    nonEmptyEdge vn en = Just (label :~> Tree vn en)

-- | /O(?)/ Delete a key and its value from the map.
--
-- When the key is not a member of the map, the original map is returned.
delete :: Ord a => String a -> RadixTreeMap a v -> RadixTreeMap a v
delete Null (Tree _ es) = Tree Nothing es
delete k@(Head h) (Tree val es) = Tree val (Map.update (remove k) h es)

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

-- ------------------ --
-- Foldable instances --
-- ------------------ --

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
    fold (Leaf v) = v
    fold t@(WithSomeKey k) = fold1 (k :~> t)
    foldMap _ Empty = mempty
    foldMap f (Leaf v) = f v
    foldMap f t@(WithSomeKey k) = foldMap1 f (k :~> t)
    foldMap' _ Empty = mempty
    foldMap' f (Leaf !v) = f v
    foldMap' f t@(WithSomeKey !k) = foldMap1' f (k :~> t)
    foldr _ x Empty = x
    foldr f x (Leaf v) = f v x
    foldr f x t@(WithSomeKey k) = foldr f x (k :~> t)
    foldr' _ !x Empty = x
    foldr' f !x (Leaf !v) = f v x
    foldr' f !x t@(WithSomeKey !k) = foldr' f x (k :~> t)
    foldl _ x Empty = x
    foldl f x (Leaf v) = f x v
    foldl f x t@(WithSomeKey k) = foldl f x (k :~> t)
    foldl' _ x Empty = x
    foldl' f !x (Leaf !v) = f x v
    foldl' f !x t@(WithSomeKey !k) = foldl' f x (k :~> t)
    foldr1 _ Empty = error "foldr1.RadixTreeMap: empty tree"
    foldr1 _ (Leaf v) = v
    foldr1 f t@(WithSomeKey k) = foldr1 f (k :~> t)
    foldl1 _ Empty = error "foldl1.RadixTreeMap: empty tree"
    foldl1 _ (Leaf v) = v
    foldl1 f t@(WithSomeKey k) = foldl1 f (k :~> t)
    toList Empty = []
    toList (Leaf v) = [v]
    toList t@(WithSomeKey k) = toList (k :~> t)
    null Empty = True
    null _ = False
    length Empty = 0
    length (Leaf _) = 1
    length t@(WithSomeKey k) = Foldable.length (k :~> t)
    elem _ Empty = False
    elem x (Leaf v) = x == v
    elem x t@(WithSomeKey k) = Foldable.elem x (k :~> t)
    maximum Empty = error "maximum.RadixTreeMap: empty tree"
    maximum (Leaf v) = v
    maximum t@(WithSomeKey k) = maximum (k :~> t)
    minimum Empty = error "minimum.RadixTreeMap: empty tree"
    minimum (Leaf v) = v
    minimum t@(WithSomeKey k) = minimum (k :~> t)
    sum Empty = 0
    sum (Leaf v) = v
    sum t@(WithSomeKey k) = Foldable.sum (k :~> t)
    product Empty = 1
    product (Leaf v) = v
    product t@(WithSomeKey k) = Foldable.product (k :~> t)

-- | Implementation based on @`Foldable1` (`Edge` s)@
instance Foldable.Foldable (Edge s) where
    fold = fold1
    foldMap = foldMap1
    foldMap' = foldMap1'
    foldr f x (_ :~> Tree val es) = foldr f (Map.foldr (flip $ foldr f) x es) val
    foldr' f x (_ :~> Tree !val !es) = foldr' f (Map.foldr' (flip $! foldr' f) x es) val
    foldl f x (_ :~> Tree val es) = Map.foldl (foldl f) (foldl f x val) es
    foldl' f x (_ :~> Tree !val !es) = Map.foldl' (foldl' f) (foldl' f x val) es
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
    head (_ :~> t) = unwrap "Edge.head: unexpected empty subtree" (lookupMin t)
    last (_ :~> t) = unwrap "Edge.last: unexpected empty subtree" (lookupMax t)

-- --------------------- --
-- Traversable instances --
-- --------------------- --

instance Functor (RadixTreeMap s) where
    fmap f (Tree val es) = Tree (f <$> val) (fmap f <$> es)
    x <$ (Tree val es) = Tree (x <$ val) ((x <$) <$> es)

instance Functor (Edge s) where
    fmap f (label :~> t) = label :~> (f <$> t)
    x <$ (label :~> t) = label :~> (x <$ t)

instance Traversable (RadixTreeMap s) where
    traverse f (Tree val es) = liftA2 Tree (traverse f val) (traverse (traverse f) es)
    sequenceA (Tree val es) = liftA2 Tree (sequenceA val) (traverse sequenceA es)
    mapM f (Tree val es) = liftA2 Tree (mapM f val) (mapM (mapM f) es)
    sequence (Tree val es) = liftA2 Tree (sequence val) (mapM sequence es)

instance Traversable (Edge s) where
    traverse f (label :~> t) = (label :~>) <$> traverse f t
    sequenceA (label :~> t) = (label :~>) <$> sequenceA t
    mapM f (label :~> t) = (label :~>) <$> mapM f t
    sequence (label :~> t) = (label :~>) <$> sequence t
