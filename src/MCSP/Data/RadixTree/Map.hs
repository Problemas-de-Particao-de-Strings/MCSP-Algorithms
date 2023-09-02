-- | A map of `String` keys represented by a radix tree.
module MCSP.Data.RadixTree.Map (
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
    insertWith,
    union,
    unionWith,
    delete,
    updatePath,
) where

import Control.Applicative (liftA2)
import Control.Monad ((<$!>))
import Data.Bool (Bool (False, True))
import Data.Char (Char)
import Data.Eq (Eq ((==)))
import Data.Foldable (foldMap, foldMap', foldl, foldl', foldr, foldr', toList)
import Data.Foldable qualified as Foldable (Foldable (..))
import Data.Foldable1 (Foldable1 (..), foldl1, foldr1)
import Data.Function (const, flip, id, ($), (.))
import Data.Functor (Functor (fmap, (<$)), (<$>))
import Data.Int (Int)
import Data.List (map)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (Maybe (Just, Nothing), isJust, maybe)
import Data.Monoid (mempty, (<>))
import Data.Ord (Ord, (>))
import Data.String qualified as Text (String)
import Data.Traversable (Traversable (..))
import Data.Tuple (snd, uncurry)
import Data.Word (Word8)
import GHC.Base (($!))
import GHC.Err (error, errorWithoutStackTrace)
import Text.Show (Show (showsPrec), showChar, showParen, showString, shows)

import Data.Map.Strict qualified as Map
import Data.Map.Strict.Internal (Map (Bin))

import MCSP.Data.String (ShowString, String (..), Unbox, (++))
import MCSP.Data.String.Extra.Radix (splitCommonPrefix, stripPrefix)

-- --------------- --
-- Data definition --
-- --------------- --

-- | A map of `String` keys represented by a [radix tree](https://en.wikipedia.org/wiki/Radix_tree).
--
-- The tree structure uses the maximal substring possible to label edges so that each internal node
-- has at least two children. Even though a substrings are used, the trie property is maintained,
-- that is, a node has at most @r@ children, where @r@ is the number of possible values for @a@.
-- Given this property, substring are stored using a sorted `Map.Map` which may improve performance.
data RadixTreeMap a v
    = -- | A subtree or node in the map.
      --
      -- Each node may contain a value and is considered a terminating node in that case, or it may
      -- be only a radix for its children. Note that all leaves must be terminating, except for
      -- empty tree maps.
      Tree
      { -- | The value for a terminating node.
        value :: {-# UNPACK #-} !(Maybe v),
        -- | The set of labelled edges to its children.
        edges :: {-# UNPACK #-} !(EdgeSet a v)
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
-- [trie property](https://en.wikipedia.org/wiki/Trie) ensures that the first character of the
-- label must also be unique. This enables us to use a faster implementation and also helps to
-- ensure the trie property.
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
--
-- >>> import Prelude (Char, Int)
-- >>> empty :: RadixTreeMap Char Int
-- Tree []
empty :: RadixTreeMap a v
empty = Empty
{-# INLINE empty #-}

-- | /O(?)/ Build a map from a list of key/value pairs.
--
-- >>> import Prelude (Char, Int)
-- >>> construct [("abc", 1), ("def", 3), ("abb", 5)] :: RadixTreeMap Char Int
-- Tree [ab :~> Tree [b :~> Tree (5) [],c :~> Tree (1) []],def :~> Tree (3) []]
construct :: Ord a => [(String a, v)] -> RadixTreeMap a v
construct = foldl' (flip $ uncurry insert) empty
{-# INLINEABLE construct #-}

-- ----- --
-- Query --

-- | /O(n log r)/ Lookup the value at a key in the map.
--
-- >>> import Prelude (Int)
-- >>> lookup "abc" (construct [("abc", 1), ("def", 3), ("abb", 5)]) :: Maybe Int
-- Just 1
-- >>> lookup "xyz" (construct [("abc", 1), ("def", 3), ("abb", 5)]) :: Maybe Int
-- Nothing
--
-- >>> lookupMax (construct [("abc", 1), ("def", 3), ("abb", 5)]) :: Maybe Int
-- Just 3
lookup :: Ord a => String a -> RadixTreeMap a v -> Maybe v
lookup Null t = value t
lookup k@(Head h) t = do
    prefix :~> subt <- Map.lookup h (edges t)
    rest <- stripPrefix prefix k
    lookup rest subt
{-# INLINEABLE lookup #-}

-- | /O(n log r)/ Extract the value associated with the minimal key in the map.
--
-- >>> lookupMin (construct [("abc", 1), ("def", 3), ("abb", 5)]) :: Maybe Int
-- Just 5
-- >>> lookupMin empty :: Maybe Int
-- Nothing
lookupMin :: RadixTreeMap a v -> Maybe v
lookupMin (Tree (Just !x) _) = Just x
lookupMin (Tree Nothing es) = do
    (_, _ :~> t) <- Map.lookupMin es
    lookupMin t
{-# INLINEABLE lookupMin #-}

-- | /O(n log r)/ Extract the value associated with the maximal key in the map.
--
-- >>> lookupMax (construct [("abc", 1), ("def", 3), ("abb", 5)]) :: Maybe Int
-- Just 3
-- >>> lookupMax empty :: Maybe Int
-- Nothing
lookupMax :: RadixTreeMap a v -> Maybe v
lookupMax (Leaf !x) = Just x
lookupMax (Tree _ es) = do
    (_, _ :~> t) <- Map.lookupMax es
    lookupMax t
{-# INLINEABLE lookupMax #-}

-- | /O(n log r)/ Check if there is an associated value for the key.
--
-- >>> member "abc" (construct [("abc", 1), ("def", 3), ("abb", 5)]) :: Bool
-- True
-- >>> member "xyz" (construct [("abc", 1), ("def", 3), ("abb", 5)]) :: Bool
-- False
member :: Ord a => String a -> RadixTreeMap a v -> Bool
member k t = isJust (lookup k t)
{-# INLINE member #-}

-- --------- --
-- Insertion --

-- | /O(?)/ Merge two edges into a single edge by their common prefix.
--
-- There are basically three situations:
-- * both edge labels are distinct, but share a common prefix; then the edge is replaced by a
-- new edge with both subtrees as children.
-- * one label is a prefix of the other; then the other subtree is inserted into the prefix edge.
-- * both label are equal; then the edge is replaced by the union of the subtrees.
mergeWith :: Ord a => (v -> v -> v) -> Edge a v -> Edge a v -> Edge a v
mergeWith f (kx :~> tx@(Tree vx ex)) (ky :~> ty@(Tree vy ey)) =
    prefix :~> case (rkx, rky) of
        -- kx == prefix == ky
        (Null, Null) -> unionWith f tx ty
        -- prefix + rky == kx + rky == ky
        (Null, Head hy) -> Tree vx (Map.insertWith (mergeWith f) hy (rky :~> ty) ex)
        -- prefix + rkx == kx == ky + rkx
        (Head hx, Null) -> Tree vy (Map.insertWith (mergeWith f) hx (rkx :~> tx) ey)
        -- rkx != rky
        (Head hx, Head hy) -> Tree Nothing (Map.fromList [(hx, rkx :~> tx), (hy, rky :~> ty)])
  where
    (prefix, rkx, rky) = splitCommonPrefix kx ky

-- | Insert with a function, combining new value and old value.
--
-- @`insertWith` f key value tree@ will insert the pair @(key, value)@ into @tree@ if key does
-- not exist in the map. If the key does exist, the function will insert the pair
-- @(key, f new_value old_value)@.
insertWith :: Ord a => (v -> v -> v) -> String a -> v -> RadixTreeMap a v -> RadixTreeMap a v
insertWith f Null x (Tree val es) = Tree (Just (maybe x (f x) val)) es
insertWith f kx@(Head h) x (Tree val es) = Tree val (Map.insertWith (mergeWith f) h (kx :~> Leaf x) es)
{-# INLINE insertWith #-}

-- | /O(?)/ Insert a new key and value in the map.
--
--  If the key is already present in the map, the associated value is replaced with the supplied
-- value.
insert :: Ord a => String a -> v -> RadixTreeMap a v -> RadixTreeMap a v
insert = insertWith const
{-# INLINE insert #-}

-- | /O(?)/  Union with a combining function.
unionWith :: Ord a => (v -> v -> v) -> RadixTreeMap a v -> RadixTreeMap a v -> RadixTreeMap a v
unionWith f (Tree vx ex) (Tree vy ey) = Tree (liftA2 f vx vy) (Map.unionWith (mergeWith f) ex ey)
{-# INLINE unionWith #-}

-- | /O(?)/ The expression @union t1 t2@ takes the left-biased union of @t1@ and @t2@.
--
-- It prefers @t1@ when duplicate keys are encountered.
union :: Ord a => RadixTreeMap a v -> RadixTreeMap a v -> RadixTreeMap a v
union = unionWith const
{-# INLINE union #-}

-- | /O(n log r)/ Update values for all prefixes of key present in the map.
updatePath :: Ord a => (v -> v) -> String a -> RadixTreeMap a v -> RadixTreeMap a v
updatePath f Null (Tree val es) = Tree (f <$> val) es
updatePath f k@(Head h) (Tree val es) = Tree (f <$> val) (Map.adjust updateEdgeOnPath h es)
  where
    updateEdgeOnPath (kx :~> tx) = kx :~> maybe id (updatePath f) (stripPrefix kx k) tx
{-# INLINEABLE updatePath #-}

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
{-# INLINE delete #-}

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
-- This is an adaptation of `Data.Maybe.fromJust` without capturing the call stack for traces. It
-- should only be used when the error should never happen, like an `Edge` pointing to a an empty
-- subtree.
unwrap :: Text.String -> Maybe a -> a
unwrap _ (Just x) = x
unwrap message Nothing = errorWithoutStackTrace message
{-# INLINEABLE unwrap #-}

-- | Strict version of @unwrap@.
unwrap' :: Text.String -> Maybe a -> a
unwrap' !_ (Just !x) = x
unwrap' !message Nothing = errorWithoutStackTrace message
{-# INLINEABLE unwrap' #-}

-- | Strict version of `fmap` for `Maybe`.
fmap' :: (a -> b) -> Maybe a -> Maybe b
fmap' = (<$!>)
{-# INLINE fmap' #-}

-- | Delegate `Foldable.Foldabe` to its `Edge`s.
instance Foldable.Foldable (RadixTreeMap s) where
    {-# SPECIALIZE instance Foldable.Foldable (RadixTreeMap Char) #-}
    {-# SPECIALIZE instance Foldable.Foldable (RadixTreeMap Int) #-}
    {-# SPECIALIZE instance Foldable.Foldable (RadixTreeMap Word8) #-}
    fold Empty = mempty
    fold (Leaf v) = v
    fold t@(WithSomeKey k) = fold1 (k :~> t)
    {-# INLINEABLE fold #-}
    foldMap _ Empty = mempty
    foldMap f (Leaf v) = f v
    foldMap f t@(WithSomeKey k) = foldMap1 f (k :~> t)
    {-# INLINEABLE foldMap #-}
    foldMap' _ Empty = mempty
    foldMap' f (Leaf !v) = f v
    foldMap' f t@(WithSomeKey !k) = foldMap1' f (k :~> t)
    {-# INLINEABLE foldMap' #-}
    foldr _ x Empty = x
    foldr f x (Leaf v) = f v x
    foldr f x t@(WithSomeKey k) = foldr f x (k :~> t)
    {-# INLINEABLE foldr #-}
    foldr' _ !x Empty = x
    foldr' f !x (Leaf !v) = f v x
    foldr' f !x t@(WithSomeKey !k) = foldr' f x (k :~> t)
    {-# INLINEABLE foldr' #-}
    foldl _ x Empty = x
    foldl f x (Leaf v) = f x v
    foldl f x t@(WithSomeKey k) = foldl f x (k :~> t)
    {-# INLINEABLE foldl #-}
    foldl' _ x Empty = x
    foldl' f !x (Leaf !v) = f x v
    foldl' f !x t@(WithSomeKey !k) = foldl' f x (k :~> t)
    {-# INLINEABLE foldl' #-}
    foldr1 _ Empty = error "foldr1.RadixTreeMap: empty tree"
    foldr1 _ (Leaf v) = v
    foldr1 f t@(WithSomeKey k) = foldr1 f (k :~> t)
    {-# INLINEABLE foldr1 #-}
    foldl1 _ Empty = error "foldl1.RadixTreeMap: empty tree"
    foldl1 _ (Leaf v) = v
    foldl1 f t@(WithSomeKey k) = foldl1 f (k :~> t)
    {-# INLINEABLE foldl1 #-}
    toList Empty = []
    toList (Leaf v) = [v]
    toList t@(WithSomeKey k) = toList (k :~> t)
    {-# INLINEABLE toList #-}
    null Empty = True
    null _ = False
    {-# INLINE null #-}
    length Empty = 0
    length (Leaf _) = 1
    length t@(WithSomeKey k) = Foldable.length (k :~> t)
    {-# INLINEABLE length #-}
    elem _ Empty = False
    elem x (Leaf v) = x == v
    elem x t@(WithSomeKey k) = Foldable.elem x (k :~> t)
    {-# INLINEABLE elem #-}
    maximum Empty = error "maximum.RadixTreeMap: empty tree"
    maximum (Leaf v) = v
    maximum t@(WithSomeKey k) = maximum (k :~> t)
    {-# INLINEABLE maximum #-}
    minimum Empty = error "minimum.RadixTreeMap: empty tree"
    minimum (Leaf v) = v
    minimum t@(WithSomeKey k) = minimum (k :~> t)
    {-# INLINEABLE minimum #-}
    sum Empty = 0
    sum (Leaf v) = v
    sum t@(WithSomeKey k) = Foldable.sum (k :~> t)
    {-# INLINEABLE sum #-}
    product Empty = 1
    product (Leaf v) = v
    product t@(WithSomeKey k) = Foldable.product (k :~> t)
    {-# INLINEABLE product #-}

-- | Implementation based on @`Foldable1` (`Edge` s)@
instance Foldable.Foldable (Edge s) where
    {-# SPECIALIZE instance Foldable.Foldable (Edge Char) #-}
    {-# SPECIALIZE instance Foldable.Foldable (Edge Int) #-}
    {-# SPECIALIZE instance Foldable.Foldable (Edge Word8) #-}
    fold = fold1
    {-# INLINE fold #-}
    foldMap = foldMap1
    {-# INLINE foldMap #-}
    foldMap' = foldMap1'
    {-# INLINE foldMap' #-}
    foldr f x (_ :~> Tree val es) = foldr f (Map.foldr (flip $ foldr f) x es) val
    {-# INLINEABLE foldr #-}
    foldr' f x (_ :~> Tree !val !es) = foldr' f (Map.foldr' (flip $! foldr' f) x es) val
    {-# INLINEABLE foldr' #-}
    foldl f x (_ :~> Tree val es) = Map.foldl (foldl f) (foldl f x val) es
    {-# INLINEABLE foldl #-}
    foldl' f x (_ :~> Tree !val !es) = Map.foldl' (foldl' f) (foldl' f x val) es
    {-# INLINEABLE foldl' #-}
    foldr1 = foldr1
    {-# INLINE foldr1 #-}
    foldl1 = foldl1
    {-# INLINE foldl1 #-}
    toList (_ :~> Tree (Just x) es) = x : foldMap toList es
    toList (_ :~> Tree Nothing es) = foldMap toList es
    {-# INLINEABLE toList #-}
    null _ = False
    {-# INLINE null #-}
    maximum = maximum
    {-# INLINE maximum #-}
    minimum = minimum
    {-# INLINE minimum #-}

instance Foldable1 (Edge s) where
    {-# SPECIALIZE instance Foldable1 (Edge Char) #-}
    {-# SPECIALIZE instance Foldable1 (Edge Int) #-}
    {-# SPECIALIZE instance Foldable1 (Edge Word8) #-}
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
    {-# INLINE toNonEmpty #-}
    head (_ :~> t) = unwrap "Edge.head: unexpected empty subtree" (lookupMin t)
    {-# INLINE head #-}
    last (_ :~> t) = unwrap "Edge.last: unexpected empty subtree" (lookupMax t)
    {-# INLINE last #-}

-- --------------------- --
-- Traversable instances --
-- --------------------- --

instance Functor (RadixTreeMap s) where
    {-# SPECIALIZE instance Functor (RadixTreeMap Char) #-}
    {-# SPECIALIZE instance Functor (RadixTreeMap Int) #-}
    {-# SPECIALIZE instance Functor (RadixTreeMap Word8) #-}
    fmap f (Tree val es) = Tree (f <$> val) (fmap f <$> es)
    {-# INLINEABLE fmap #-}
    x <$ (Tree val es) = Tree (x <$ val) ((x <$) <$> es)
    {-# INLINEABLE (<$) #-}

instance Functor (Edge s) where
    {-# SPECIALIZE instance Functor (Edge Char) #-}
    {-# SPECIALIZE instance Functor (Edge Int) #-}
    {-# SPECIALIZE instance Functor (Edge Word8) #-}
    fmap f (label :~> t) = label :~> (f <$> t)
    {-# INLINE fmap #-}
    x <$ (label :~> t) = label :~> (x <$ t)
    {-# INLINE (<$) #-}

instance Traversable (RadixTreeMap s) where
    {-# SPECIALIZE instance Traversable (RadixTreeMap Char) #-}
    {-# SPECIALIZE instance Traversable (RadixTreeMap Int) #-}
    {-# SPECIALIZE instance Traversable (RadixTreeMap Word8) #-}
    traverse f (Tree val es) = liftA2 Tree (traverse f val) (traverse (traverse f) es)
    {-# INLINEABLE traverse #-}
    sequenceA (Tree val es) = liftA2 Tree (sequenceA val) (traverse sequenceA es)
    {-# INLINEABLE sequenceA #-}
    mapM f (Tree val es) = liftA2 Tree (mapM f val) (mapM (mapM f) es)
    {-# INLINEABLE mapM #-}
    sequence (Tree val es) = liftA2 Tree (sequence val) (mapM sequence es)
    {-# INLINEABLE sequence #-}

instance Traversable (Edge s) where
    {-# SPECIALIZE instance Traversable (Edge Char) #-}
    {-# SPECIALIZE instance Traversable (Edge Int) #-}
    {-# SPECIALIZE instance Traversable (Edge Word8) #-}
    traverse f (label :~> t) = (label :~>) <$> traverse f t
    {-# INLINE traverse #-}
    sequenceA (label :~> t) = (label :~>) <$> sequenceA t
    {-# INLINE sequenceA #-}
    mapM f (label :~> t) = (label :~>) <$> mapM f t
    {-# INLINE mapM #-}
    sequence (label :~> t) = (label :~>) <$> sequence t
    {-# INLINE sequence #-}
