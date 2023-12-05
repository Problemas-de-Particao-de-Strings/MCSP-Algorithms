-- | Additional variables and parameters.
module MCSP.Data.Meta (
    MetaVariable,

    -- * Collection of variables
    VariableMap,
    empty,
    (<:),
    lookup,

    -- * Monadic operations
    Meta,
    setVar,
    (<::),
    getVar,
    getVarOrDefault,

    -- ** Execution
    evalMetaWith,
    runMetaWith,
    evalMeta,
    runMeta,
) where

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..))
import Control.Monad.Trans.State.Strict (State, evalState, gets, modify, runState, state)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Function (($), (.))
import Data.Functor (Functor (..))
import Data.Map.Strict qualified as Map (Map, empty, insert, keys, lookup)
import Data.Maybe (Maybe (..))
import Data.Proxy (Proxy (..))
import Data.Typeable (TypeRep, Typeable, typeOf, typeRep)
import Text.Show (Show (..), showChar, showString)

-- | Represents a additional variable that can be used as input or output of a computation.
--
-- These variables are set and resolved dynamically inside the `Meta` monad.
class Typeable v => MetaVariable v

-- | A polymorphic storage of `MetaVariable`s.
newtype VariableMap = VariableMap (Map.Map TypeRep Dynamic)

instance Show VariableMap where
    showsPrec d (VariableMap vars) =
        showString "VariableMap"
            . showChar ' '
            . showsPrec d (Map.keys vars)

-- | A map with no variables set.
--
-- >>> empty
-- VariableMap []
empty :: VariableMap
empty = VariableMap Map.empty
{-# INLINEABLE empty #-}

-- | Insert a meta-variable into the map.
--
-- >>> import Prelude (Int)
-- >>> instance MetaVariable Int
--
-- >>> insert @Int 12 empty
-- VariableMap [Int]
insert :: MetaVariable v => v -> VariableMap -> VariableMap
insert val (VariableMap vars) =
    VariableMap $ Map.insert (typeOf val) (toDyn val) vars
{-# INLINEABLE insert #-}

-- | Insert a meta-variable into the map.
--
-- Infix version of `insert`.
--
-- >>> import Prelude (Int, Double, String)
-- >>> instance MetaVariable Int
-- >>> instance MetaVariable Double
-- >>> instance MetaVariable String
--
-- >>> empty <: (12 :: Int) <: (3.14 :: Double) <: "Hello"
-- VariableMap [Double,Int,[Char]]
(<:) :: MetaVariable v => VariableMap -> v -> VariableMap
vars <: val = insert val vars
{-# INLINEABLE (<:) #-}

-- | Return `Just` the value a `MetaVariable` of the given type, or `Nothing` if no such variable is
-- available.
--
-- >>> import Prelude (Int)
-- >>> instance MetaVariable Int
--
-- >>> lookup @Int empty
-- Nothing
--
-- >>> lookup @Int (empty <: (12 :: Int))
-- Just 12
lookup :: forall v. MetaVariable v => VariableMap -> Maybe v
lookup (VariableMap vars) = Map.lookup (typeRep (Proxy @v)) vars >>= fromDynamic
{-# INLINEABLE lookup #-}

-- | Return the previous value of a `MetaVariable` of the given type, or insert and return the
-- default values if no such variable is available.
--
-- >>> import Prelude (Int)
-- >>> instance MetaVariable Int
--
-- >>> lookupOrInsert (12 :: Int) empty
-- (12,VariableMap [Int])
--
-- >>> lookupOrInsert (12 :: Int) $ empty <: (34 :: Int)
-- (34,VariableMap [Int])
lookupOrInsert :: MetaVariable v => v -> VariableMap -> (v, VariableMap)
lookupOrInsert defaultValue vars = case lookup vars of
    Just value -> (value, vars)
    Nothing -> (defaultValue, insert defaultValue vars)
{-# INLINEABLE lookupOrInsert #-}

-- | A monad that represents operation with `MetaVariable`s.
--
-- The meta-variables may be used as input, output or both.
newtype Meta a = Meta (State VariableMap a)
    deriving newtype (Functor, Applicative, Monad)

-- | Set meta-variable for the given type.
--
-- >>> import Prelude (Int)
-- >>> instance MetaVariable Int
--
-- >>> runMeta (setVar (12 :: Int))
-- ((),VariableMap [Int])
setVar :: MetaVariable v => v -> Meta ()
setVar value = Meta $ modify (insert value)
{-# INLINEABLE setVar #-}

-- | Set a meta-variable for the given type.
--
-- Infix version of `setVar`.
--
-- >>> import Prelude (Int)
-- >>> instance MetaVariable Int
--
-- >>> runMeta (getVar @Int <:: (12 :: Int))
-- (Just 12,VariableMap [Int])
(<::) :: MetaVariable v => Meta a -> v -> Meta a
m <:: value = setVar value >> m
{-# INLINEABLE (<::) #-}

-- | Get a meta-variable for the expected type.
--
-- >>> import Prelude (Int)
-- >>> instance MetaVariable Int
--
-- >>> runMeta (getVar @Int)
-- (Nothing,VariableMap [])
--
-- >>> runMeta (getVar @Int <:: (12 :: Int))
-- (Just 12,VariableMap [Int])
getVar :: MetaVariable v => Meta (Maybe v)
getVar = Meta $ gets lookup
{-# INLINEABLE getVar #-}

-- | Get the value of a meta-variable or set a default one.
--
-- >>> import Prelude (String)
-- >>> instance MetaVariable String
--
-- >>> runMeta (getVarOrDefault "default")
-- ("default",VariableMap [[Char]])
--
-- >>> runMeta (getVarOrDefault "default" <:: "pre-set")
-- ("pre-set",VariableMap [[Char]])
getVarOrDefault :: MetaVariable v => v -> Meta v
getVarOrDefault value = Meta $ state (lookupOrInsert value)
{-# INLINEABLE getVarOrDefault #-}

-- | Execute a `Meta` monad with the given variables and return the output.
evalMetaWith :: VariableMap -> Meta a -> a
evalMetaWith vars (Meta m) = evalState m vars
{-# INLINEABLE evalMetaWith #-}

-- | Execute a `Meta` monad and return the output.
evalMeta :: Meta a -> a
evalMeta = evalMetaWith empty
{-# INLINEABLE evalMeta #-}

-- | Execute a `Meta` monad with the given variables and return the output and the final variables.
runMetaWith :: VariableMap -> Meta a -> (a, VariableMap)
runMetaWith vars (Meta m) = runState m vars
{-# INLINEABLE runMetaWith #-}

-- | Execute a `Meta` monad and return the output and the final variables.
runMeta :: Meta a -> (a, VariableMap)
runMeta = runMetaWith empty
{-# INLINEABLE runMeta #-}
