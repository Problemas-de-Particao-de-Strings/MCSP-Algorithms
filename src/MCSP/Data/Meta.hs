-- | Additional variables and parameters.
module MCSP.Data.Meta (
    MetaVariable,

    -- * Collection of variables
    VariableMap,
    lookup,

    -- * Monadic operations
    Meta,
    setVar,
    (<::),
    getVar,
    getVarOrDefault,

    -- ** Execution
    evalMeta,
    runMeta,
) where

import Control.Applicative (Applicative)
import Control.Monad (Monad, (>>))
import Control.Monad.Trans.State.Strict (State, evalState, gets, modify, runState, state)
import Data.Function (id, ($), (.))
import Data.Functor (Functor (..))
import Data.Maybe (Maybe (..))
import Data.TypeMap.Dynamic.Alt qualified as Map (Item, TypeMap, empty, insert, lookup, map, toList)
import Data.Typeable (Typeable, showsTypeRep, typeOf)
import Text.Show (Show (..), showChar, showListWith, showString)

-- | Represents a additional variable that can be used as input or output of a computation.
--
-- These variables are set and resolved dynamically inside the `Meta` monad.
class Typeable v => MetaVariable v

-- ---------- --
-- Collection --

-- | A fully dynamic `TypeMap` mapping from a type to itself.
data Dynamic

type instance Map.Item Dynamic v = v

-- | A polymorphic storage of `MetaVariable`s.
newtype VariableMap = VariableMap (Map.TypeMap Dynamic)

instance Show VariableMap where
    showsPrec _ (VariableMap vars) =
        showString "VariableMap"
            . showChar ' '
            . showListWith id keys
      where
        keys = Map.toList $ Map.map (showsTypeRep . typeOf) vars

-- | A map with no variables set.
--
-- >>> empty
-- VariableMap []
empty :: VariableMap
empty = VariableMap Map.empty
{-# INLINEABLE empty #-}

-- | Return `Just` the value a `MetaVariable` of the given type, or `Nothing` if no such variable is
-- available.
--
-- >>> import Prelude (Int)
-- >>> instance MetaVariable Int
--
-- >>> lookup @Int empty
-- Nothing
--
-- >>> let (_, vars) = runMeta (setVar (12 :: Int))
-- >>> lookup @Int vars
-- Just 12
lookup :: MetaVariable v => VariableMap -> Maybe v
lookup (VariableMap vars) = Map.lookup vars
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
-- >>> let (_, vars) = runMeta (setVar (34 :: Int))
-- >>> lookupOrInsert (12 :: Int) vars
-- (34,VariableMap [Int])
lookupOrInsert :: MetaVariable v => v -> VariableMap -> (v, VariableMap)
lookupOrInsert defaultValue vars = case lookup vars of
    Just value -> (value, vars)
    Nothing -> (defaultValue, insert defaultValue vars)
  where
    insert value (VariableMap vm) = VariableMap $ Map.insert value vm
{-# INLINEABLE lookupOrInsert #-}

-- ----------------- --
-- Monadic Operation --

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
setVar value = Meta $ modify $ variableMap (Map.insert value)
  where
    variableMap f (VariableMap vm) = VariableMap (f vm)
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

-- | Execute a `Meta` monad and return the output.
evalMeta :: Meta a -> a
evalMeta (Meta m) = evalState m empty
{-# INLINEABLE evalMeta #-}

-- | Execute a `Meta` monad and return the output and the final variables.
runMeta :: Meta a -> (a, VariableMap)
runMeta (Meta m) = runState m empty
{-# INLINEABLE runMeta #-}
