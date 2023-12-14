-- | Additional variables and parameters.
module MCSP.Data.Meta (
    -- * Collection of variables
    VariableMap,
    lookup,

    -- * Data Class
    MetaInputVariable (..),
    MetaOutputVariable (..),

    -- * Monadic operations
    Meta,
    getOrDefine,
    setOutputVar,
    inspect,
    (<::),

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

-- | Insert a meta-variable into the map.
--
-- >>> import Prelude (Int)
--
-- >>> insert @Int 12 empty
-- VariableMap [Int]
insert :: Typeable v => v -> VariableMap -> VariableMap
insert value (VariableMap vm) = VariableMap (Map.insert value vm)
{-# INLINEABLE insert #-}

-- | Return `Just` the value a meta-variable of the given type, or `Nothing` if no such variable is
-- available.
--
-- >>> import Prelude (Int)
-- >>> instance MetaOutputVariable Int
--
-- >>> lookup @Int empty
-- Nothing
--
-- >>> let (_, vars) = runMeta (setVar (12 :: Int))
-- >>> lookup @Int vars
-- Just 12
lookup :: Typeable v => VariableMap -> Maybe v
lookup (VariableMap vars) = Map.lookup vars
{-# INLINEABLE lookup #-}

-- | Return the previous value of a `MetaVariable` of the given type, or insert and return the
-- default value if no such variable is available.
--
-- >>> import Prelude (Int)
-- >>> instance MetaOutputVariable Int
--
-- >>> lookupOrInsert (12 :: Int) empty
-- (12,VariableMap [Int])
--
-- >>> let (_, vars) = runMeta (setVar (34 :: Int))
-- >>> lookupOrInsert (12 :: Int) vars
-- (34,VariableMap [Int])
lookupOrInsert :: Typeable v => v -> VariableMap -> (v, VariableMap)
lookupOrInsert defaultValue vars = case lookup vars of
    Just value -> (value, vars)
    Nothing -> (defaultValue, insert defaultValue vars)
{-# INLINEABLE lookupOrInsert #-}

-- ---------------------- --
-- Dynamic Variable Class --

-- | Represents a additional variable that can be used as input of a computation.
--
-- These variables are set and resolved dynamically inside the `Meta` monad.
class Typeable v => MetaInputVariable v where
    -- | Extracts the input variable, possibly modifying the environment.
    getVar :: Meta v

-- | Represents a additional variable that can be set as output of a computation.
--
-- These variables are set and resolved dynamically inside the `Meta` monad.
class Typeable v => MetaOutputVariable v where
    -- | Updates the output variable, possibly modifying the environment.
    setVar :: v -> Meta ()
    setVar = setOutputVar

-- ----------------- --
-- Monadic Operation --

-- | A monad that represents operation with meta-variables.
--
-- The meta-variables may be used as input, output or both.
newtype Meta a = Meta (State VariableMap a)
    deriving newtype (Functor, Applicative, Monad)

-- | Get the value of an input meta-variable or set a default one.
--
-- >>> import Prelude (String)
-- >>> instance MetaInputVariable String where getVar = getOrDefine ""
--
-- >>> runMeta (getOrDefine "default")
-- ("default",VariableMap [[Char]])
--
-- >>> runMeta (getOrDefine "default" <:: "pre-set")
-- ("pre-set",VariableMap [[Char]])
getOrDefine :: MetaInputVariable v => v -> Meta v
getOrDefine value = Meta $ state (lookupOrInsert value)
{-# INLINEABLE getOrDefine #-}

-- | Set the value of meta-variable for the given type, without any additional side-effect.
--
-- >>> import Prelude (Int)
-- >>> instance MetaOutputVariable Int where
-- >>>     setVar = setOutputVar
--
-- >>> runMeta (setOutputVar @Int 12)
-- ((),VariableMap [Int])
setOutputVar :: MetaOutputVariable v => v -> Meta ()
setOutputVar value = Meta (modify $ insert value)
{-# INLINEABLE setOutputVar #-}

-- | Set an input meta-variable for the given type.
--
-- >>> import Prelude (Int)
-- >>> instance MetaInputVariable Int where getVar = getOrDefine 0
--
-- >>> runMeta (getVar @Int <:: (12 :: Int))
-- (12,VariableMap [Int])
(<::) :: MetaInputVariable v => Meta a -> v -> Meta a
m <:: value = Meta (modify $ insert value) >> m
{-# INLINEABLE (<::) #-}

-- | Get a meta-variable for the expected type.
--
-- >>> import Prelude (Int)
-- >>> instance MetaOutputVariable Int
--
-- >>> runMeta (inspect @Int)
-- (Nothing,VariableMap [])
--
-- >>> runMeta (setVar (12 :: Int) >> inspect @Int)
-- (Just 12,VariableMap [Int])
inspect :: MetaOutputVariable v => Meta (Maybe v)
inspect = Meta $ gets lookup
{-# INLINEABLE inspect #-}

-- | Execute a `Meta` monad and return the output.
evalMeta :: Meta a -> a
evalMeta (Meta m) = evalState m empty
{-# INLINEABLE evalMeta #-}

-- | Execute a `Meta` monad and return the output and the final variables.
runMeta :: Meta a -> (a, VariableMap)
runMeta (Meta m) = runState m empty
{-# INLINEABLE runMeta #-}
