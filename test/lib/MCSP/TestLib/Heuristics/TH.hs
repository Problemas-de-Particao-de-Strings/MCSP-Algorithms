-- | Template to extract name and value of a list of items.
module MCSP.TestLib.Heuristics.TH (
    mkNamedList,
) where

import Control.Applicative (pure)
import Control.Monad (fail)
import Data.Bool (not, otherwise)
import Data.Foldable (foldl')
import Data.Function (flip, ($), (.))
import Data.List (map, null, (++))
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (mempty)
import Data.String qualified as Text
import Text.Show (show)

import Data.Map.Strict (alter, filter, keysSet)
import Data.Set (Set, member)

import Language.Haskell.TH (ExpQ, Name, Q, listE, nameBase, nameModule, varE)

-- | The set of all `nameBase` in a collection of names that appear only once.
--
-- >>> import Data.Map (Map)
-- >>> import Data.Set (Set)
-- >>> uniqueBase [''Map, ''Set]
-- fromList ["Map","Set"]
-- >>> import Data.Map.Strict (Map)
-- >>> uniqueBase [''Data.Map.Map, ''Data.Map.Strict.Map]
-- fromList []
uniqueBase :: [Name] -> Set Text.String
uniqueBase = keysSet . filter not . repeated . map nameBase
  where
    increment v = pure (isJust v)
    repeated = foldl' (flip $ alter increment) mempty

-- | `nameBase` or fully qualified name for an item, depending if it is in the @uniq@ or not.
--
-- >>> import Language.Haskell.TH (runQ)
-- >>> import Data.Map.Lazy (Map)
-- >>> import Data.Map.Strict (Map)
-- >>> import Data.Set (Set)
-- >>> names = uniqueBase [''Data.Map.Lazy.Map, ''Data.Map.Strict.Map, ''Data.Set.Set]
-- >>> runQ (getUnambiguous names ''Data.Map.Lazy.Map)
-- "Data.Map.Internal.Map"
-- >>> runQ (getUnambiguous names ''Data.Set.Set)
-- "Set"
getUnambiguous :: Set Text.String -> Name -> Q Text.String
getUnambiguous uniq name
    | base `member` uniq = pure base
    | not (null mod) = pure (mod ++ "." ++ base)
    | otherwise = fail (show name ++ " cannot be resolved to a unique name")
  where
    base = nameBase name
    mod = fromMaybe "" (nameModule name)

-- | Create an expression that evaluates to @(name, value)@ for the given named item.
--
-- >>> (x, y) = (2, 3)
-- >>> names = uniqueBase ['x]
-- >>> $(mkNamed names 'x)
-- ("x",2)
mkNamed :: Set Text.String -> Name -> ExpQ
mkNamed uniq name = do
    uniqName <- getUnambiguous uniq name
    [e|(uniqName, $(varE name))|]

-- | Maps a list of items to pairs @(name, value)@.
--
-- >>> import Data.Int (Int)
-- >>> (x, y) = (2, 3)
-- >>> $(mkNamedList ['x, 'y]) :: [(Text.String, Int)]
-- [("x",2),("y",3)]
mkNamedList :: [Name] -> ExpQ
mkNamedList names = listE (map (mkNamed (uniqueBase names)) names)
