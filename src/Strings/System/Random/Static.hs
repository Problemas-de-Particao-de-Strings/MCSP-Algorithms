-- | Compile time random value generation.
module Strings.System.Random.Static (
    mkWord64,
    RandomIO,
    mkRandomQ,
    mkRandom,
    mkRandomE,
) where

import Data.Word (Word64)
import System.Random.PCG (GenIO, withSystemRandom)
import System.Random.PCG.Class (uniformW64)

import Language.Haskell.TH (CodeQ, ExpQ, Q, bindCode, runIO)
import Language.Haskell.TH.Syntax (Lift (liftTyped), unTypeCode)

-- | A random value generator in the `IO` Monad.
type RandomIO a = GenIO -> IO a

-- | Yields a random @a@ in the quotation monad.
mkRandomQ :: RandomIO a -> Q a
mkRandomQ = runIO . withSystemRandom

-- | Generates a random @a@ in compile-time.
mkRandom :: Lift a => RandomIO a -> CodeQ a
mkRandom r = bindCode (mkRandomQ r) liftTyped

-- | Generates an untyped random @a@ in compile-time.
mkRandomE :: Lift a => RandomIO a -> ExpQ
mkRandomE = unTypeCode . mkRandom

-- | Generates a randomized literal `Word64` at compile time.
--
-- >>> $$mkWord64
-- 5802473051628008271
mkWord64 :: CodeQ Word64
mkWord64 = mkRandom uniformW64
