module Strings.System.Random.Static (
    mkWord64,
    Random,
    mkRandomQ,
    mkRandom,
    mkRandomE,
) where

import Data.Word (Word64)
import System.Random.PCG (GenIO, withSystemRandom)
import System.Random.PCG.Class (uniformW64)

import Language.Haskell.TH (CodeQ, ExpQ, Q, bindCode, runIO)
import Language.Haskell.TH.Syntax (Lift (liftTyped), unTypeCode)

-- | A random value generator .
type Random a = GenIO -> IO a

-- | Yields a random `a` in the quotation monad.
mkRandomQ :: Random a -> Q a
mkRandomQ = runIO . withSystemRandom

-- | Generates a random `a` in compile-time.
mkRandom :: Lift a => Random a -> CodeQ a
mkRandom r = bindCode (mkRandomQ r) liftTyped

-- | Generates an untyped random `a` in compile-time.
mkRandomE :: Lift a => Random a -> ExpQ
mkRandomE = unTypeCode . mkRandom

-- | Generates a randomized literal `Word64` at compile time.
mkWord64 :: CodeQ Word64
mkWord64 = mkRandom uniformW64
