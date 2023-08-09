module Strings.System.Random.Static (mkStaticSeed) where

import Data.Word (Word64)
import System.Random.PCG (withSystemRandom)
import System.Random.PCG.Class (uniformW64)

import Language.Haskell.TH (
    DecsQ,
    ExpQ,
    Lit (IntegerL),
    Quote (newName),
    litE,
    runIO,
    sigD,
    varP,
 )

-- | Generates a randomized literal `Word64`.
mkWord64 :: ExpQ
mkWord64 = do
    seed <- runIO $ withSystemRandom uniformW64
    litE $ IntegerL (toInteger seed)

-- | Defines a random seed at compile time.
--
-- The seed is stored in a variable with the given name.
mkStaticSeed :: String -> DecsQ
mkStaticSeed varName = do
    seedN <- newName varName
    seedDecl <- sigD seedN [t|(Word64, Word64)|]
    seedDef <- [d|$(varP seedN) = ($(mkWord64), $(mkWord64))|]
    pure (seedDecl : seedDef)
