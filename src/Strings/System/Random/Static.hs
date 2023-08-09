module Strings.System.Random.Static (mkStaticSeed) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Word (Word64)
import System.Random.PCG (withSystemRandom)
import System.Random.PCG.Class (uniformW64)

import Language.Haskell.TH (
    Body (NormalB),
    Dec (SigD, ValD),
    Exp (LitE),
    Lit (IntegerL),
    Pat (VarP),
    Q,
    Quote (newName),
    Type (ConT),
 )

-- | Generates a random seed at compile time.
--
-- The seed is stored in a variable with the given name.
mkStaticSeed :: String -> Q [Dec]
mkStaticSeed varName = do
    seedN <- newName varName
    seed <- liftIO $ withSystemRandom uniformW64
    let litSeed = LitE $ IntegerL $ toInteger seed
    let seedDecl = SigD seedN (ConT ''Word64)
    let seedDef = ValD (VarP seedN) (NormalB litSeed) []
    pure [seedDecl, seedDef]
