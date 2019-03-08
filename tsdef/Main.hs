{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import Data.Aeson.TypeScript.TH (deriveTypeScript, formatTSDeclarations, getTypeScriptDeclarations)
import Data.Proxy (Proxy (..))
import Modeling.Data.Core
import Modeling.Data.Outside
import Modeling.Data.Type

-- $(deriveTypeScript options ''TypeFix)
-- $(deriveTypeScript options ''Signature)
-- $(deriveTypeScript options ''Bundle)
-- $(deriveTypeScript options ''ModelSpaceBundle)

-- main :: IO ()
-- main = putStrLn $ formatTSDeclarations (getTypeScriptDeclarations (Proxy :: Proxy ModelSpaceBundle))  -- <> ...

main :: IO ()
main = pure ()
