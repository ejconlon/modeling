module Modeling.Data.Outside where

import Data.Aeson
import Modeling.Data.Aeson
import Modeling.Data.Core
import Modeling.Data.Model

newtype ModelSpace a = ModelSpace { unModelSpace :: Space (Model a) }
    deriving (Eq, Show, Functor, Foldable, Traversable)
    deriving (ToJSON1, FromJSON1) via (AesonWrapperComp Space Model)

newtype ModelSpaceFix = ModelSpaceFix { unModelSpaceFix :: ModelSpace ModelSpaceFix }
    deriving (Eq, Show)
    deriving (ToJSON, FromJSON) via (AesonWrapperApp ModelSpace ModelSpaceFix)

newtype ModelSpaceBundle = ModelSpaceBundle { unModelSpaceBundle :: Bundle ModelSpaceFix }
    deriving (Eq, Show)
    deriving (ToJSON, FromJSON) via (AesonWrapperApp Bundle ModelSpaceFix)
