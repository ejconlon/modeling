module Modeling.Data.Outside where

import Data.Aeson
import GHC.Generics (Generic, Generic1)
import Modeling.Data.Aeson
import Modeling.Data.Core
import Modeling.Data.Model

newtype ModelSpace a = ModelSpace { unModelSpace :: Space (Model a) }
    deriving (Generic1, Eq, Show, Functor, Foldable, Traversable)
    deriving (ToJSON1, FromJSON1) via (AesonWrapper1 ModelSpace)

newtype ModelSpaceFix = ModelSpaceFix { unModelSpaceFix :: ModelSpace ModelSpaceFix }
    deriving (Generic, Eq, Show)
    deriving (ToJSON, FromJSON) via (AesonWrapperApp ModelSpace ModelSpaceFix)

newtype ModelSpaceBundle = ModelSpaceBundle { unModelSpaceBundle :: Bundle ModelSpaceFix }
    deriving (Generic, Eq, Show)
    deriving (ToJSON, FromJSON) via (AesonWrapperApp Bundle ModelSpaceFix)
