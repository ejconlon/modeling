module Modeling.Data.Outside where

import Data.Aeson
import GHC.Generics (Generic)
import Modeling.Data.Core
import Modeling.Data.Model

newtype ModelSpace a = ModelSpace { unModelSpace :: Space (Model a) }
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)
    deriving (ToJSON, FromJSON) via (Space (Model a))

newtype ModelSpaceFix = ModelSpaceFix { unModelSpaceFix :: ModelSpace ModelSpaceFix }
    deriving (Generic, Eq, Show)
    deriving (ToJSON, FromJSON) via (ModelSpace ModelSpaceFix)

newtype ModelSpaceBundle = ModelSpaceBundle { unModelSpaceBundle :: Bundle ModelSpaceFix }
    deriving (Generic, Eq, Show)
    deriving (ToJSON, FromJSON) via (Bundle ModelSpaceFix)
