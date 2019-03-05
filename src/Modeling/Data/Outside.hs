module Modeling.Data.Outside where

import Data.Aeson
import GHC.Generics (Generic)
import Modeling.Data.Fix
import Modeling.Data.Core
import Modeling.Data.Model

newtype ModelSpace a = ModelSpace { unModelSpace :: Space (Model a) }
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)
    deriving (ToJSON, FromJSON) via (Space (Model a))

newtype ModelSpaceFix = ModelSpaceFix { unModelSpaceFix :: Fix ModelSpace }
    deriving (Generic, Eq, Show)
    deriving (ToJSON, FromJSON) via (Fix ModelSpace)

newtype ModelSpaceBundle = ModelSpaceBundle { unModelSpaceBundle :: Bundle ModelSpaceFix }
    deriving (Generic, Eq, Show)
    deriving (ToJSON, FromJSON) via (Bundle ModelSpaceFix)
