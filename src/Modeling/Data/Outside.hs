module Modeling.Data.Outside where

import Control.Newtype.Generics
import Data.Aeson
import GHC.Generics (Generic)
import Modeling.Data.Aeson
import Modeling.Data.Core
import Modeling.Data.Model

newtype ModelSpace a = ModelSpace { unModelSpace :: Space (Model a) }
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonNewtype (ModelSpace a) (Space (Model a)))

instance Newtype (ModelSpace a)

newtype ModelSpaceFix = ModelSpaceFix { unModelSpaceFix :: ModelSpace ModelSpaceFix }
    deriving (Generic, Eq, Show)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonNewtype ModelSpaceFix (ModelSpace ModelSpaceFix))

instance Newtype ModelSpaceFix

newtype ModelSpaceBundle = ModelSpaceBundle { unModelSpaceBundle :: Bundle ModelSpaceFix }
    deriving (Generic, Eq, Show)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonNewtype ModelSpaceBundle (Bundle ModelSpaceFix))

instance Newtype ModelSpaceBundle
