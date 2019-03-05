module Modeling.Data.Outside where

import Data.Aeson
import GHC.Generics (Generic)
import Modeling.Data.Aeson
import Modeling.Data.Core
import Modeling.Data.Model

newtype ModelSpace a = ModelSpace { unModelSpace :: Space (Model a) }
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

-- TODO weaken constraints when Model constraints are fixed
deriving via (AesonWrapper (Space (Model a))) instance (ToJSON a, FromJSON a) => ToJSON (ModelSpace a)
deriving via (AesonWrapper (Space (Model a))) instance (ToJSON a, FromJSON a) => FromJSON (ModelSpace a)

newtype ModelSpaceFix = ModelSpaceFix { unModelSpaceFix :: ModelSpace ModelSpaceFix }
    deriving (Generic, Eq, Show)

deriving via (AesonWrapper (ModelSpace ModelSpaceFix)) instance ToJSON ModelSpaceFix
deriving via (AesonWrapper (ModelSpace ModelSpaceFix)) instance FromJSON ModelSpaceFix

newtype ModelSpaceBundle = ModelSpaceBundle { unModelSpaceBundle :: Bundle ModelSpaceFix }
    deriving (Generic, Eq, Show)

deriving via (AesonWrapper (Bundle ModelSpaceFix)) instance ToJSON ModelSpaceBundle
deriving via (AesonWrapper (Bundle ModelSpaceFix)) instance FromJSON ModelSpaceBundle
