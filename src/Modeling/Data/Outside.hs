module Modeling.Data.Outside where

import Control.Newtype.Generics
import Data.Aeson
import GHC.Generics (Generic)
import Modeling.Data.Aeson
import Modeling.Data.Core
import Modeling.Data.Model

data ModelDeps a = ModelDeps
    { dependencies :: Dependencies a
    , model :: Model a
    } deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

deriving via (AesonWrapper (ModelDeps a)) instance ToJSON a => ToJSON (ModelDeps a)
deriving via (AesonWrapper (ModelDeps a)) instance FromJSON a => FromJSON (ModelDeps a)

newtype ModelSpace a = ModelSpace { unModelSpace :: Space (ModelDeps a) }
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

instance Newtype (ModelSpace a)
deriving via (AesonNewtype (ModelSpace a) (Space (ModelDeps a))) instance ToJSON a => ToJSON (ModelSpace a)
deriving via (AesonNewtype (ModelSpace a) (Space (ModelDeps a))) instance FromJSON a => FromJSON (ModelSpace a)

newtype ModelSpaceFix = ModelSpaceFix { unModelSpaceFix :: ModelSpace ModelSpaceFix }
    deriving (Generic, Eq, Show)

instance Newtype ModelSpaceFix
deriving via (AesonNewtype ModelSpaceFix (ModelSpace ModelSpaceFix)) instance ToJSON ModelSpaceFix
deriving via (AesonNewtype ModelSpaceFix (ModelSpace ModelSpaceFix)) instance FromJSON ModelSpaceFix

newtype ModelSpaceBundle = ModelSpaceBundle { unModelSpaceBundle :: Bundle ModelSpaceFix }
    deriving (Generic, Eq, Show)

instance Newtype ModelSpaceBundle
deriving via (AesonNewtype ModelSpaceBundle (Bundle ModelSpaceFix)) instance ToJSON ModelSpaceBundle
deriving via (AesonNewtype ModelSpaceBundle (Bundle ModelSpaceFix)) instance FromJSON ModelSpaceBundle
