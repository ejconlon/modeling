module Modeling.Data.Outside where

import Control.Newtype.Generics
import Data.Aeson
import GHC.Generics (Generic)
import Modeling.Data.Aeson
import Modeling.Data.Core
import Modeling.Data.Model

data DepModel a = DepModel
    { dependencies :: Maybe (Dependencies a)
    , model :: Model a
    } deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

deriving via (AesonWrapper (DepModel a)) instance ToJSON a => ToJSON (DepModel a)
deriving via (AesonWrapper (DepModel a)) instance FromJSON a => FromJSON (DepModel a)

newtype ModelSpace a = ModelSpace { unModelSpace :: Space (DepModel a) }
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

instance Newtype (ModelSpace a)
deriving via (AesonNewtype (ModelSpace a) (Space (DepModel a))) instance ToJSON a => ToJSON (ModelSpace a)
deriving via (AesonNewtype (ModelSpace a) (Space (DepModel a))) instance FromJSON a => FromJSON (ModelSpace a)

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
