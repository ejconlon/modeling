module Modeling.Data.Outside where

import Control.Newtype.Generics
import Data.Aeson
import GHC.Generics (Generic)
import Modeling.Data.Aeson
import Modeling.Data.Core
import Modeling.Data.Generics
import Modeling.Data.JsonRep
import Modeling.Data.Model

newtype ModelSpace a = ModelSpace { unModelSpace :: Space (Model a) }
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonNewtype (ModelSpace a) (Space (Model a)))

deriving via (GenRepNewtype (ModelSpace a) (Space (Model a))) instance HasGenRep JsonRep a => HasGenRep JsonRep (ModelSpace a)

instance Newtype (ModelSpace a)

newtype ModelSpaceFix = ModelSpaceFix { unModelSpaceFix :: ModelSpace ModelSpaceFix }
    deriving (Generic, Eq, Show)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonNewtype ModelSpaceFix (ModelSpace ModelSpaceFix))
    deriving (HasGenRep JsonRep) via (GenRepFix ModelSpaceFix ModelSpace)

instance Newtype ModelSpaceFix

newtype ModelSpaceBundle = ModelSpaceBundle { unModelSpaceBundle :: Bundle ModelSpaceFix }
    deriving (Generic, Eq, Show)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonNewtype ModelSpaceBundle (Bundle ModelSpaceFix))
    deriving (HasGenRep JsonRep) via (GenRepNewtype ModelSpaceBundle (Bundle ModelSpaceFix))

instance Newtype ModelSpaceBundle
