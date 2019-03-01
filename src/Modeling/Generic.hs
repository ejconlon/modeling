module Modeling.Generic where

import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

data GenericTypeSingleAttrs = GenericTypeSingleAttrs
    { ty :: GenericType
    } deriving (Generic, Show, Eq)

instance ToJSON GenericTypeSingleAttrs
instance FromJSON GenericTypeSingleAttrs

data GenericTypeReferenceAttrs = GenericTypeReferenceAttrs
    { name :: Text
    } deriving (Generic, Show, Eq)

instance ToJSON GenericTypeReferenceAttrs
instance FromJSON GenericTypeReferenceAttrs

data GenericTypeStructAttrs = GenericTypeStructAttrs
    { fields :: Map Text GenericType
    } deriving (Generic, Show, Eq)

instance ToJSON GenericTypeStructAttrs
instance FromJSON GenericTypeStructAttrs

data GenericTypeAttrs = GenericTypeAttrs
    { optional :: Maybe GenericTypeSingleAttrs
    , list :: Maybe GenericTypeSingleAttrs
    , stringmap :: Maybe GenericTypeSingleAttrs
    , struct :: Maybe (Map Text GenericType)
    , reference :: Maybe GenericTypeReferenceAttrs
    } deriving (Generic, Show, Eq)

instance ToJSON GenericTypeAttrs
instance FromJSON GenericTypeAttrs

data GenericType = GenericType
    { name :: Text
    , attributes :: Maybe GenericTypeAttrs
    } deriving (Generic, Show, Eq)

instance ToJSON GenericType
instance FromJSON GenericType