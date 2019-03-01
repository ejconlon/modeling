module Modeling.Data.Mid where

import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

data MidTypeSingleAttrs = MidTypeSingleAttrs
    { ty :: MidType
    } deriving (Generic, Show, Eq)

instance ToJSON MidTypeSingleAttrs
instance FromJSON MidTypeSingleAttrs

data MidTypeReferenceAttrs = MidTypeReferenceAttrs
    { name :: Text
    } deriving (Generic, Show, Eq)

instance ToJSON MidTypeReferenceAttrs
instance FromJSON MidTypeReferenceAttrs

data MidTypeStructAttrs = MidTypeStructAttrs
    { fields :: Map Text MidType
    } deriving (Generic, Show, Eq)

instance ToJSON MidTypeStructAttrs
instance FromJSON MidTypeStructAttrs

data MidTypeAttrs = MidTypeAttrs
    { optional :: Maybe MidTypeSingleAttrs
    , list :: Maybe MidTypeSingleAttrs
    , stringmap :: Maybe MidTypeSingleAttrs
    , struct :: Maybe (Map Text MidType)
    , reference :: Maybe MidTypeReferenceAttrs
    } deriving (Generic, Show, Eq)

instance ToJSON MidTypeAttrs
instance FromJSON MidTypeAttrs

data MidType = MidType
    { name :: Text
    , attributes :: Maybe MidTypeAttrs
    } deriving (Generic, Show, Eq)

instance ToJSON MidType
instance FromJSON MidType
