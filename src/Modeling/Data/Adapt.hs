module Modeling.Data.Adapt where

import Data.Aeson
import Modeling.Data.Core
import Modeling.Data.Util

instance ToJSON ExtParam
instance FromJSON ExtParam

instance ToJSON Interface
instance FromJSON Interface

instance ToJSON ModelConnection
instance FromJSON ModelConnection

instance ToJSON Model
instance FromJSON Model

instance ToJSON ModelSpace
instance FromJSON ModelSpace

instance ToJSON Bundle
instance FromJSON Bundle
