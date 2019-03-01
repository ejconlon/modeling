module Modeling.Data.Generic where

import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

data Sum = Sum { name :: Text, attributes :: Maybe Value } deriving (Generic, Show, Eq)

instance ToJSON Sum
instance FromJSON Sum
