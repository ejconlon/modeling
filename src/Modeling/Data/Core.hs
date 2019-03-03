module Modeling.Data.Core where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Modeling.Data.Common
import Modeling.Data.Type

data ExtParam = ExtParam
    { ns :: Namespace
    , name :: ParamName
    , ty :: Type
    } deriving (Generic, Show, Eq)

instance ToJSON ExtParam
instance FromJSON ExtParam

data Interface = Interface
    { params :: Seq ExtParam
    , tydefs :: Map Text Type
    } deriving (Generic, Show, Eq)

instance ToJSON Interface
instance FromJSON Interface

data Bundle a = Bundle
    { interface :: Interface
    , root :: a
    } deriving (Generic, Show, Eq)

instance ToJSON a => ToJSON (Bundle a)
instance FromJSON a => FromJSON (Bundle a)
