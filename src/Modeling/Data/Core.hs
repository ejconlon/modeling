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

data Interface = Interface
    { params :: Seq ExtParam
    , tydefs :: Map Text Type
    } deriving (Generic, Show, Eq)

data ModelConnection = ModelConnection
    {
    } deriving (Generic, Show, Eq)

data Model = Model
    {
    } deriving (Generic, Show, Eq)

data ModelSpace = ModelSpace
    { connection :: ModelConnection
    , model :: Model
    } deriving (Generic, Show, Eq)

data Bundle = Bundle
    { interface :: Interface
    , root :: ModelSpace
    } deriving (Generic, Show, Eq)
