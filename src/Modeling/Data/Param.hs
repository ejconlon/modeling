module Modeling.Data.Param where

import Data.Aeson
import GHC.Generics

data ParamCon =
      LitParamCon
    deriving (Generic, Eq, Show)

data LiteralParamAttrs = LitParamAttrs
    { value :: Value
    } deriving (Generic, Eq, Show)

instance ToJSON LiteralParamAttrs
instance FromJSON LiteralParamAttrs

data ParamAttrs = ParamAttrs
    { literal :: Maybe (LiteralParamAttrs)
    } deriving (Generic, Eq, Show)

data ParamSum = ParamSum
    { name :: ParamCon
    , attributes :: Maybe ParamAttrs
    } deriving (Generic, Eq, Show)

data Param =
      LiteralParam LiteralParamAttrs
    deriving (Generic, Eq, Show)
