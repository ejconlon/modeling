module Modeling.Data.MidModel where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Modeling.Data.Common
import Modeling.Data.Util

data MidModelName =
    DirectModelName
    | SerialModelName
    | ParallelModelName
    | AdaptorModelName
    | SplitModelName
    deriving (Generic, Show, Eq)

midModelNameToText :: Injection ErrorMsg MidModelName Text
midModelNameToText = Injection apply invert where
    apply t =
        case t of
            DirectModelName -> "direct"
            SerialModelName -> "serial"
            ParallelModelName -> "parallel"
            AdaptorModelName -> "adaptor"
            SplitModelName -> "split"
    invert u =
        case u of
            "direct" -> Right DirectModelName
            "serial" -> Right SerialModelName
            "parallel" -> Right ParallelModelName
            "adaptor" -> Right AdaptorModelName
            "split" -> Right SplitModelName
            _ -> Left (ErrorMsg ("Unknown model name " <> u))

instance ToJSON MidModelName where
    toJSON = injectionToJSON midModelNameToText

instance FromJSON MidModelName where
    parseJSON = injectionParseJSON renderErrorMsg midModelNameToText

data MidModelDirectAttrs = MidModelDirectAttrs
    {
    } deriving (Generic, Show, Eq)

instance ToJSON MidModelDirectAttrs
instance FromJSON MidModelDirectAttrs

data MidModelAttrs a = MidModelAttrs
    { direct :: MidModelDirectAttrs
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (MidModelAttrs a)
instance FromJSON a => FromJSON (MidModelAttrs a)

data MidModel = MidModel
    { name :: MidModelName
    , attributes :: Maybe (MidModelAttrs MidModel)
    } deriving (Generic, Show, Eq)

instance ToJSON MidModel
instance FromJSON MidModel

data MidModelSpace = MidModelSpace
    { nspart :: NamespacePart
    -- bring ns back into the fold - want total freedom to rewrite models which includes
    -- supporting model types we don't know about yet. so need to be able to map params, load models etc
    -- not just for direct models.  direct becomes just { "direct" : { "name": "xyz" } }
    -- The space around it is { "metadata": {"ns": .., "params": ...}, "model":  }
    , model :: MidModel
    } deriving (Generic, Show, Eq)

instance ToJSON MidModelSpace
instance FromJSON MidModelSpace
