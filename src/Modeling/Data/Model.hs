module Modeling.Data.Model where

import Data.Aeson
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics (Generic)
import Modeling.Data.Bidi
import Modeling.Data.Common
import Modeling.Data.Util

data ModelName =
      DirectModelName
    | SerialModelName
    | ParallelModelName
    | AdaptorModelName
    | SplitModelName
    deriving (Generic, Show, Eq)

modelNameToText :: Injection ErrorMsg ModelName Text
modelNameToText = Injection apply invert where
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

instance ToJSON ModelName where
    toJSON = injectionToJSON modelNameToText

instance FromJSON ModelName where
    parseJSON = injectionParseJSON renderErrorMsg modelNameToText

data ModelDirectAttrs = ModelDirectAttrs
    { name :: Text
    } deriving (Generic, Show, Eq)

instance ToJSON ModelDirectAttrs
instance FromJSON ModelDirectAttrs

data ModelSerialAttrs a = ModelSerialAttrs
    { models :: Seq a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (ModelSerialAttrs a)
instance FromJSON a => FromJSON (ModelSerialAttrs a)

data ModelSplitAttrs a = ModelSplitAttrs
    { attribute :: Text
    , values :: Seq Text
    , other :: Maybe Text
    , model :: a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (ModelSplitAttrs a)
instance FromJSON a => FromJSON (ModelSplitAttrs a)

data ModelAttrs a = ModelAttrs
    { direct :: ModelDirectAttrs
    , serial :: ModelSerialAttrs a
    , split :: ModelSplitAttrs a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (ModelAttrs a)
instance FromJSON a => FromJSON (ModelAttrs a)

data ModelSum a = ModelSum
    { name :: ModelName
    , attributes :: Maybe (ModelAttrs a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (ModelSum a)
instance FromJSON a => FromJSON (ModelSum a)

data Model a =
      DirectModel ModelDirectAttrs
    | SerialModel (ModelSerialAttrs a)
    | SplitModel (ModelSplitAttrs a)
    deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
