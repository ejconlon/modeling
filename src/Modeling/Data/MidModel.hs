module Modeling.Data.MidModel where

import Data.Aeson
import Data.Map (Map)
import Data.Sequence (Seq)
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
    { name :: Text
    } deriving (Generic, Show, Eq)

instance ToJSON MidModelDirectAttrs
instance FromJSON MidModelDirectAttrs

data MidModelAttrs a = MidModelAttrs
    { direct :: MidModelDirectAttrs
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (MidModelAttrs a)
instance FromJSON a => FromJSON (MidModelAttrs a)

data MidModel a = MidModel
    { name :: MidModelName
    , attributes :: Maybe (MidModelAttrs a)
    } deriving (Generic, Show, Eq)

instance ToJSON a => ToJSON (MidModel a)
instance FromJSON a => FromJSON (MidModel a)

data MidModelConnection a = MidModelConnection
    { nspart :: NamespacePart
    , namedModels :: Map Text a
    , additionalModels :: Seq a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (MidModelConnection a)
instance FromJSON a => FromJSON (MidModelConnection a)

data MidModelSpace a = MidModelSpace
    { connection :: MidModelConnection a
    , model :: MidModel a
    } deriving (Generic, Show, Eq)

instance ToJSON a => ToJSON (MidModelSpace a)
instance FromJSON a => FromJSON (MidModelSpace a)
