module Modeling.Data.Model where

import Data.Aeson
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics (Generic)
import Modeling.Data.Aeson
import Modeling.Data.Bidi
import Modeling.Data.Common
import Modeling.Data.Util

data ModelCon =
      DirectModelCon
    | SerialModelCon
    | ParallelModelCon
    | AdaptorModelCon
    | SplitModelCon
    deriving (Generic, Show, Eq)

modelConToText :: Injection ErrorMsg ModelCon Text
modelConToText = Injection apply invert where
    apply t =
        case t of
            DirectModelCon -> "direct"
            SerialModelCon -> "serial"
            ParallelModelCon -> "parallel"
            AdaptorModelCon -> "adaptor"
            SplitModelCon -> "split"
    invert u =
        case u of
            "direct" -> Right DirectModelCon
            "serial" -> Right SerialModelCon
            "parallel" -> Right ParallelModelCon
            "adaptor" -> Right AdaptorModelCon
            "split" -> Right SplitModelCon
            _ -> Left (ErrorMsg ("Unknown model name " <> u))

instance ToJSON ModelCon where
    toJSON = injectionToJSON modelConToText
    toEncoding = injectionToEncoding modelConToText

instance FromJSON ModelCon where
    parseJSON = injectionParseJSON renderErrorMsg modelConToText

data ModelDirectAttrs = ModelDirectAttrs
    { name :: Text
    } deriving (Generic, Show, Eq)
      deriving (ToJSON, FromJSON) via (AesonWrapper ModelDirectAttrs)

data ModelSerialAttrs a = ModelSerialAttrs
    { models :: Seq a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

deriving via (AesonWrapper (ModelSerialAttrs a)) instance ToJSON a => ToJSON (ModelSerialAttrs a)
deriving via (AesonWrapper (ModelSerialAttrs a)) instance FromJSON a => FromJSON (ModelSerialAttrs a)

data ModelSplitAttrs a = ModelSplitAttrs
    { attribute :: Text
    , values :: Seq Text
    , other :: Maybe Text
    , model :: a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

deriving via (AesonWrapper (ModelSplitAttrs a)) instance ToJSON a => ToJSON (ModelSplitAttrs a)
deriving via (AesonWrapper (ModelSplitAttrs a)) instance FromJSON a => FromJSON (ModelSplitAttrs a)

data ModelAttrs a = ModelAttrs
    { direct :: Maybe (ModelDirectAttrs)
    , serial :: Maybe (ModelSerialAttrs a)
    , split :: Maybe (ModelSplitAttrs a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

deriving via (AesonWrapper (ModelAttrs a)) instance ToJSON a => ToJSON (ModelAttrs a)
deriving via (AesonWrapper (ModelAttrs a)) instance FromJSON a => FromJSON (ModelAttrs a)

emptyModelAttrs :: ModelAttrs a
emptyModelAttrs = ModelAttrs Nothing Nothing Nothing

data ModelSum a = ModelSum
    { name :: ModelCon
    , attributes :: Maybe (ModelAttrs a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

deriving via (AesonWrapper (ModelSum a)) instance ToJSON a => ToJSON (ModelSum a)
deriving via (AesonWrapper (ModelSum a)) instance FromJSON a => FromJSON (ModelSum a)

data Model a =
      DirectModel ModelDirectAttrs
    | SerialModel (ModelSerialAttrs a)
    | SplitModel (ModelSplitAttrs a)
    deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

modelToPair :: Model a -> ModelSum a
modelToPair t =
    case t of
        DirectModel attrs -> ModelSum DirectModelCon (Just (emptyModelAttrs { direct = Just attrs }))
        SerialModel attrs -> ModelSum SplitModelCon (Just (emptyModelAttrs { serial = Just attrs }))
        SplitModel attrs -> ModelSum SplitModelCon (Just (emptyModelAttrs { split = Just attrs }))

modelFromPair :: ModelSum a -> Either ErrorMsg (Model a)
modelFromPair (ModelSum n ma) = f ma where
    f = case n of
        DirectModelCon -> withAttrs direct DirectModel
        SerialModelCon -> withAttrs serial SerialModel
        SplitModelCon -> withAttrs split SplitModel

instance ToJSON a => ToJSON (Model a) where
    toJSON = toJSON . modelToPair
    toEncoding = toEncoding . modelToPair

instance FromJSON a => FromJSON (Model a) where
    parseJSON = liftParser renderErrorMsg modelFromPair . parseJSON
