module Modeling.Data.Model where

import Data.Aeson
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics (Generic)
import Modeling.Data.Aeson
import Modeling.Data.Common
import Modeling.Data.Util

data ModelCon =
      ModelConDirect
    | ModelConSerial
    | ModelConParallel
    | ModelConAdaptor
    | ModelConSplit
    deriving (Generic, Show, Eq, Enum, Bounded)
    deriving (ToJSON, FromJSON) via (AesonWrapper ModelCon)

instance HasJSONOptions ModelCon where getJSONOptions _ = tagOptions "ModelCon"

data Dependencies a = Dependencies
    { named :: Maybe (Map ElementName a)
    , additional :: Maybe (Seq a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance HasJSONOptions (Dependencies a) where getJSONOptions _= recordOptions
deriving via (AesonWrapper (Dependencies a)) instance ToJSON a => ToJSON (Dependencies a)
deriving via (AesonWrapper (Dependencies a)) instance FromJSON a => FromJSON (Dependencies a)

data ModelDirectAttrs a = ModelDirectAttrs
    { name :: Text
    , dependencies :: Maybe (Dependencies a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance HasJSONOptions (ModelDirectAttrs a) where getJSONOptions _= recordOptions
deriving via (AesonWrapper (ModelDirectAttrs a)) instance ToJSON a => ToJSON (ModelDirectAttrs a)
deriving via (AesonWrapper (ModelDirectAttrs a)) instance FromJSON a => FromJSON (ModelDirectAttrs a)

data ModelSerialAttrs a = ModelSerialAttrs
    { models :: Seq a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance HasJSONOptions (ModelSerialAttrs a) where getJSONOptions _= recordOptions
deriving via (AesonWrapper (ModelSerialAttrs a)) instance ToJSON a => ToJSON (ModelSerialAttrs a)
deriving via (AesonWrapper (ModelSerialAttrs a)) instance FromJSON a => FromJSON (ModelSerialAttrs a)

data ModelSplitAttrs a = ModelSplitAttrs
    { attribute :: Text
    , values :: Seq Text
    , other :: Maybe Text
    , model :: a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance HasJSONOptions (ModelSplitAttrs a) where getJSONOptions _= recordOptions
deriving via (AesonWrapper (ModelSplitAttrs a)) instance ToJSON a => ToJSON (ModelSplitAttrs a)
deriving via (AesonWrapper (ModelSplitAttrs a)) instance FromJSON a => FromJSON (ModelSplitAttrs a)

data ModelAttrs a = ModelAttrs
    { direct :: Maybe (ModelDirectAttrs a)
    , serial :: Maybe (ModelSerialAttrs a)
    , split :: Maybe (ModelSplitAttrs a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance HasJSONOptions (ModelAttrs a) where getJSONOptions _= recordOptions
deriving via (AesonWrapper (ModelAttrs a)) instance ToJSON a => ToJSON (ModelAttrs a)
deriving via (AesonWrapper (ModelAttrs a)) instance FromJSON a => FromJSON (ModelAttrs a)

emptyModelAttrs :: ModelAttrs a
emptyModelAttrs = ModelAttrs Nothing Nothing Nothing

data ModelSum a = ModelSum
    { name :: ModelCon
    , attributes :: Maybe (ModelAttrs a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance HasJSONOptions (ModelSum a) where getJSONOptions _= recordOptions
deriving via (AesonWrapper (ModelSum a)) instance ToJSON a => ToJSON (ModelSum a)
deriving via (AesonWrapper (ModelSum a)) instance FromJSON a => FromJSON (ModelSum a)

data Model a =
      DirectModel (ModelDirectAttrs a)
    | SerialModel (ModelSerialAttrs a)
    | SplitModel (ModelSplitAttrs a)
    deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

modelToPair :: Model a -> ModelSum a
modelToPair t =
    case t of
        DirectModel attrs -> ModelSum ModelConDirect (Just (emptyModelAttrs { direct = Just attrs }))
        SerialModel attrs -> ModelSum ModelConSplit (Just (emptyModelAttrs { serial = Just attrs }))
        SplitModel attrs -> ModelSum ModelConSplit (Just (emptyModelAttrs { split = Just attrs }))

modelFromPair :: ModelSum a -> Either ErrorMsg (Model a)
modelFromPair (ModelSum n ma) = f ma where
    f = case n of
        ModelConDirect -> withAttrs direct DirectModel
        ModelConSerial -> withAttrs serial SerialModel
        ModelConSplit -> withAttrs split SplitModel

instance ToJSON a => ToJSON (Model a) where
    toJSON = toJSON . modelToPair
    toEncoding = toEncoding . modelToPair

instance FromJSON a => FromJSON (Model a) where
    parseJSON = liftParser renderErrorMsg modelFromPair . parseJSON
