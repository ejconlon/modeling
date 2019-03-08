module Modeling.Data.Model where

import Control.Newtype.Generics
import Data.Aeson
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Modeling.Data.Aeson
import Modeling.Data.Common
import Modeling.Data.Util

newtype ModelName = ModelName { unModelName :: Text }
    deriving (Generic, Show, Eq, Ord, IsString, ToJSONKey, FromJSONKey)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonNewtype ModelName Text)

instance Newtype ModelName

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
      deriving (ToJSON, FromJSON) via (AesonWrapper (Dependencies a))

instance HasJSONOptions (Dependencies a) where getJSONOptions _= recordOptions

data ModelDirectAttrs a = ModelDirectAttrs
    { name :: ModelName
    , dependencies :: Maybe (Dependencies a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
      deriving (ToJSON, FromJSON) via (AesonWrapper (ModelDirectAttrs a))

instance HasJSONOptions (ModelDirectAttrs a) where getJSONOptions _= recordOptions

data ModelSerialAttrs a = ModelSerialAttrs
    { models :: Seq a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
      deriving (ToJSON, FromJSON) via (AesonWrapper (ModelSerialAttrs a))

instance HasJSONOptions (ModelSerialAttrs a) where getJSONOptions _= recordOptions

data ModelSplitAttrs a = ModelSplitAttrs
    { attribute :: Text
    , values :: Seq Text
    , other :: Maybe Text
    , model :: a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
      deriving (ToJSON, FromJSON) via (AesonWrapper (ModelSplitAttrs a))

instance HasJSONOptions (ModelSplitAttrs a) where getJSONOptions _= recordOptions

data ModelAttrs a = ModelAttrs
    { direct :: Maybe (ModelDirectAttrs a)
    , serial :: Maybe (ModelSerialAttrs a)
    , split :: Maybe (ModelSplitAttrs a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
      deriving (ToJSON, FromJSON) via (AesonWrapper (ModelAttrs a))

instance HasJSONOptions (ModelAttrs a) where getJSONOptions _= recordOptions

emptyModelAttrs :: ModelAttrs a
emptyModelAttrs = ModelAttrs Nothing Nothing Nothing

data ModelSum a = ModelSum
    { name :: ModelCon
    , attributes :: Maybe (ModelAttrs a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
      deriving (ToJSON, FromJSON) via (AesonWrapper (ModelSum a))

instance HasJSONOptions (ModelSum a) where getJSONOptions _= recordOptions

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
