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
import Modeling.Data.Generics
import Modeling.Data.Error
import Modeling.Data.JsonRep
import Modeling.Data.Util

newtype ModelName = ModelName { unModelName :: Text }
    deriving (Generic, Show, Eq, Ord, IsString, ToJSONKey, FromJSONKey)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonNewtype ModelName Text)
    deriving (HasGenRep JsonRep) via (GenRepNewtype ModelName Text)

instance Newtype ModelName

data ModelCon =
      ModelConDirect
    | ModelConSerial
    | ModelConParallel
    | ModelConAdaptor
    | ModelConSplit
    deriving (Generic, Show, Eq, Enum, Bounded)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonTag ModelCon)
    deriving (HasGenRep JsonRep) via (GenRepTag ModelCon)

instance HasTagPrefix ModelCon where getTagPrefix _ = "ModelCon"

data Dependencies a = Dependencies
    { named :: Maybe (Map ElementName a)
    , additional :: Maybe (Seq a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
      deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonRecord (Dependencies a))

instance HasGenRep JsonRep a => HasGenRep JsonRep (Dependencies a)

data ModelDirectAttrs a = ModelDirectAttrs
    { name :: ModelName
    , dependencies :: Maybe (Dependencies a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
      deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonRecord (ModelDirectAttrs a))

instance HasGenRep JsonRep a => HasGenRep JsonRep (ModelDirectAttrs a)

data ModelSerialAttrs a = ModelSerialAttrs
    { models :: Seq a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
      deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonRecord (ModelSerialAttrs a))

instance HasGenRep JsonRep a => HasGenRep JsonRep (ModelSerialAttrs a)

data ModelSplitAttrs a = ModelSplitAttrs
    { attribute :: Text
    , values :: Seq Text
    , other :: Maybe Text
    , model :: a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
      deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonRecord (ModelSplitAttrs a))

instance HasGenRep JsonRep a => HasGenRep JsonRep (ModelSplitAttrs a)

data ModelAttrs a = ModelAttrs
    { direct :: Maybe (ModelDirectAttrs a)
    , serial :: Maybe (ModelSerialAttrs a)
    , split :: Maybe (ModelSplitAttrs a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
      deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonRecord (ModelAttrs a))

instance HasGenRep JsonRep a => HasGenRep JsonRep (ModelAttrs a)

emptyModelAttrs :: ModelAttrs a
emptyModelAttrs = ModelAttrs Nothing Nothing Nothing

data ModelSum a = ModelSum
    { name :: ModelCon
    , attributes :: Maybe (ModelAttrs a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
      deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonRecord (ModelSum a))

instance HasGenRep JsonRep a => HasGenRep JsonRep (ModelSum a)

data Model a =
      DirectModel (ModelDirectAttrs a)
    | SerialModel (ModelSerialAttrs a)
    | SplitModel (ModelSplitAttrs a)
    deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonInjection (Model a) (ModelSum a))

deriving via GenRepInjection (Model a) (ModelSum a) instance HasGenRep JsonRep a => HasGenRep JsonRep (Model a)

instance Injection (Model a) where
    type InjTarget (Model a) = ModelSum a

    injApply t =
        case t of
            DirectModel attrs -> ModelSum ModelConDirect (Just (emptyModelAttrs { direct = Just attrs }))
            SerialModel attrs -> ModelSum ModelConSplit (Just (emptyModelAttrs { serial = Just attrs }))
            SplitModel attrs -> ModelSum ModelConSplit (Just (emptyModelAttrs { split = Just attrs }))

    injInvert (ModelSum n ma) = f ma where
        f = case n of
            ModelConDirect -> withAttrs direct DirectModel
            ModelConSerial -> withAttrs serial SerialModel
            ModelConSplit -> withAttrs split SplitModel
