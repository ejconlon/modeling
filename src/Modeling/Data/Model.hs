module Modeling.Data.Model where

import Data.Aeson
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (fromList)
import GHC.Generics (Generic, Generic1)
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
    } deriving (Generic1, Show, Eq, Functor, Foldable, Traversable)
      deriving (ToJSON1, FromJSON1) via (AesonWrapper1 ModelSerialAttrs)

data ModelSplitAttrs a = ModelSplitAttrs
    { attribute :: Text
    , values :: Seq Text
    , other :: Maybe Text
    , model :: a
    } deriving (Generic1, Show, Eq, Functor, Foldable, Traversable)
      deriving (ToJSON1, FromJSON1) via (AesonWrapper1 ModelSplitAttrs)

data ModelAttrs a = ModelAttrs
    { direct :: Maybe (ModelDirectAttrs)
    , serial :: Maybe (ModelSerialAttrs a)
    , split :: Maybe (ModelSplitAttrs a)
    } deriving (Generic1, Show, Eq, Functor, Foldable, Traversable)
      deriving (ToJSON1, FromJSON1) via (AesonWrapper1 ModelAttrs)

emptyModelAttrs :: ModelAttrs a
emptyModelAttrs = ModelAttrs Nothing Nothing Nothing

data ModelSum a = ModelSum
    { name :: ModelCon
    , attributes :: Maybe (ModelAttrs a)
    } deriving (Generic1, Show, Eq, Functor, Foldable, Traversable)
      deriving (ToJSON1, FromJSON1) via (AesonWrapper1 ModelSum)

data Model a =
      DirectModel ModelDirectAttrs
    | SerialModel (ModelSerialAttrs a)
    | SplitModel (ModelSplitAttrs a)
    deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

newtype ModelRawSum a = ModelRawSum { unModelRawSum :: RawSum (ModelAttrs a) }
        deriving (ToJSON1, FromJSON1) via (AesonWrapperComp RawSum ModelAttrs)

modelToPair :: Model a -> (ModelCon, Maybe (ModelAttrs a))
modelToPair t =
    case t of
        DirectModel attrs -> (DirectModelCon, Just (emptyModelAttrs { direct = Just attrs }))
        SerialModel attrs -> (SplitModelCon, Just (emptyModelAttrs { serial = Just attrs }))
        SplitModel attrs -> (SplitModelCon, Just (emptyModelAttrs { split = Just attrs }))

pairToRawSum :: ModelCon -> Maybe (ModelAttrs a) -> RawSum (ModelAttrs a)
pairToRawSum c ma = RawSum ((injApply modelConToText) c) ma

modelToRawSum :: Model a -> ModelRawSum a
modelToRawSum = ModelRawSum . uncurry pairToRawSum . modelToPair

modelFromPair :: (ModelCon, Maybe (ModelAttrs a)) -> Either ErrorMsg (Model a)
modelFromPair (n, ma) = f ma where
    f = case n of
        DirectModelCon -> withAttrs direct DirectModel
        SerialModelCon -> withAttrs serial SerialModel
        SplitModelCon -> withAttrs split SplitModel

pairFromRawSum :: RawSum (ModelAttrs a) -> Either ErrorMsg (ModelCon, Maybe (ModelAttrs a))
pairFromRawSum (RawSum t ma) = (\n -> (n, ma)) <$> (injInvert modelConToText) t

modelFromRawSum :: ModelRawSum a -> Either ErrorMsg (Model a)
modelFromRawSum rs = pairFromRawSum (unModelRawSum rs) >>= modelFromPair

instance ToJSON1 Model where
    liftToJSON tv tvl = liftToJSON tv tvl . modelToRawSum
    liftToEncoding tv tvl = liftToEncoding tv tvl . modelToRawSum

instance FromJSON1 Model where
    liftParseJSON tv tvl = liftParser renderErrorMsg modelFromRawSum . liftParseJSON tv tvl
