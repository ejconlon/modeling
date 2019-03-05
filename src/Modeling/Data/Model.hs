module Modeling.Data.Model where

import Data.Aeson
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics (Generic)
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

instance FromJSON ModelCon where
    parseJSON = injectionParseJSON renderErrorMsg modelConToText

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
    { direct :: Maybe (ModelDirectAttrs)
    , serial :: Maybe (ModelSerialAttrs a)
    , split :: Maybe (ModelSplitAttrs a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (ModelAttrs a)
instance FromJSON a => FromJSON (ModelAttrs a)

emptyModelAttrs :: ModelAttrs a
emptyModelAttrs = ModelAttrs Nothing Nothing Nothing

data ModelSum a = ModelSum
    { name :: ModelCon
    , attributes :: Maybe (ModelAttrs a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (ModelSum a)
instance FromJSON a => FromJSON (ModelSum a)

modelSumPairBijection :: Bijection (ModelSum a) (ModelCon, Maybe (ModelAttrs a))
modelSumPairBijection = Bijection apl inv where
    apl (ModelSum tn ma) = (tn, ma)
    inv (n, ma) = ModelSum n ma

modelSumSumInjection :: Injection ErrorMsg (ModelSum a) (Sum (ModelAttrs a))
modelSumSumInjection = domainInjection' modelConToText modelSumPairBijection

data Model a =
      DirectModel ModelDirectAttrs
    | SerialModel (ModelSerialAttrs a)
    | SplitModel (ModelSplitAttrs a)
    deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

modelPairInjection :: Injection ErrorMsg (Model a) (ModelCon, Maybe (ModelAttrs a))
modelPairInjection = Injection apl inv where
    apl t =
        case t of
            DirectModel attrs -> (DirectModelCon, Just (emptyModelAttrs { direct = Just attrs }))
            SerialModel attrs -> (SplitModelCon, Just (emptyModelAttrs { serial = Just attrs }))
            SplitModel attrs -> (SplitModelCon, Just (emptyModelAttrs { split = Just attrs }))
    inv (n, ma) = f ma where
        f = case n of
            DirectModelCon -> withAttrs direct DirectModel
            SerialModelCon -> withAttrs serial SerialModel
            SplitModelCon -> withAttrs split SplitModel

modelSumInjection :: Injection ErrorMsg a b -> Injection ErrorMsg (Model a) (ModelSum b)
modelSumInjection rinj = composeInjection (postTraverseInjection rinj (lowerBijection (flipBijection modelSumPairBijection))) modelPairInjection

-- TODO don't use injection composition for this so we can separate classes
instance (ToJSON a, FromJSON a) => ToJSON (Model a) where
    toJSON = injectionToJSON (modelSumInjection jsonInjection)

instance (ToJSON a, FromJSON a) => FromJSON (Model a) where
    parseJSON = injectionParseJSON renderErrorMsg (modelSumInjection jsonInjection)
