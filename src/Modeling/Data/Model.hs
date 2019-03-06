module Modeling.Data.Model where

import Data.Aeson
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
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

-- modelSumPairBijection :: Bijection (ModelSum a) (ModelCon, Maybe (ModelAttrs a))
-- modelSumPairBijection = Bijection apl inv where
--     apl (ModelSum tn ma) = (tn, ma)
--     inv (n, ma) = ModelSum n ma

-- modelSumSumInjection :: Injection ErrorMsg (ModelSum a) (Sum (ModelAttrs a))
-- modelSumSumInjection = domainInjection' modelConToText modelSumPairBijection

data Model a =
      DirectModel ModelDirectAttrs
    | SerialModel (ModelSerialAttrs a)
    | SplitModel (ModelSplitAttrs a)
    deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

-- modelPairInjection :: Injection ErrorMsg (Model a) (ModelCon, Maybe (ModelAttrs a))
-- modelPairInjection = Injection apl inv where
--     apl t =
--         case t of
--             DirectModel attrs -> (DirectModelCon, Just (emptyModelAttrs { direct = Just attrs }))
--             SerialModel attrs -> (SplitModelCon, Just (emptyModelAttrs { serial = Just attrs }))
--             SplitModel attrs -> (SplitModelCon, Just (emptyModelAttrs { split = Just attrs }))
--     inv (n, ma) = f ma where
--         f = case n of
--             DirectModelCon -> withAttrs direct DirectModel
--             SerialModelCon -> withAttrs serial SerialModel
--             SplitModelCon -> withAttrs split SplitModel

-- modelSumInjection :: Injection ErrorMsg a b -> Injection ErrorMsg (Model a) (ModelSum b)
-- modelSumInjection rinj = composeInjection (postTraverseInjection rinj (lowerBijection (flipBijection modelSumPairBijection))) modelPairInjection

--  liftToJSON :: (a -> Value) -> ([a] -> Value) -> f a -> Value

-- modelToPair :: Model a -> (ModelCon, Maybe (ModelAttrs a))
-- modelToPair t =
--     case t of
--         DirectModel attrs -> (DirectModelCon, Just (emptyModelAttrs { direct = Just attrs }))
--         SerialModel attrs -> (SplitModelCon, Just (emptyModelAttrs { serial = Just attrs }))
--         SplitModel attrs -> (SplitModelCon, Just (emptyModelAttrs { split = Just attrs }))

-- pairToRawSum :: ModelCon -> Maybe (ModelAttrs a) -> RawSum (ModelAttrs a)
-- pairToRawSum c ma = RawSum ((injApply modelConToText) c) ma

-- modelToRawSum :: Model a -> RawSum (ModelAttrs a)
-- modelToRawSum = uncurry pairToRawSum . modelToPair

-- TODO don't use injection composition for this so we can separate classes
instance ToJSON1 Model where
    liftToJSON = undefined
    liftToEncoding = undefined

instance FromJSON1 Model where
    liftParseJSON = undefined