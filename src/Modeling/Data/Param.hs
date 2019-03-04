module Modeling.Data.Param where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Modeling.Data.Bidi
import Modeling.Data.Common
import Modeling.Data.Util

data ParamCon =
      LiteralParamCon
    | ExternalParamCon
    | InternalParamCon
    deriving (Generic, Eq, Show)

paramConToText :: Injection ErrorMsg ParamCon Text
paramConToText = Injection apply invert where
    apply t =
        case t of
            LiteralParamCon -> "literal"
            ExternalParamCon -> "external"
            InternalParamCon -> "internal"
    invert u =
        case u of
            "literal" -> Right LiteralParamCon
            "external" -> Right ExternalParamCon
            "internal" -> Right InternalParamCon
            _ -> Left (ErrorMsg ("Unknown param con " <> u))

instance ToJSON ParamCon where
    toJSON = injectionToJSON paramConToText

instance FromJSON ParamCon where
    parseJSON = injectionParseJSON renderErrorMsg paramConToText

data LiteralParamAttrs = LiteralParamAttrs
    { value :: Value
    } deriving (Generic, Eq, Show)

instance ToJSON LiteralParamAttrs
instance FromJSON LiteralParamAttrs

data ExternalParamAttrs = ExternalParamAttrs
    { ns :: Namespace
    , name :: ParamName
    } deriving (Generic, Eq, Show)

instance ToJSON ExternalParamAttrs
instance FromJSON ExternalParamAttrs

data InternalParamAttrs = InternalParamAttrs
    { index :: Int
    } deriving (Generic, Eq, Show)

instance ToJSON InternalParamAttrs
instance FromJSON InternalParamAttrs

data ParamAttrs = ParamAttrs
    { literal :: Maybe LiteralParamAttrs
    , external :: Maybe ExternalParamAttrs
    , internal :: Maybe InternalParamAttrs
    } deriving (Generic, Eq, Show)

instance ToJSON ParamAttrs
instance FromJSON ParamAttrs

emptyParamAttrs :: ParamAttrs
emptyParamAttrs = ParamAttrs Nothing Nothing Nothing

data ParamSum = ParamSum
    { name :: ParamCon
    , attributes :: Maybe ParamAttrs
    } deriving (Generic, Eq, Show)

instance ToJSON ParamSum
instance FromJSON ParamSum

paramSumPairBijection :: Bijection ParamSum (ParamCon, Maybe ParamAttrs)
paramSumPairBijection = Bijection apl inv where
    apl (ParamSum tn ma) = (tn, ma)
    inv (n, ma) = ParamSum n ma

paramSumSumInjection :: Injection ErrorMsg ParamSum (Sum ParamAttrs)
paramSumSumInjection = domainInjection' paramConToText paramSumPairBijection

data Param =
      LiteralParam LiteralParamAttrs
    | ExternalParam ExternalParamAttrs
    | InternalParam InternalParamAttrs
    deriving (Generic, Eq, Show)

paramPairInjection :: Injection ErrorMsg Param (ParamCon, Maybe ParamAttrs)
paramPairInjection = Injection apl inv where
    apl t =
        case t of
            LiteralParam attrs -> (LiteralParamCon, Just (emptyParamAttrs { literal = Just attrs }))
            ExternalParam attrs -> (ExternalParamCon, Just (emptyParamAttrs { external = Just attrs }))
            InternalParam attrs -> (InternalParamCon, Just (emptyParamAttrs { internal = Just attrs }))
    inv (n, ma) = f ma where
        f = case n of
            LiteralParamCon -> withAttrs literal LiteralParam
            ExternalParamCon -> withAttrs external ExternalParam
            InternalParamCon -> withAttrs internal InternalParam

paramInjection :: Injection ErrorMsg Param ParamSum
paramInjection = composeLeft (flipBijection paramSumPairBijection) paramPairInjection

instance ToJSON Param where
    toJSON = injectionToJSON paramInjection

instance FromJSON Param where
    parseJSON = injectionParseJSON renderErrorMsg paramInjection
