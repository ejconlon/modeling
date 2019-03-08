module Modeling.Data.Param where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Modeling.Data.Aeson
import Modeling.Data.Bidi
import Modeling.Data.Common
import Modeling.Data.Util

data ParamCon =
      ParamConLiteral
    | ParamConExternal
    | ParamConInternal
    deriving (Generic, Eq, Show, Enum, Bounded)

paramConToText :: Injection ErrorMsg ParamCon Text
paramConToText = Injection apply invert where
    apply t =
        case t of
            ParamConLiteral -> "literal"
            ParamConExternal -> "external"
            ParamConInternal -> "internal"
    invert u =
        case u of
            "literal" -> Right ParamConLiteral
            "external" -> Right ParamConExternal
            "internal" -> Right ParamConInternal
            _ -> Left (ErrorMsg ("Unknown param con " <> u))

instance ToJSON ParamCon where
    toJSON = injectionToJSON paramConToText
    toEncoding = injectionToEncoding paramConToText

instance FromJSON ParamCon where
    parseJSON = injectionParseJSON renderErrorMsg paramConToText

data LiteralParamAttrs = LiteralParamAttrs
    { value :: Value
    } deriving (Generic, Eq, Show)
      deriving (ToJSON, FromJSON) via (AesonWrapper LiteralParamAttrs)

instance HasJSONOptions LiteralParamAttrs where getJSONOptions _= recordOptions

data ExternalParamAttrs = ExternalParamAttrs
    { ns :: Namespace
    , name :: ParamName
    } deriving (Generic, Eq, Show)
      deriving (ToJSON, FromJSON) via (AesonWrapper ExternalParamAttrs)

instance HasJSONOptions ExternalParamAttrs where getJSONOptions _= recordOptions

data InternalParamAttrs = InternalParamAttrs
    { index :: Int
    } deriving (Generic, Eq, Show)
      deriving (ToJSON, FromJSON) via (AesonWrapper InternalParamAttrs)

instance HasJSONOptions InternalParamAttrs where getJSONOptions _= recordOptions

data ParamAttrs = ParamAttrs
    { literal :: Maybe LiteralParamAttrs
    , external :: Maybe ExternalParamAttrs
    , internal :: Maybe InternalParamAttrs
    } deriving (Generic, Eq, Show)
      deriving (ToJSON, FromJSON) via (AesonWrapper ParamAttrs)

instance HasJSONOptions ParamAttrs where getJSONOptions _= recordOptions

emptyParamAttrs :: ParamAttrs
emptyParamAttrs = ParamAttrs Nothing Nothing Nothing

data ParamSum = ParamSum
    { name :: ParamCon
    , attributes :: Maybe ParamAttrs
    } deriving (Generic, Eq, Show)
      deriving (ToJSON, FromJSON) via (AesonWrapper ParamSum)

instance HasJSONOptions ParamSum where getJSONOptions _= recordOptions

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
            LiteralParam attrs -> (ParamConLiteral, Just (emptyParamAttrs { literal = Just attrs }))
            ExternalParam attrs -> (ParamConExternal, Just (emptyParamAttrs { external = Just attrs }))
            InternalParam attrs -> (ParamConInternal, Just (emptyParamAttrs { internal = Just attrs }))
    inv (n, ma) = f ma where
        f = case n of
            ParamConLiteral -> withAttrs literal LiteralParam
            ParamConExternal -> withAttrs external ExternalParam
            ParamConInternal -> withAttrs internal InternalParam

paramInjection :: Injection ErrorMsg Param ParamSum
paramInjection = composeLeft (flipBijection paramSumPairBijection) paramPairInjection

instance ToJSON Param where
    toJSON = injectionToJSON paramInjection
    toEncoding = injectionToEncoding paramInjection

instance FromJSON Param where
    parseJSON = injectionParseJSON renderErrorMsg paramInjection
