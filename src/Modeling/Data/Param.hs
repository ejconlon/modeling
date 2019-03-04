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

data ParamSum = ParamSum
    { name :: ParamCon
    , attributes :: Maybe ParamAttrs
    } deriving (Generic, Eq, Show)

instance ToJSON ParamSum
instance FromJSON ParamSum

data Param =
      LiteralParam LiteralParamAttrs
    | ExternalParam ExternalParamAttrs
    | InternalParam InternalParamAttrs
    deriving (Generic, Eq, Show)

-- TODO use ParamSum serde for this
instance ToJSON Param where
    toJSON = undefined

instance FromJSON Param where
    parseJSON = undefined