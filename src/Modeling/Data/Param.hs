module Modeling.Data.Param where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Modeling.Data.Aeson
import Modeling.Data.Common
import Modeling.Data.Generics
import Modeling.Data.Error
import Modeling.Data.JsonRep
import Modeling.Data.Util

data ParamCon =
      ParamConLiteral
    | ParamConExternal
    | ParamConInternal
    deriving (Generic, Eq, Show, Enum, Bounded)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonTag ParamCon)
    deriving (HasGenRep JsonRep) via (GenRepTag ParamCon)

instance HasTagPrefix ParamCon where getTagPrefix _ = "ParamCon"

data LiteralParamAttrs = LiteralParamAttrs
    { value :: Value
    } deriving (Generic, Eq, Show)
      deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonRecord LiteralParamAttrs)

instance HasGenRep JsonRep LiteralParamAttrs

data ExternalParamAttrs = ExternalParamAttrs
    { ns :: Namespace
    , name :: ParamName
    } deriving (Generic, Eq, Show)
      deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonRecord ExternalParamAttrs)

instance HasGenRep JsonRep ExternalParamAttrs

data InternalParamAttrs = InternalParamAttrs
    { index :: Int
    } deriving (Generic, Eq, Show)
      deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonRecord InternalParamAttrs)

instance HasGenRep JsonRep InternalParamAttrs

data ParamAttrs = ParamAttrs
    { literal :: Maybe LiteralParamAttrs
    , external :: Maybe ExternalParamAttrs
    , internal :: Maybe InternalParamAttrs
    } deriving (Generic, Eq, Show)
      deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonRecord ParamAttrs)

instance HasGenRep JsonRep ParamAttrs

emptyParamAttrs :: ParamAttrs
emptyParamAttrs = ParamAttrs Nothing Nothing Nothing

data ParamSum = ParamSum
    { name :: ParamCon
    , attributes :: Maybe ParamAttrs
    } deriving (Generic, Eq, Show)
      deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonRecord ParamSum)

instance HasGenRep JsonRep ParamSum

data Param =
      LiteralParam LiteralParamAttrs
    | ExternalParam ExternalParamAttrs
    | InternalParam InternalParamAttrs
    deriving (Generic, Eq, Show)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonInjection Param ParamSum)
    deriving (HasGenRep JsonRep) via (GenRepInjection Param ParamSum)

instance Injection Param where
    type InjTarget Param = ParamSum

    injApply t =
        case t of
            LiteralParam attrs -> ParamSum ParamConLiteral (Just (emptyParamAttrs { literal = Just attrs }))
            ExternalParam attrs -> ParamSum ParamConExternal (Just (emptyParamAttrs { external = Just attrs }))
            InternalParam attrs -> ParamSum ParamConInternal (Just (emptyParamAttrs { internal = Just attrs }))

    injInvert (ParamSum n ma) = f ma where
        f = case n of
            ParamConLiteral -> withAttrs literal LiteralParam
            ParamConExternal -> withAttrs external ExternalParam
            ParamConInternal -> withAttrs internal InternalParam
