module Modeling.Data.Mid where

import Data.Aeson
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Modeling.Data.Core
import Modeling.Util

-- TODO This is probably overkill
data MidTypeName =
      StringTypeName
    | LongTypeName
    | DoubleTypeName
    | OptionalTypeName
    | ListTypeName
    | StringMapTypeName
    | StructTypeName
    | ReferenceTypeName
    | EnumTypeName
    | UnionTypeName
    deriving (Generic, Eq, Show)

midTypeNameToText :: Injection ErrorMsg MidTypeName Text
midTypeNameToText = Injection apply invert where
    apply t =
        case t of
            StringTypeName -> "string"
            LongTypeName -> "long"
            DoubleTypeName -> "double"
            OptionalTypeName -> "optional"
            ListTypeName -> "list"
            StringMapTypeName -> "stringmap"
            StructTypeName -> "struct"
            ReferenceTypeName -> "reference"
            EnumTypeName -> "enum"
            UnionTypeName -> "union"
    invert u =
        case u of
            "string" -> Right StringTypeName
            "long" -> Right LongTypeName
            "double" -> Right DoubleTypeName
            "optional" -> Right OptionalTypeName
            "list" -> Right ListTypeName
            "stringmap" -> Right StringMapTypeName
            "struct" -> Right StructTypeName
            "reference" -> Right ReferenceTypeName
            "enum" -> Right EnumTypeName
            "union" -> Right UnionTypeName
            _ -> Left (ErrorMsg ("Unknown type name " <> u))

-- instance ToJSON MidTypeName where
--     toJSON = injectionToJSON midTypeNameToText

-- instance FromJSON MidTypeName where
--     parseJSON = injectionParseJSON midTypeNameToText

data MidTypeSingleAttrs = MidTypeSingleAttrs
    { ty :: MidType
    } deriving (Generic, Show, Eq)

instance ToJSON MidTypeSingleAttrs
instance FromJSON MidTypeSingleAttrs

data MidTypeReferenceAttrs = MidTypeReferenceAttrs
    { name :: Text
    } deriving (Generic, Show, Eq)

instance ToJSON MidTypeReferenceAttrs
instance FromJSON MidTypeReferenceAttrs

data MidTypeStructAttrs = MidTypeStructAttrs
    { fields :: Map Text MidType
    } deriving (Generic, Show, Eq)

instance ToJSON MidTypeStructAttrs
instance FromJSON MidTypeStructAttrs

data MidTypeEnumAttrs = MidTypeEnumAttrs
    { values :: Seq Text
    } deriving (Generic, Show, Eq)

instance ToJSON MidTypeEnumAttrs
instance FromJSON MidTypeEnumAttrs

data MidTypeUnionAttrs = MidTypeUnionAttrs
    { elements :: Map Text MidType
    } deriving (Generic, Show, Eq)

instance ToJSON MidTypeUnionAttrs
instance FromJSON MidTypeUnionAttrs

data MidTypeAttrs = MidTypeAttrs
    { optional :: Maybe MidTypeSingleAttrs
    , list :: Maybe MidTypeSingleAttrs
    , stringmap :: Maybe MidTypeSingleAttrs
    , struct :: Maybe (Map Text MidType)
    , reference :: Maybe MidTypeReferenceAttrs
    , enum :: Maybe MidTypeEnumAttrs
    , union :: Maybe MidTypeUnionAttrs
    } deriving (Generic, Show, Eq)

instance ToJSON MidTypeAttrs
instance FromJSON MidTypeAttrs

data MidType = MidType
    { name :: Text
    , attributes :: Maybe MidTypeAttrs
    } deriving (Generic, Show, Eq)

instance ToJSON MidType
instance FromJSON MidType

data MidModelName =
      DirectModelName
    | SerialModelName
    | ParallelModelName
    | AdaptorModelName
    | SplitModelName
    deriving (Generic, Show, Eq)

midModelNameToText :: Injection ErrorMsg MidModelName Text
midModelNameToText = Injection apply invert where
    apply t =
        case t of
            DirectModelName -> "direct"
            SerialModelName -> "serial"
            ParallelModelName -> "parallel"
            AdaptorModelName -> "adaptor"
            SplitModelName -> "split"
    invert u =
        case u of
            "direct" -> Right DirectModelName
            "serial" -> Right SerialModelName
            "parallel" -> Right ParallelModelName
            "adaptor" -> Right AdaptorModelName
            "split" -> Right SplitModelName
            _ -> Left (ErrorMsg ("Unknown model name " <> u))

data MidModelDirectAttrs = MidModelDirectAttrs
    {
    } deriving (Generic, Show, Eq)

instance ToJSON MidModelDirectAttrs
instance FromJSON MidModelDirectAttrs

data MidModelAttrs = MidModelAttrs
    { direct :: MidModelDirectAttrs
    } deriving (Generic, Show, Eq)

instance ToJSON MidModelAttrs
instance FromJSON MidModelAttrs

data MidModel = MidModel
    { name :: Text
    , attributes :: Maybe MidModelAttrs
    } deriving (Generic, Show, Eq)

instance ToJSON MidModel
instance FromJSON MidModel

data MidModelSpace = MidModelSpace
    { nspart :: NamespacePart
    , model :: MidModel
    } deriving (Generic, Show, Eq)

instance ToJSON MidModelSpace
instance FromJSON MidModelSpace
