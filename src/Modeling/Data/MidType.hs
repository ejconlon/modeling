module Modeling.Data.MidType where

import Data.Aeson
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Modeling.Data.Common
import Modeling.Data.Util

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

instance ToJSON MidTypeName where
    toJSON = injectionToJSON midTypeNameToText

instance FromJSON MidTypeName where
    parseJSON = injectionParseJSON renderErrorMsg midTypeNameToText

data MidTypeSingleAttrs a = MidTypeSingleAttrs
    { ty :: a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (MidTypeSingleAttrs a)
instance FromJSON a => FromJSON (MidTypeSingleAttrs a)

data MidTypeReferenceAttrs = MidTypeReferenceAttrs
    { name :: Text
    } deriving (Generic, Show, Eq)

instance ToJSON MidTypeReferenceAttrs
instance FromJSON MidTypeReferenceAttrs

data MidTypeStructAttrs a = MidTypeStructAttrs
    { fields :: Map Text a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (MidTypeStructAttrs a)
instance FromJSON a => FromJSON (MidTypeStructAttrs a)

data MidTypeEnumAttrs = MidTypeEnumAttrs
    { values :: Seq Text
    } deriving (Generic, Show, Eq)

instance ToJSON MidTypeEnumAttrs
instance FromJSON MidTypeEnumAttrs

data MidTypeUnionAttrs a = MidTypeUnionAttrs
    { elements :: Map Text a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (MidTypeUnionAttrs a)
instance FromJSON a => FromJSON (MidTypeUnionAttrs a)

data MidTypeAttrs a = MidTypeAttrs
    { optional :: Maybe (MidTypeSingleAttrs a)
    , list :: Maybe (MidTypeSingleAttrs a)
    , stringmap :: Maybe (MidTypeSingleAttrs a)
    , struct :: Maybe (MidTypeStructAttrs a)
    , reference :: Maybe MidTypeReferenceAttrs
    , enum :: Maybe MidTypeEnumAttrs
    , union :: Maybe (MidTypeUnionAttrs a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

emptyMidTypeAttrs :: MidTypeAttrs a
emptyMidTypeAttrs = MidTypeAttrs Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON a => ToJSON (MidTypeAttrs a)
instance FromJSON a => FromJSON (MidTypeAttrs a)

data MidType = MidType
    { name :: MidTypeName
    , attributes :: Maybe (MidTypeAttrs MidType)
    } deriving (Generic, Show, Eq)

instance ToJSON MidType
instance FromJSON MidType
