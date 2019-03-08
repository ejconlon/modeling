{-# LANGUAGE UndecidableInstances #-}

module Modeling.Data.Type where

import Control.Newtype.Generics (Newtype)
import Data.Aeson
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Modeling.Data.Aeson
import Modeling.Data.Bidi
import Modeling.Data.Common
import Modeling.Data.Util

data TypeCon =
      TypeConString
    | TypeConLong
    | TypeConDouble
    | TypeConBoolean
    | TypeConOptional
    | TypeConList
    | TypeConStringMap
    | TypeConStruct
    | TypeConReference
    | TypeConEnum
    | TypeConUnion
    | TypeConAny
    deriving (Generic, Eq, Show, Enum, Bounded)

typeConToText :: Injection ErrorMsg TypeCon Text
typeConToText = Injection apply invert where
    apply t =
        case t of
            TypeConString -> "string"
            TypeConLong -> "long"
            TypeConDouble -> "double"
            TypeConBoolean -> "boolean"
            TypeConOptional -> "optional"
            TypeConList -> "list"
            TypeConStringMap -> "stringmap"
            TypeConStruct -> "struct"
            TypeConReference -> "reference"
            TypeConEnum -> "enum"
            TypeConUnion -> "union"
            TypeConAny -> "any"
    invert u =
        case u of
            "string" -> Right TypeConString
            "long" -> Right TypeConLong
            "double" -> Right TypeConDouble
            "boolean" -> Right TypeConBoolean
            "optional" -> Right TypeConOptional
            "list" -> Right TypeConList
            "stringmap" -> Right TypeConStringMap
            "struct" -> Right TypeConStruct
            "reference" -> Right TypeConReference
            "enum" -> Right TypeConEnum
            "union" -> Right TypeConUnion
            "any" -> Right TypeConAny
            _ -> Left (ErrorMsg ("Unknown type name " <> u))

instance ToJSON TypeCon where
    toJSON = injectionToJSON typeConToText
    toEncoding = injectionToEncoding typeConToText

instance FromJSON TypeCon where
    parseJSON = injectionParseJSON renderErrorMsg typeConToText

data TypeSingleAttrs a = TypeSingleAttrs
    { ty :: a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance HasJSONOptions (TypeSingleAttrs a) where getJSONOptions _= recordOptions
deriving via (AesonWrapper (TypeSingleAttrs a)) instance ToJSON a => ToJSON (TypeSingleAttrs a)
deriving via (AesonWrapper (TypeSingleAttrs a)) instance FromJSON a => FromJSON (TypeSingleAttrs a)

data TypeReferenceAttrs = TypeReferenceAttrs
    { name :: Text
    } deriving (Generic, Show, Eq)
      deriving (ToJSON, FromJSON) via (AesonWrapper TypeReferenceAttrs)

instance HasJSONOptions TypeReferenceAttrs where getJSONOptions _= recordOptions

data TypeStructAttrs a = TypeStructAttrs
    { fields :: Map Text a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance HasJSONOptions (TypeStructAttrs a) where getJSONOptions _= recordOptions
deriving via (AesonWrapper (TypeStructAttrs a)) instance ToJSON a => ToJSON (TypeStructAttrs a)
deriving via (AesonWrapper (TypeStructAttrs a)) instance FromJSON a => FromJSON (TypeStructAttrs a)

data TypeEnumAttrs = TypeEnumAttrs
    { values :: Seq Text
    } deriving (Generic, Show, Eq)
      deriving (ToJSON, FromJSON) via (AesonWrapper TypeEnumAttrs)

instance HasJSONOptions TypeEnumAttrs where getJSONOptions _= recordOptions

data TypeUnionAttrs a = TypeUnionAttrs
    { elements :: Map Text a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance HasJSONOptions (TypeUnionAttrs a) where getJSONOptions _= recordOptions
deriving via (AesonWrapper (TypeUnionAttrs a)) instance ToJSON a => ToJSON (TypeUnionAttrs a)
deriving via (AesonWrapper (TypeUnionAttrs a)) instance FromJSON a => FromJSON (TypeUnionAttrs a)

data TypeAttrs a = TypeAttrs
    { optional :: Maybe (TypeSingleAttrs a)
    , list :: Maybe (TypeSingleAttrs a)
    , stringmap :: Maybe (TypeSingleAttrs a)
    , struct :: Maybe (TypeStructAttrs a)
    , reference :: Maybe TypeReferenceAttrs
    , enum :: Maybe TypeEnumAttrs
    , union :: Maybe (TypeUnionAttrs a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance HasJSONOptions (TypeAttrs a) where getJSONOptions _= recordOptions
deriving via (AesonWrapper (TypeAttrs a)) instance ToJSON a => ToJSON (TypeAttrs a)
deriving via (AesonWrapper (TypeAttrs a)) instance FromJSON a => FromJSON (TypeAttrs a)

emptyTypeAttrs :: TypeAttrs a
emptyTypeAttrs = TypeAttrs Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data TypeSum a = TypeSum
    { name :: TypeCon
    , attributes :: Maybe (TypeAttrs a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance HasJSONOptions (TypeSum a) where getJSONOptions _= recordOptions
deriving via (AesonWrapper (TypeSum a)) instance ToJSON a => ToJSON (TypeSum a)
deriving via (AesonWrapper (TypeSum a)) instance FromJSON a => FromJSON (TypeSum a)

newtype TypeSumFix = TypeSumFix { unTypeSumFix :: TypeSum TypeSumFix }
    deriving (Generic, Show, Eq)

instance Newtype TypeSumFix
deriving via (AesonNewtype TypeSumFix (TypeSum TypeSumFix)) instance ToJSON TypeSumFix
deriving via (AesonNewtype TypeSumFix (TypeSum TypeSumFix)) instance FromJSON TypeSumFix

data Type a =
    StringType
  | LongType
  | DoubleType
  | BooleanType
  | OptionalType (TypeSingleAttrs a)
  | ListType (TypeSingleAttrs a)
  | StringMapType (TypeSingleAttrs a)
  | StructType (TypeStructAttrs a)
  | ReferenceType TypeReferenceAttrs
  | EnumType TypeEnumAttrs
  | UnionType (TypeUnionAttrs a)
  | AnyType
  deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

typeToPair :: Type a -> TypeSum a
typeToPair t =
    case t of
        StringType -> TypeSum TypeConString Nothing
        LongType -> TypeSum TypeConLong Nothing
        DoubleType -> TypeSum TypeConDouble Nothing
        BooleanType -> TypeSum TypeConBoolean Nothing
        OptionalType attrs -> TypeSum TypeConOptional (Just (emptyTypeAttrs { optional = Just attrs }))
        ListType attrs -> TypeSum TypeConList (Just (emptyTypeAttrs { list = Just attrs }))
        StringMapType attrs -> TypeSum TypeConStringMap (Just (emptyTypeAttrs { stringmap = Just attrs }))
        StructType attrs -> TypeSum TypeConStruct (Just (emptyTypeAttrs { struct = Just attrs }))
        ReferenceType attrs -> TypeSum TypeConReference (Just (emptyTypeAttrs { reference = Just attrs }))
        EnumType attrs -> TypeSum TypeConEnum (Just (emptyTypeAttrs { enum = Just attrs }))
        UnionType attrs -> TypeSum TypeConUnion (Just (emptyTypeAttrs { union = Just attrs }))
        AnyType -> TypeSum TypeConAny Nothing

typeFromPair :: TypeSum a -> Either ErrorMsg (Type a)
typeFromPair (TypeSum n ma) = f ma where
    f = case n of
        TypeConString -> withoutAttrs StringType
        TypeConLong -> withoutAttrs LongType
        TypeConDouble -> withoutAttrs DoubleType
        TypeConBoolean -> withoutAttrs BooleanType
        TypeConOptional -> withAttrs optional OptionalType
        TypeConList -> withAttrs list ListType
        TypeConStringMap -> withAttrs stringmap StringMapType
        TypeConStruct -> withAttrs struct StructType
        TypeConReference -> withAttrs reference ReferenceType
        TypeConEnum-> withAttrs enum EnumType
        TypeConUnion -> withAttrs union UnionType
        TypeConAny -> withoutAttrs AnyType

instance ToJSON a => ToJSON (Type a) where
    toJSON = toJSON . typeToPair
    toEncoding = toEncoding . typeToPair

instance FromJSON a => FromJSON (Type a) where
    parseJSON = liftParser renderErrorMsg typeFromPair . parseJSON

newtype TypeFix = TypeFix { unTypeFix :: Type TypeFix }
    deriving (Generic, Show, Eq)

instance Newtype TypeFix
deriving via (AesonNewtype TypeFix (Type TypeFix)) instance ToJSON TypeFix
deriving via (AesonNewtype TypeFix (Type TypeFix)) instance FromJSON TypeFix
