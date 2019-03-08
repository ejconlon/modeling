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
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonTag TypeCon)

instance HasTagPrefix TypeCon where getTagPrefix _ = "TypeCon"

data TypeSingleAttrs a = TypeSingleAttrs
    { ty :: a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
      deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonRecord (TypeSingleAttrs a))

data TypeReferenceAttrs = TypeReferenceAttrs
    { name :: TypeName
    } deriving (Generic, Show, Eq)
      deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonRecord TypeReferenceAttrs)

data TypeStructAttrs a = TypeStructAttrs
    { fields :: Map FieldName a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
      deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonRecord (TypeStructAttrs a))

data TypeEnumAttrs = TypeEnumAttrs
    { values :: Seq EnumName
    } deriving (Generic, Show, Eq)
      deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonRecord TypeEnumAttrs)

data TypeUnionAttrs a = TypeUnionAttrs
    { elements :: Map BranchName a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
      deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonRecord (TypeUnionAttrs a))

data TypeAttrs a = TypeAttrs
    { optional :: Maybe (TypeSingleAttrs a)
    , list :: Maybe (TypeSingleAttrs a)
    , stringmap :: Maybe (TypeSingleAttrs a)
    , struct :: Maybe (TypeStructAttrs a)
    , reference :: Maybe TypeReferenceAttrs
    , enum :: Maybe TypeEnumAttrs
    , union :: Maybe (TypeUnionAttrs a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
      deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonRecord (TypeAttrs a))

emptyTypeAttrs :: TypeAttrs a
emptyTypeAttrs = TypeAttrs Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data TypeSum a = TypeSum
    { name :: TypeCon
    , attributes :: Maybe (TypeAttrs a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
      deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonRecord (TypeSum a))

newtype TypeSumFix = TypeSumFix { unTypeSumFix :: TypeSum TypeSumFix }
    deriving (Generic, Show, Eq)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonNewtype TypeSumFix (TypeSum TypeSumFix))

instance Newtype TypeSumFix

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
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonNewtype TypeFix (Type TypeFix))

instance Newtype TypeFix
