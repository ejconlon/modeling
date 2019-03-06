module Modeling.Data.Type where

import Data.Aeson
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic, Generic1)
import Modeling.Data.Aeson
import Modeling.Data.Bidi
import Modeling.Data.Common
import Modeling.Data.Util

data TypeCon =
      StringTypeCon
    | LongTypeCon
    | DoubleTypeCon
    | BooleanTypeCon
    | OptionalTypeCon
    | ListTypeCon
    | StringMapTypeCon
    | StructTypeCon
    | ReferenceTypeCon
    | EnumTypeCon
    | UnionTypeCon
    | AnyTypeCon
    deriving (Generic, Eq, Show)

typeConToText :: Injection ErrorMsg TypeCon Text
typeConToText = Injection apply invert where
    apply t =
        case t of
            StringTypeCon -> "string"
            LongTypeCon -> "long"
            DoubleTypeCon -> "double"
            BooleanTypeCon -> "boolean"
            OptionalTypeCon -> "optional"
            ListTypeCon -> "list"
            StringMapTypeCon -> "stringmap"
            StructTypeCon -> "struct"
            ReferenceTypeCon -> "reference"
            EnumTypeCon -> "enum"
            UnionTypeCon -> "union"
            AnyTypeCon -> "any"
    invert u =
        case u of
            "string" -> Right StringTypeCon
            "long" -> Right LongTypeCon
            "double" -> Right DoubleTypeCon
            "boolean" -> Right BooleanTypeCon
            "optional" -> Right OptionalTypeCon
            "list" -> Right ListTypeCon
            "stringmap" -> Right StringMapTypeCon
            "struct" -> Right StructTypeCon
            "reference" -> Right ReferenceTypeCon
            "enum" -> Right EnumTypeCon
            "union" -> Right UnionTypeCon
            "any" -> Right AnyTypeCon
            _ -> Left (ErrorMsg ("Unknown type name " <> u))

instance ToJSON TypeCon where
    toJSON = injectionToJSON typeConToText
    toEncoding = injectionToEncoding typeConToText

instance FromJSON TypeCon where
    parseJSON = injectionParseJSON renderErrorMsg typeConToText

data TypeSingleAttrs a = TypeSingleAttrs
    { ty :: a
    } deriving (Generic1, Show, Eq, Functor, Foldable, Traversable)
      deriving (ToJSON1, FromJSON1) via (AesonWrapper1 TypeSingleAttrs)

data TypeReferenceAttrs = TypeReferenceAttrs
    { name :: Text
    } deriving (Generic, Show, Eq)
      deriving (ToJSON, FromJSON) via (AesonWrapper TypeReferenceAttrs)

data TypeStructAttrs a = TypeStructAttrs
    { fields :: Map Text a
    } deriving (Generic1, Show, Eq, Functor, Foldable, Traversable)
      deriving (ToJSON1, FromJSON1) via (AesonWrapper1 TypeStructAttrs)

data TypeEnumAttrs = TypeEnumAttrs
    { values :: Seq Text
    } deriving (Generic, Show, Eq)
      deriving (ToJSON, FromJSON) via (AesonWrapper TypeEnumAttrs)

data TypeUnionAttrs a = TypeUnionAttrs
    { elements :: Map Text a
    } deriving (Generic1, Show, Eq, Functor, Foldable, Traversable)
      deriving (ToJSON1, FromJSON1) via (AesonWrapper1 TypeUnionAttrs)

data TypeAttrs a = TypeAttrs
    { optional :: Maybe (TypeSingleAttrs a)
    , list :: Maybe (TypeSingleAttrs a)
    , stringmap :: Maybe (TypeSingleAttrs a)
    , struct :: Maybe (TypeStructAttrs a)
    , reference :: Maybe TypeReferenceAttrs
    , enum :: Maybe TypeEnumAttrs
    , union :: Maybe (TypeUnionAttrs a)
    } deriving (Generic1, Show, Eq, Functor, Foldable, Traversable)
      deriving (ToJSON1, FromJSON1) via (AesonWrapper1 TypeAttrs)

emptyTypeAttrs :: TypeAttrs a
emptyTypeAttrs = TypeAttrs Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data TypeSum a = TypeSum
    { name :: TypeCon
    , attributes :: Maybe (TypeAttrs a)
    } deriving (Generic1, Show, Eq, Functor, Foldable, Traversable)
      deriving (ToJSON1, FromJSON1) via (AesonWrapper1 TypeSum)

newtype TypeSumFix = TypeSumFix { unTypeSumFix :: TypeSum TypeSumFix }
    deriving (Show, Eq)
    deriving (ToJSON, FromJSON) via (AesonWrapperApp TypeSum TypeSumFix)

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
  deriving (Generic1, Show, Eq, Functor, Foldable, Traversable)

typeToPair :: Type a -> TypeSum a
typeToPair t =
    case t of
        StringType -> TypeSum StringTypeCon Nothing
        LongType -> TypeSum LongTypeCon Nothing
        DoubleType -> TypeSum DoubleTypeCon Nothing
        BooleanType -> TypeSum BooleanTypeCon Nothing
        OptionalType attrs -> TypeSum OptionalTypeCon (Just (emptyTypeAttrs { optional = Just attrs }))
        ListType attrs -> TypeSum ListTypeCon (Just (emptyTypeAttrs { list = Just attrs }))
        StringMapType attrs -> TypeSum StringMapTypeCon (Just (emptyTypeAttrs { stringmap = Just attrs }))
        StructType attrs -> TypeSum StructTypeCon (Just (emptyTypeAttrs { struct = Just attrs }))
        ReferenceType attrs -> TypeSum ReferenceTypeCon (Just (emptyTypeAttrs { reference = Just attrs }))
        EnumType attrs -> TypeSum EnumTypeCon (Just (emptyTypeAttrs { enum = Just attrs }))
        UnionType attrs -> TypeSum UnionTypeCon (Just (emptyTypeAttrs { union = Just attrs }))
        AnyType -> TypeSum AnyTypeCon Nothing

typeFromPair :: TypeSum a -> Either ErrorMsg (Type a)
typeFromPair (TypeSum n ma) = f ma where
    f = case n of
        StringTypeCon -> withoutAttrs StringType
        LongTypeCon -> withoutAttrs LongType
        DoubleTypeCon -> withoutAttrs DoubleType
        BooleanTypeCon -> withoutAttrs BooleanType
        OptionalTypeCon -> withAttrs optional OptionalType
        ListTypeCon -> withAttrs list ListType
        StringMapTypeCon -> withAttrs stringmap StringMapType
        StructTypeCon -> withAttrs struct StructType
        ReferenceTypeCon -> withAttrs reference ReferenceType
        EnumTypeCon -> withAttrs enum EnumType
        UnionTypeCon -> withAttrs union UnionType
        AnyTypeCon -> withoutAttrs AnyType

instance ToJSON1 Type where
    liftToJSON tv tvl = liftToJSON tv tvl . typeToPair
    liftToEncoding tv tvl = liftToEncoding tv tvl . typeToPair

instance FromJSON1 Type where
    liftParseJSON tv tvl = liftParser renderErrorMsg typeFromPair . liftParseJSON tv tvl

newtype TypeFix = TypeFix { unTypeFix :: Type TypeFix }
    deriving (Show, Eq)
    deriving (ToJSON, FromJSON) via (AesonWrapperApp Type TypeFix)
