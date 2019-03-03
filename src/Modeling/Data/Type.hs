module Modeling.Data.Type where

import Data.Aeson
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Modeling.Data.Bidi
import Modeling.Data.Fix
import Modeling.Data.Common
import Modeling.Data.Util

data TypeName =
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

typeNameToText :: Injection ErrorMsg TypeName Text
typeNameToText = Injection apply invert where
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

instance ToJSON TypeName where
    toJSON = injectionToJSON typeNameToText

instance FromJSON TypeName where
    parseJSON = injectionParseJSON renderErrorMsg typeNameToText

data TypeSingleAttrs a = TypeSingleAttrs
    { ty :: a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (TypeSingleAttrs a)
instance FromJSON a => FromJSON (TypeSingleAttrs a)

data TypeReferenceAttrs = TypeReferenceAttrs
    { name :: Text
    } deriving (Generic, Show, Eq)

instance ToJSON TypeReferenceAttrs
instance FromJSON TypeReferenceAttrs

data TypeStructAttrs a = TypeStructAttrs
    { fields :: Map Text a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (TypeStructAttrs a)
instance FromJSON a => FromJSON (TypeStructAttrs a)

data TypeEnumAttrs = TypeEnumAttrs
    { values :: Seq Text
    } deriving (Generic, Show, Eq)

instance ToJSON TypeEnumAttrs
instance FromJSON TypeEnumAttrs

data TypeUnionAttrs a = TypeUnionAttrs
    { elements :: Map Text a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (TypeUnionAttrs a)
instance FromJSON a => FromJSON (TypeUnionAttrs a)

data TypeAttrs a = TypeAttrs
    { optional :: Maybe (TypeSingleAttrs a)
    , list :: Maybe (TypeSingleAttrs a)
    , stringmap :: Maybe (TypeSingleAttrs a)
    , struct :: Maybe (TypeStructAttrs a)
    , reference :: Maybe TypeReferenceAttrs
    , enum :: Maybe TypeEnumAttrs
    , union :: Maybe (TypeUnionAttrs a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

emptyTypeAttrs :: TypeAttrs a
emptyTypeAttrs = TypeAttrs Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON a => ToJSON (TypeAttrs a)
instance FromJSON a => FromJSON (TypeAttrs a)

data TypeSum a = TypeSum
    { name :: TypeName
    , attributes :: Maybe (TypeAttrs a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (TypeSum a)
instance FromJSON a => FromJSON (TypeSum a)

typeSumPairBijection :: Bijection (TypeSum a) (TypeName, Maybe (TypeAttrs a))
typeSumPairBijection = Bijection apl inv where
    apl (TypeSum tn ma) = (tn, ma)
    inv (n, ma) = TypeSum n ma

typeSumSumInjection :: Injection ErrorMsg (TypeSum a) (Sum (TypeAttrs a))
typeSumSumInjection = domainInjection' typeNameToText typeSumPairBijection

newtype TypeSumFix = TypeSumFix { unTypeSumFix :: Fix TypeSum } deriving (Generic, Show, Eq)

instance ToJSON TypeSumFix
instance FromJSON TypeSumFix

typeSumFixBijection :: Bijection (TypeSum (Fix TypeSum)) TypeSumFix
typeSumFixBijection = Bijection (TypeSumFix . Fix) (unFix . unTypeSumFix)

data Type a =
    StringType
  | LongType
  | DoubleType
  | OptionalType (TypeSingleAttrs a)
  | ListType (TypeSingleAttrs a)
  | StringMapType (TypeSingleAttrs a)
  | StructType (TypeStructAttrs a)
  | ReferenceType TypeReferenceAttrs
  | EnumType TypeEnumAttrs
  | UnionType (TypeUnionAttrs a)
  deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

typePairInjection :: Injection ErrorMsg (Type a) (TypeName, Maybe (TypeAttrs a))
typePairInjection = Injection apl inv where
    apl t =
        case t of
            StringType -> (StringTypeName, Nothing)
            LongType -> (LongTypeName, Nothing)
            DoubleType -> (DoubleTypeName, Nothing)
            OptionalType attrs -> (OptionalTypeName, Just (emptyTypeAttrs { optional = Just attrs }))
            ListType attrs -> (ListTypeName, Just (emptyTypeAttrs { list = Just attrs }))
            StringMapType attrs -> (StringMapTypeName, Just (emptyTypeAttrs { stringmap = Just attrs }))
            StructType attrs -> (StructTypeName, Just (emptyTypeAttrs { struct = Just attrs }))
            ReferenceType attrs -> (ReferenceTypeName, Just (emptyTypeAttrs { reference = Just attrs }))
            EnumType attrs -> (EnumTypeName, Just (emptyTypeAttrs { enum = Just attrs }))
            UnionType attrs -> (UnionTypeName, Just (emptyTypeAttrs { union = Just attrs }))
    inv (n, ma) = f ma where
        f = case n of
            StringTypeName -> withoutAttrs StringType
            LongTypeName -> withoutAttrs LongType
            DoubleTypeName -> withoutAttrs DoubleType
            OptionalTypeName -> withAttrs optional OptionalType
            ListTypeName -> withAttrs list ListType
            StringMapTypeName -> withAttrs stringmap StringMapType
            StructTypeName -> withAttrs struct StructType
            ReferenceTypeName -> withAttrs reference ReferenceType
            EnumTypeName -> withAttrs enum EnumType
            UnionTypeName -> withAttrs union UnionType

typeSumInjection :: Injection ErrorMsg a b -> Injection ErrorMsg (Type a) (TypeSum b)
typeSumInjection rinj = composeInjection (postTraverseInjection rinj (lowerBijection (flipBijection typeSumPairBijection))) typePairInjection

newtype TypeFix = TypeFix { unTypeFix :: Fix Type } deriving (Generic, Show, Eq)

fixType :: Type (Fix Type) -> TypeFix
fixType = TypeFix . Fix

typeFixBijection :: Bijection (Type (Fix Type)) TypeFix
typeFixBijection = Bijection fixType (unFix . unTypeFix)

typeFixInjection :: Injection ErrorMsg TypeFix TypeSumFix
typeFixInjection = undefined

instance ToJSON TypeFix where
    toJSON = injectionToJSON typeFixInjection

instance FromJSON TypeFix where
    parseJSON = injectionParseJSON renderErrorMsg typeFixInjection
