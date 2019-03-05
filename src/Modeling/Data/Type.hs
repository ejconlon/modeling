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

instance FromJSON TypeCon where
    parseJSON = injectionParseJSON renderErrorMsg typeConToText

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
    { name :: TypeCon
    , attributes :: Maybe (TypeAttrs a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (TypeSum a)
instance FromJSON a => FromJSON (TypeSum a)

typeSumPairBijection :: Bijection (TypeSum a) (TypeCon, Maybe (TypeAttrs a))
typeSumPairBijection = Bijection apl inv where
    apl (TypeSum tn ma) = (tn, ma)
    inv (n, ma) = TypeSum n ma

typeSumSumInjection :: Injection ErrorMsg (TypeSum a) (Sum (TypeAttrs a))
typeSumSumInjection = domainInjection' typeConToText typeSumPairBijection

newtype TypeSumFix = TypeSumFix { unTypeSumFix :: Fix TypeSum }
    deriving (Generic, Show, Eq)
    deriving (ToJSON, FromJSON) via (Fix TypeSum)

typeSumFixBijection :: Bijection (TypeSum (Fix TypeSum)) TypeSumFix
typeSumFixBijection = Bijection (TypeSumFix . Fix) (unFix . unTypeSumFix)

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

typePairInjection :: Injection ErrorMsg (Type a) (TypeCon, Maybe (TypeAttrs a))
typePairInjection = Injection apl inv where
    apl t =
        case t of
            StringType -> (StringTypeCon, Nothing)
            LongType -> (LongTypeCon, Nothing)
            DoubleType -> (DoubleTypeCon, Nothing)
            BooleanType -> (BooleanTypeCon, Nothing)
            OptionalType attrs -> (OptionalTypeCon, Just (emptyTypeAttrs { optional = Just attrs }))
            ListType attrs -> (ListTypeCon, Just (emptyTypeAttrs { list = Just attrs }))
            StringMapType attrs -> (StringMapTypeCon, Just (emptyTypeAttrs { stringmap = Just attrs }))
            StructType attrs -> (StructTypeCon, Just (emptyTypeAttrs { struct = Just attrs }))
            ReferenceType attrs -> (ReferenceTypeCon, Just (emptyTypeAttrs { reference = Just attrs }))
            EnumType attrs -> (EnumTypeCon, Just (emptyTypeAttrs { enum = Just attrs }))
            UnionType attrs -> (UnionTypeCon, Just (emptyTypeAttrs { union = Just attrs }))
            AnyType -> (AnyTypeCon, Nothing)
    inv (n, ma) = f ma where
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

typeSumInjection :: Injection ErrorMsg a b -> Injection ErrorMsg (Type a) (TypeSum b)
typeSumInjection rinj = composeInjection (postTraverseInjection rinj (lowerBijection (flipBijection typeSumPairBijection))) typePairInjection

newtype TypeFix = TypeFix { unTypeFix :: Fix Type } deriving (Generic, Show, Eq)

fixType :: Type (Fix Type) -> TypeFix
fixType = TypeFix . Fix

typeFixBijection :: Bijection (Type (Fix Type)) TypeFix
typeFixBijection = Bijection fixType (unFix . unTypeFix)

typeFixInjection :: Injection ErrorMsg TypeFix TypeSumFix
typeFixInjection =
    let knot = typeSumInjection (bothFixInjection knot)
    in composeRight (composeLeft typeSumFixBijection knot) (flipBijection typeFixBijection)

instance ToJSON TypeFix where
    toJSON = injectionToJSON typeFixInjection

instance FromJSON TypeFix where
    parseJSON = injectionParseJSON renderErrorMsg typeFixInjection
