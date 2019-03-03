module Modeling.Data.Type where

import Data.Aeson
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
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

data TypeShim = TypeShim
    { name :: TypeName
    , attributes :: Maybe (TypeAttrs TypeShim)
    } deriving (Generic, Show, Eq)

instance ToJSON TypeShim
instance FromJSON TypeShim

-- NOTE: Dont need to do this here, we're not guaranteeing that the shim is correct
-- typeShimPairInjection :: Injection ErrorMsg TypeShim (TypeName, Maybe (TypeAttrs TypeShim))
-- typeShimPairInjection = Injection apl inv where
--     apl (TypeShim tn ma) = (tn, ma)
--     inv (n, ma) = TypeShim n <$> f ma where
--         f = case n of
--             StringTypeName -> simpleWithoutAttrs
--             LongTypeName -> simpleWithoutAttrs
--             DoubleTypeName -> simpleWithoutAttrs
--             OptionalTypeName -> simpleWithAttrs optional
--             ListTypeName -> simpleWithAttrs list
--             StringMapTypeName -> simpleWithAttrs stringmap
--             StructTypeName -> simpleWithAttrs struct
--             ReferenceTypeName -> simpleWithAttrs reference
--             EnumTypeName -> simpleWithAttrs enum
--             UnionTypeName -> simpleWithAttrs union

typeShimPairBijection :: Bijection TypeShim (TypeName, Maybe (TypeAttrs TypeShim))
typeShimPairBijection = Bijection apl inv where
    apl (TypeShim tn ma) = (tn, ma)
    inv (n, ma) = TypeShim n ma

typeShimSumInjection :: Injection ErrorMsg TypeShim (Sum (TypeAttrs TypeShim))
typeShimSumInjection = domainInjection' typeNameToText typeShimPairBijection

data Type =
    StringType
  | LongType
  | DoubleType
  | OptionalType Type
  | ListType Type
  | StringMapType Type
  | StructType (Map Text Type)
  | ReferenceType Text
  | EnumType (Seq Text)
  | UnionType (Map Text Type)
  deriving (Generic, Show, Eq)

typeShimInjection :: Injection ErrorMsg Type TypeShim
typeShimInjection = Injection apl inv where
    apl t =
        case t of
            StringType -> TypeShim StringTypeName Nothing
            LongType -> TypeShim LongTypeName Nothing
            DoubleType -> TypeShim DoubleTypeName Nothing
            OptionalType ty -> TypeShim OptionalTypeName (Just (emptyTypeAttrs { optional = Just (TypeSingleAttrs (apl ty)) }))
            ListType ty -> TypeShim ListTypeName (Just (emptyTypeAttrs { list = Just (TypeSingleAttrs (apl ty)) }))
            StringMapType ty -> TypeShim StringMapTypeName (Just (emptyTypeAttrs { stringmap = Just (TypeSingleAttrs (apl ty)) }))
            StructType fields -> TypeShim StructTypeName (Just (emptyTypeAttrs { struct = Just (TypeStructAttrs (apl <$> fields)) }))
            ReferenceType name -> TypeShim ReferenceTypeName (Just (emptyTypeAttrs { reference = Just (TypeReferenceAttrs name) }))
            EnumType values -> TypeShim EnumTypeName (Just (emptyTypeAttrs { enum = Just (TypeEnumAttrs values) }))
            UnionType branches -> TypeShim UnionTypeName (Just (emptyTypeAttrs { union = Just (TypeUnionAttrs (apl <$> branches)) }))
    inv (TypeShim n ma) = f ma where
        f = case n of
            StringTypeName -> withoutAttrs StringType
            LongTypeName -> withoutAttrs LongType
            DoubleTypeName -> withoutAttrs DoubleType
            OptionalTypeName -> withAttrs optional (\(TypeSingleAttrs ty) -> OptionalType <$> inv ty)
            ListTypeName -> withAttrs list (\(TypeSingleAttrs ty) -> ListType <$> inv ty)
            StringMapTypeName -> withAttrs stringmap (\(TypeSingleAttrs ty) -> StringMapType <$> inv ty)
            StructTypeName -> withAttrs struct (\(TypeStructAttrs fields) -> StructType <$> traverse inv fields)
            ReferenceTypeName -> withAttrs reference (\(TypeReferenceAttrs name) -> pure (ReferenceType name))
            EnumTypeName -> withAttrs enum (\(TypeEnumAttrs values) -> pure (EnumType values))
            UnionTypeName -> withAttrs union (\(TypeUnionAttrs branches) -> UnionType <$> traverse inv branches)

instance ToJSON Type where
    toJSON = injectionToJSON typeShimInjection

instance FromJSON Type where
    parseJSON = injectionParseJSON renderErrorMsg typeShimInjection
