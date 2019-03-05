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

-- typeSumPairBijection :: Bijection (TypeSum a) (TypeCon, Maybe (TypeAttrs a))
-- typeSumPairBijection = Bijection apl inv where
--     apl (TypeSum tn ma) = (tn, ma)
--     inv (n, ma) = TypeSum n ma

-- typeSumSumInjection :: Injection ErrorMsg (TypeSum a) (Sum (TypeAttrs a))
-- typeSumSumInjection = domainInjection' typeConToText typeSumPairBijection

newtype TypeSumFix = TypeSumFix { unTypeSumFix :: TypeSum TypeSumFix }
    deriving (Generic, Show, Eq)
    deriving (ToJSON, FromJSON) via (AesonWrapperApp TypeSum TypeSumFix)

-- deriving via (AesonWrapper (TypeSum TypeSumFix)) instance ToJSON TypeSumFix
-- deriving via (AesonWrapper (TypeSum TypeSumFix)) instance FromJSON TypeSumFix

-- typeSumFixBijection :: Bijection (TypeSum TypeSumFix) TypeSumFix
-- typeSumFixBijection = Bijection TypeSumFix unTypeSumFix

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

instance ToJSON1 Type where
    liftToJSON = undefined
    liftToEncoding = undefined

instance FromJSON1 Type where
    liftParseJSON = undefined

-- typePairInjection :: Injection ErrorMsg (Type a) (TypeCon, Maybe (TypeAttrs a))
-- typePairInjection = Injection apl inv where
--     apl t =
--         case t of
--             StringType -> (StringTypeCon, Nothing)
--             LongType -> (LongTypeCon, Nothing)
--             DoubleType -> (DoubleTypeCon, Nothing)
--             BooleanType -> (BooleanTypeCon, Nothing)
--             OptionalType attrs -> (OptionalTypeCon, Just (emptyTypeAttrs { optional = Just attrs }))
--             ListType attrs -> (ListTypeCon, Just (emptyTypeAttrs { list = Just attrs }))
--             StringMapType attrs -> (StringMapTypeCon, Just (emptyTypeAttrs { stringmap = Just attrs }))
--             StructType attrs -> (StructTypeCon, Just (emptyTypeAttrs { struct = Just attrs }))
--             ReferenceType attrs -> (ReferenceTypeCon, Just (emptyTypeAttrs { reference = Just attrs }))
--             EnumType attrs -> (EnumTypeCon, Just (emptyTypeAttrs { enum = Just attrs }))
--             UnionType attrs -> (UnionTypeCon, Just (emptyTypeAttrs { union = Just attrs }))
--             AnyType -> (AnyTypeCon, Nothing)
--     inv (n, ma) = f ma where
--         f = case n of
--             StringTypeCon -> withoutAttrs StringType
--             LongTypeCon -> withoutAttrs LongType
--             DoubleTypeCon -> withoutAttrs DoubleType
--             BooleanTypeCon -> withoutAttrs BooleanType
--             OptionalTypeCon -> withAttrs optional OptionalType
--             ListTypeCon -> withAttrs list ListType
--             StringMapTypeCon -> withAttrs stringmap StringMapType
--             StructTypeCon -> withAttrs struct StructType
--             ReferenceTypeCon -> withAttrs reference ReferenceType
--             EnumTypeCon -> withAttrs enum EnumType
--             UnionTypeCon -> withAttrs union UnionType
--             AnyTypeCon -> withoutAttrs AnyType

-- typeSumInjection :: Injection ErrorMsg a b -> Injection ErrorMsg (Type a) (TypeSum b)
-- typeSumInjection rinj = composeInjection (postTraverseInjection rinj (lowerBijection (flipBijection typeSumPairBijection))) typePairInjection

newtype TypeFix = TypeFix { unTypeFix :: Type TypeFix }
    deriving (Generic, Show, Eq)
    deriving (ToJSON, FromJSON) via (AesonWrapperApp Type TypeFix)

-- typeFixBijection :: Bijection (Type TypeFix) TypeFix
-- typeFixBijection = Bijection TypeFix unTypeFix

-- typeFixInjection :: Injection ErrorMsg TypeFix TypeSumFix
-- typeFixInjection =
--     let knot = typeSumInjection typeFixInjection
--     in composeRight (composeLeft typeSumFixBijection knot) (flipBijection typeFixBijection)

-- instance ToJSON TypeFix where
--     toJSON = injectionToJSON typeFixInjection
--     toEncoding = injectionToEncoding typeFixInjection

-- instance FromJSON TypeFix where
--     parseJSON = injectionParseJSON renderErrorMsg typeFixInjection
