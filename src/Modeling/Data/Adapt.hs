module Modeling.Data.Adapt where

import Data.Aeson
import Modeling.Data.Core
import Modeling.Data.MidType
import Modeling.Data.Util

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

instance ToJSON ExtParam
instance FromJSON ExtParam

instance ToJSON Interface
instance FromJSON Interface

instance ToJSON ModelConnection
instance FromJSON ModelConnection

instance ToJSON Model
instance FromJSON Model

instance ToJSON ModelSpace
instance FromJSON ModelSpace

instance ToJSON Bundle
instance FromJSON Bundle
