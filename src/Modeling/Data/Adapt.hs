module Modeling.Data.Adapt where

import Data.Aeson
import Modeling.Data.Core
import Modeling.Data.MidType
import Modeling.Data.Util

typeMidInjection :: Injection ErrorMsg Type MidType
typeMidInjection = Injection apl inv where
    apl t =
        case t of
            StringType -> MidType StringTypeName Nothing
            LongType -> MidType LongTypeName Nothing
            DoubleType -> MidType DoubleTypeName Nothing
            OptionalType ty -> MidType OptionalTypeName (Just (emptyMidTypeAttrs { optional = Just (MidTypeSingleAttrs (apl ty)) }))
            ListType ty -> MidType ListTypeName (Just (emptyMidTypeAttrs { list = Just (MidTypeSingleAttrs (apl ty)) }))
            StringMapType ty -> MidType StringMapTypeName (Just (emptyMidTypeAttrs { stringmap = Just (MidTypeSingleAttrs (apl ty)) }))
            StructType fields -> MidType StructTypeName (Just (emptyMidTypeAttrs { struct = Just (MidTypeStructAttrs (apl <$> fields)) }))
            ReferenceType name -> MidType ReferenceTypeName (Just (emptyMidTypeAttrs { reference = Just (MidTypeReferenceAttrs name) }))
            EnumType values -> MidType EnumTypeName (Just (emptyMidTypeAttrs { enum = Just (MidTypeEnumAttrs values) }))
            UnionType branches -> MidType UnionTypeName (Just (emptyMidTypeAttrs { union = Just (MidTypeUnionAttrs (apl <$> branches)) }))
    inv (MidType n ma) = f ma where
        f = case n of
            StringTypeName -> withoutAttrs StringType
            LongTypeName -> withoutAttrs LongType
            DoubleTypeName -> withoutAttrs DoubleType
            OptionalTypeName -> withAttrs optional (\(MidTypeSingleAttrs ty) -> OptionalType <$> inv ty)
            ListTypeName -> withAttrs list (\(MidTypeSingleAttrs ty) -> ListType <$> inv ty)
            StringMapTypeName -> withAttrs stringmap (\(MidTypeSingleAttrs ty) -> StringMapType <$> inv ty)
            StructTypeName -> withAttrs struct (\(MidTypeStructAttrs fields) -> StructType <$> traverse inv fields)
            ReferenceTypeName -> withAttrs reference (\(MidTypeReferenceAttrs name) -> pure (ReferenceType name))
            EnumTypeName -> withAttrs enum (\(MidTypeEnumAttrs values) -> pure (EnumType values))
            UnionTypeName -> withAttrs union (\(MidTypeUnionAttrs branches) -> UnionType <$> traverse inv branches)

instance ToJSON Type where
    toJSON = injectionToJSON typeMidInjection

instance FromJSON Type where
    parseJSON = injectionParseJSON renderErrorMsg typeMidInjection

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
