module Modeling.Data.Adapt where

import Data.Aeson
import Modeling.Data.Core
import Modeling.Data.MidType
import Modeling.Data.Util

missingAttrs, unexpectedAttrs :: ErrorMsg
missingAttrs = ErrorMsg "Missing attrs"
unexpectedAttrs = ErrorMsg "Unexpected attrs"

withoutAttrs :: Maybe a -> b -> Either ErrorMsg b
withoutAttrs ma y = case ma of { Nothing -> pure y; Just _ -> Left unexpectedAttrs }

withAttrs :: Maybe a -> (a -> Maybe v) -> (v -> Either ErrorMsg b) -> Either ErrorMsg b
withAttrs ma s f = case (ma >>= s) of { Nothing -> Left missingAttrs; Just x -> f x }

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
    inv (MidType n ma) =
        case n of
            StringTypeName -> withoutAttrs ma StringType
            LongTypeName -> withoutAttrs ma LongType
            DoubleTypeName -> withoutAttrs ma DoubleType
            OptionalTypeName -> withAttrs ma optional (\(MidTypeSingleAttrs ty) -> OptionalType <$> inv ty)
            ListTypeName -> withAttrs ma list (\(MidTypeSingleAttrs ty) -> ListType <$> inv ty)
            StringMapTypeName -> withAttrs ma stringmap (\(MidTypeSingleAttrs ty) -> StringMapType <$> inv ty)
            StructTypeName -> withAttrs ma struct (\(MidTypeStructAttrs fields) -> StructType <$> traverse inv fields)
            ReferenceTypeName -> withAttrs ma reference (\(MidTypeReferenceAttrs name) -> pure (ReferenceType name))
            EnumTypeName -> withAttrs ma enum (\(MidTypeEnumAttrs values) -> pure (EnumType values))
            UnionTypeName -> withAttrs ma union (\(MidTypeUnionAttrs branches) -> UnionType <$> traverse inv branches)

instance ToJSON Type where
    toJSON = injectionToJSON typeMidInjection

instance FromJSON Type where
    parseJSON = injectionParseJSON renderErrorMsg typeMidInjection

instance ToJSON ExtParam
instance FromJSON ExtParam

instance ToJSON Interface
instance FromJSON Interface

instance ToJSON a => ToJSON (Bundle a)
instance FromJSON a => FromJSON (Bundle a)
