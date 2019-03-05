{-# LANGUAGE UndecidableInstances #-}

module Modeling.Data.Aeson where

import Data.Aeson
import GHC.Generics (Generic, Generic1, Rep, Rep1)

options :: Options
options = defaultOptions
    { omitNothingFields = True
    }

newtype AesonWrapper a = AesonWrapper { unAesonWrapper :: a }

instance (Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a)) => ToJSON (AesonWrapper a) where
    toJSON = genericToJSON options . unAesonWrapper
    toEncoding = genericToEncoding options . unAesonWrapper

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (AesonWrapper a) where
    parseJSON = (AesonWrapper <$>) . genericParseJSON options

newtype AesonWrapper1 f a = AesonWrapper1 { unAesonWrapper1 :: f a }

instance (Generic1 f, GToJSON One (Rep1 f), GToEncoding One (Rep1 f)) => ToJSON1 (AesonWrapper1 f) where
    liftToJSON a b = genericLiftToJSON options a b . unAesonWrapper1
    liftToEncoding a b = genericLiftToEncoding options a b . unAesonWrapper1

instance (Generic1 f, GFromJSON One (Rep1 f)) => FromJSON1 (AesonWrapper1 f) where
    liftParseJSON a b c = AesonWrapper1 <$> genericLiftParseJSON options a b c

newtype AesonWrapperApp f a = AesonWrapperApp { unAesonWrapperApp :: f a }

instance (Generic1 f, ToJSON1 f, ToJSON a) => ToJSON (AesonWrapperApp f a) where
    toJSON = liftToJSON toJSON toJSON . unAesonWrapperApp
    toEncoding = liftToEncoding toEncoding toEncoding . unAesonWrapperApp

instance (Generic1 f, FromJSON1 f, FromJSON a) => FromJSON (AesonWrapperApp f a) where
    parseJSON = (AesonWrapperApp <$>) . liftParseJSON parseJSON parseJSON
