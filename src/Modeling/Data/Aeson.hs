{-# LANGUAGE UndecidableInstances #-}

module Modeling.Data.Aeson where

import Data.Aeson
import GHC.Generics (Generic, Rep)

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
