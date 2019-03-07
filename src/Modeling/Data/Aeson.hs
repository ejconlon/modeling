{-# LANGUAGE UndecidableInstances #-}

module Modeling.Data.Aeson where

import Control.Newtype.Generics (Newtype, O, pack, unpack)
import Data.Aeson
import Data.Aeson.Types         (Parser)
import GHC.Generics             (Generic, Rep)

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

newtype AesonNewtype n o = AesonNewtype { unAesonNewtype :: n }

instance (Newtype n, o ~ O n, ToJSON o) => ToJSON (AesonNewtype n o) where
    toJSON = toJSON . unpack . unAesonNewtype
    toEncoding = toEncoding . unpack . unAesonNewtype

instance (Newtype n, o ~ O n, FromJSON o) => FromJSON (AesonNewtype n o) where
    parseJSON = ((AesonNewtype . pack) <$>) . parseJSON

liftParser :: (e -> String) -> (a -> Either e b) -> Parser a -> Parser b
liftParser r f p = p >>= \a -> either (fail . r) pure (f a)
