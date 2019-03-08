{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Modeling.Data.Aeson where

import Control.Newtype.Generics (Newtype, O, pack, unpack)
import Data.Aeson
import Data.Aeson.Casing        (snakeCase)
import Data.Aeson.Types         (Parser)
import Data.Proxy               (Proxy (..))
import GHC.Generics             (Generic, Rep)

recordOptions :: Options
recordOptions = defaultOptions
    { omitNothingFields = True
    , fieldLabelModifier = snakeCase
    }

tagOptions :: String -> Options
tagOptions prefix =
    let prefixLen = length prefix
    in defaultOptions
        { allNullaryToStringTag = True
        , constructorTagModifier = snakeCase . drop prefixLen
        }

class HasJSONOptions a where
    getJSONOptions :: Proxy a -> Options

newtype AesonWrapper a = AesonWrapper { unAesonWrapper :: a }

instance (HasJSONOptions a, Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a)) => ToJSON (AesonWrapper a) where
    toJSON = genericToJSON (getJSONOptions (Proxy :: Proxy a)) . unAesonWrapper
    toEncoding = genericToEncoding (getJSONOptions (Proxy :: Proxy a)) . unAesonWrapper

instance (HasJSONOptions a, Generic a, GFromJSON Zero (Rep a)) => FromJSON (AesonWrapper a) where
    parseJSON = (AesonWrapper <$>) . genericParseJSON (getJSONOptions (Proxy :: Proxy a))

newtype AesonNewtype n o = AesonNewtype { unAesonNewtype :: n }

instance (Newtype n, o ~ O n, ToJSON o) => ToJSON (AesonNewtype n o) where
    toJSON = toJSON . unpack . unAesonNewtype
    toEncoding = toEncoding . unpack . unAesonNewtype

instance (Newtype n, o ~ O n, FromJSON o) => FromJSON (AesonNewtype n o) where
    parseJSON = ((AesonNewtype . pack) <$>) . parseJSON

liftParser :: (e -> String) -> (a -> Either e b) -> Parser a -> Parser b
liftParser r f p = p >>= \a -> either (fail . r) pure (f a)
