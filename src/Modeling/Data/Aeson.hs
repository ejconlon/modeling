{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Modeling.Data.Aeson (
      AesonRecord (..)
    , AesonNewtype (..)
    , AesonTag (..)
    , HasJSONOptions (..)
    , HasTagPrefix (..)
    , liftParser
) where

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

newtypeOptions :: Options
newtypeOptions = defaultOptions
    { unwrapUnaryRecords = True
    }

class HasJSONOptions a where
    getJSONOptions :: Proxy a -> Options

class HasTagPrefix a where
    getTagPrefix :: Proxy a -> String

newtype AesonTag a = AesonTag { unAesonTag :: a }

instance HasTagPrefix a => HasJSONOptions (AesonTag a) where
    getJSONOptions _ = tagOptions (getTagPrefix (Proxy :: Proxy a))

instance (HasJSONOptions (AesonTag a), Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a)) => ToJSON (AesonTag a) where
    toJSON = genericToJSON (getJSONOptions (Proxy :: Proxy (AesonTag a))) . unAesonTag
    toEncoding = genericToEncoding (getJSONOptions (Proxy :: Proxy (AesonTag a))) . unAesonTag

instance (HasJSONOptions (AesonTag a), Generic a, GFromJSON Zero (Rep a)) => FromJSON (AesonTag a) where
    parseJSON = (AesonTag <$>) . genericParseJSON (getJSONOptions (Proxy :: Proxy (AesonTag a)))

newtype AesonRecord a = AesonRecord { unAesonRecord :: a }

instance HasJSONOptions (AesonRecord a) where
    getJSONOptions _ = recordOptions

instance (HasJSONOptions (AesonRecord a), Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a)) => ToJSON (AesonRecord a) where
    toJSON = genericToJSON (getJSONOptions (Proxy :: Proxy (AesonRecord a))) . unAesonRecord
    toEncoding = genericToEncoding (getJSONOptions (Proxy :: Proxy (AesonRecord a))) . unAesonRecord

instance (HasJSONOptions (AesonRecord a), Generic a, GFromJSON Zero (Rep a)) => FromJSON (AesonRecord a) where
    parseJSON = (AesonRecord <$>) . genericParseJSON (getJSONOptions (Proxy :: Proxy (AesonRecord a)))

newtype AesonNewtype n o = AesonNewtype { unAesonNewtype :: n }

instance HasJSONOptions (AesonNewtype n o) where
    getJSONOptions _ = newtypeOptions

instance (Newtype n, o ~ O n, ToJSON o) => ToJSON (AesonNewtype n o) where
    toJSON = toJSON . unpack . unAesonNewtype
    toEncoding = toEncoding . unpack . unAesonNewtype

instance (Newtype n, o ~ O n, FromJSON o) => FromJSON (AesonNewtype n o) where
    parseJSON = ((AesonNewtype . pack) <$>) . parseJSON

liftParser :: (e -> String) -> (a -> Either e b) -> Parser a -> Parser b
liftParser r f p = p >>= \a -> either (fail . r) pure (f a)
