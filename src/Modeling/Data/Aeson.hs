{-# LANGUAGE UndecidableInstances #-}

module Modeling.Data.Aeson where

import Data.Aeson
import Data.Aeson.Types (Parser, listEncoding, listParser)
import Data.Vector      (fromList)
import GHC.Generics     (Generic, Generic1, Rep, Rep1)

options :: Options
options = defaultOptions
    { omitNothingFields = True
    }

newtype AesonWrapper a = AesonWrapper { unAesonWrapper :: a } deriving (Eq, Show)

instance (Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a)) => ToJSON (AesonWrapper a) where
    toJSON = genericToJSON options . unAesonWrapper
    toEncoding = genericToEncoding options . unAesonWrapper

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (AesonWrapper a) where
    parseJSON = (AesonWrapper <$>) . genericParseJSON options

newtype AesonWrapper1 f a = AesonWrapper1 { unAesonWrapper1 :: f a } deriving (Eq, Show)

instance (Generic1 f, GToJSON One (Rep1 f), GToEncoding One (Rep1 f)) => ToJSON1 (AesonWrapper1 f) where
    liftToJSON a b = genericLiftToJSON options a b . unAesonWrapper1
    liftToEncoding a b = genericLiftToEncoding options a b . unAesonWrapper1

instance (Generic1 f, GFromJSON One (Rep1 f)) => FromJSON1 (AesonWrapper1 f) where
    liftParseJSON a b c = AesonWrapper1 <$> genericLiftParseJSON options a b c

newtype AesonWrapperApp f a = AesonWrapperApp { unAesonWrapperApp :: f a } deriving (Eq, Show)

instance (ToJSON1 f, ToJSON a) => ToJSON (AesonWrapperApp f a) where
    toJSON = toJSON1 . unAesonWrapperApp
    toEncoding = toEncoding1 . unAesonWrapperApp

instance (FromJSON1 f, FromJSON a) => FromJSON (AesonWrapperApp f a) where
    parseJSON = (AesonWrapperApp <$>) . parseJSON1

newtype AesonWrapperComp f g a = AesonWrapperComp { unAesonWrapperComp :: f (g a) } deriving (Eq, Show)

instance (ToJSON1 f, ToJSON1 g) => ToJSON1 (AesonWrapperComp f g) where
    liftToJSON tv tvl =
        let gv = liftToJSON tv tvl
            gvl = Array . fromList . fmap gv
        in liftToJSON gv gvl . unAesonWrapperComp
    liftToEncoding tv tvl =
        let gv = liftToEncoding tv tvl
            gvl = listEncoding gv
        in liftToEncoding gv gvl . unAesonWrapperComp

instance (FromJSON1 f, FromJSON1 g) => FromJSON1 (AesonWrapperComp f g) where
    liftParseJSON tv tvl =
        let gv = liftParseJSON tv tvl
            gvl = listParser gv
        in (AesonWrapperComp <$>) . liftParseJSON gv gvl

liftParser :: (e -> String) -> (a -> Either e b) -> Parser a -> Parser b
liftParser r f p = p >>= \a -> either (fail . r) pure (f a)
