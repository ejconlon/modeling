{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Modeling.Data.Fix where

import Data.Aeson
import Data.Data (Data, Typeable)
import Data.Function (on)
import GHC.Generics (Generic)

-- NOTE http://hackage.haskell.org/package/data-fix
-- Type and standard instances lifted from http://hackage.haskell.org/package/data-fix-0.2.0/docs/src/Data-Fix.html
-- so we don't have orphan instances for aeson typeclasses.
-- TODO do something clever with deriving via to define our own orphan-safe wrapper

-- | A fix-point type.
newtype Fix f = Fix { unFix :: f (Fix f) } deriving (Generic, Typeable)
deriving instance (Typeable f, Data (f (Fix f))) => Data (Fix f)

-- standard instances

instance Show (f (Fix f)) => Show (Fix f) where
    showsPrec n x = showParen (n > 10) $ \s ->
        "Fix " ++ showsPrec 11 (unFix x) s

instance Read (f (Fix f)) => Read (Fix f) where
    readsPrec d = readParen (d > 10) $ \r ->
        [(Fix m, t) | ("Fix", s) <- lex r, (m, t) <- readsPrec 11 s]

instance Eq (f (Fix f)) => Eq (Fix f) where
    (==) = (==) `on` unFix

instance Ord (f (Fix f)) => Ord (Fix f) where
    compare = compare `on` unFix

-- aeson interfaces

instance ToJSON (f (Fix f)) => ToJSON (Fix f) where
    toJSON = toJSON . unFix

instance FromJSON (f (Fix f)) => FromJSON (Fix f) where
    parseJSON = (Fix <$>) . parseJSON
