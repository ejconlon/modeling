{-# LANGUAGE ScopedTypeVariables #-}

module Modeling.Data.JsonRep where

import Modeling.Data.Generics

import Data.Aeson (Value)
import Data.Map (Map)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import Data.Text (Text)

data JsonRep a =
      JsonRepAny
    | JsonRepBool
    | JsonRepInt
    | JsonRepString
    | JsonRepDouble
    | JsonRepList a
    | JsonRepMap a a
    | JsonRepOptional a
    deriving (Show, Eq, Functor, Foldable, Traversable)

instance HasGenRep JsonRep Value where
    getGenRoot _ = GenTypeFix (GenEmbed JsonRepAny)
    getGenDecl _ = Nothing

instance HasGenRep JsonRep Int where
    getGenRoot _ = GenTypeFix (GenEmbed JsonRepInt)
    getGenDecl _ = Nothing

instance HasGenRep JsonRep Text where
    getGenRoot _ = GenTypeFix (GenEmbed JsonRepString)
    getGenDecl _ = Nothing

instance HasGenRep JsonRep Double where
    getGenRoot _ = GenTypeFix (GenEmbed JsonRepDouble)
    getGenDecl _ = Nothing

instance HasGenRep JsonRep Bool where
    getGenRoot _ = GenTypeFix (GenEmbed JsonRepBool)
    getGenDecl _ = Nothing

instance HasGenRep JsonRep a => HasGenRep JsonRep (Maybe a) where
    getGenRoot _ = GenTypeFix (GenEmbed (JsonRepOptional (getGenRep (Proxy :: Proxy (JsonRep a)))))
    getGenDecl _ = Nothing

instance HasGenRep JsonRep a => HasGenRep JsonRep (Seq a) where
    getGenRoot _ = GenTypeFix (GenEmbed (JsonRepList (getGenRep (Proxy :: Proxy (JsonRep a)))))
    getGenDecl _ = Nothing

instance (HasGenRep JsonRep s, HasGenRep JsonRep a) => HasGenRep JsonRep (Map s a) where
    getGenRoot _ = GenTypeFix (GenEmbed (JsonRepMap (getGenRep (Proxy :: Proxy (JsonRep a))) (getGenRep (Proxy :: Proxy (JsonRep a)))))
    getGenDecl _ = Nothing
