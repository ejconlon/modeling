{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Modeling.Data.Generics where

import Control.Newtype.Generics
import Data.Aeson (Value)
import Data.Map (Map)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Modeling.Data.Aeson
import Modeling.Data.Error

data Private

data Part a =
      PartSum a a
    | PartProd a a
    | PartDatatype Text a
    | PartConstructor Text a
    | PartSelector Text a
    | PartCon a
    | PartEmpty
    deriving (Show, Eq, Functor, Foldable, Traversable)

data Logic a =
      LogicAny
    | LogicBool
    | LogicInt
    | LogicString
    | LogicDouble
    | LogicArray a
    | LogicMap a a
    | LogicOptional a
    deriving (Show, Eq, Functor, Foldable, Traversable)

data GenType a =
      GenRef Text
    | GenLogic (Logic a)
    | GenPart (Part a)
    deriving (Show, Eq)

data GenTypeFix = GenTypeFix { unGenTypeFix :: GenType GenTypeFix } deriving (Show, Eq)

class HasStringRep a where
    getStringRep :: Proxy a -> ()

class HasGenRep a where
    getGenRoot :: Proxy a -> GenTypeFix
    getGenDecl :: Proxy a -> Maybe Text

    default getGenRoot :: (Generic a, GHasGenRep (Rep a)) => Proxy a -> GenTypeFix
    getGenRoot _ = gGetGenRoot (Proxy :: Proxy (Rep a Private))

    default getGenDecl :: (Generic a, GHasGenRep (Rep a)) => Proxy a -> Maybe Text
    getGenDecl _ = gGetGenDecl (Proxy :: Proxy (Rep a Private))

getGenRep :: HasGenRep a => Proxy a -> GenTypeFix
getGenRep p = maybe (getGenRoot p) (GenTypeFix . GenRef) (getGenDecl p)

-- instance GHasGenRep f => HasGenRep (f ()) where
--     getGenRoot _ = gGetGenRoot (Proxy :: Proxy (f ()))
--     getGenDecl _ = gGetGenDecl (Proxy :: Proxy (f ()))

class GHasGenRep (f :: * -> *) where
    gGetGenRoot :: Proxy (f a) -> GenTypeFix
    gGetGenDecl :: Proxy (f a) -> Maybe Text

gGetGenRep :: GHasGenRep f => Proxy (f a) -> GenTypeFix
gGetGenRep p = maybe (gGetGenRoot p) (GenTypeFix . GenRef) (gGetGenDecl p)

instance HasGenRep Value where
    getGenRoot _ = GenTypeFix (GenLogic LogicAny)
    getGenDecl _ = Nothing

instance HasGenRep Int where
    getGenRoot _ = GenTypeFix (GenLogic LogicInt)
    getGenDecl _ = Nothing

instance HasStringRep Text where
    getStringRep _ = ()

instance HasGenRep Text where
    getGenRoot _ = GenTypeFix (GenLogic LogicString)
    getGenDecl _ = Nothing

instance HasGenRep Double where
    getGenRoot _ = GenTypeFix (GenLogic LogicDouble)
    getGenDecl _ = Nothing

instance HasGenRep Bool where
    getGenRoot _ = GenTypeFix (GenLogic LogicBool)
    getGenDecl _ = Nothing

instance HasGenRep a => HasGenRep (Maybe a) where
    getGenRoot _ = GenTypeFix (GenLogic (LogicOptional (getGenRep (Proxy :: Proxy a))))
    getGenDecl _ = Nothing

instance HasGenRep a => HasGenRep [a] where
    getGenRoot _ = GenTypeFix (GenLogic (LogicArray (getGenRep (Proxy :: Proxy a))))
    getGenDecl _ = Nothing

instance HasGenRep a => HasGenRep (Seq a) where
    getGenRoot _ = GenTypeFix (GenLogic (LogicArray (getGenRep (Proxy :: Proxy a))))
    getGenDecl _ = Nothing

instance (HasGenRep s, HasGenRep a) => HasGenRep (Map s a) where
    getGenRoot _ = GenTypeFix (GenLogic (LogicMap (getGenRep (Proxy :: Proxy a)) (getGenRep (Proxy :: Proxy a))))
    getGenDecl _ = Nothing

-- Datatype
instance (GHasGenRep f, Datatype c) => GHasGenRep (M1 D c f) where
    gGetGenRoot _ = GenTypeFix (GenPart (PartDatatype (T.pack (datatypeName m)) (gGetGenRep (Proxy :: Proxy (f ())))))
        where m = (undefined :: t c f a)
    gGetGenDecl _ = Just (T.pack (datatypeName m))
        where m = (undefined :: t c f a)

instance (GHasGenRep f, Datatype c) => HasGenRep (M1 D c f Private) where
    getGenRoot _ = gGetGenRoot (Proxy :: Proxy (M1 D c f Private))
    getGenDecl _ = Nothing

-- Constructor Metadata
instance (GHasGenRep f, Constructor c) => GHasGenRep (M1 C c f) where
    gGetGenRoot _ = GenTypeFix (GenPart (PartConstructor (T.pack (conName m)) (gGetGenRep (Proxy :: Proxy (f ())))))
        where m = (undefined :: t c f a)
    gGetGenDecl _ = Nothing

instance (GHasGenRep f, Constructor c) => HasGenRep (M1 C c f Private) where
    getGenRoot _ = gGetGenRoot (Proxy :: Proxy (M1 C c f Private))
    getGenDecl _ = Nothing

-- Selector Metadata
instance (GHasGenRep f, Selector c) => GHasGenRep (M1 S c f) where
    gGetGenRoot _ = GenTypeFix (GenPart (PartSelector (T.pack (selName m)) (gGetGenRep (Proxy :: Proxy (f ())))))
        where m = (undefined :: t c f a)
    gGetGenDecl _ = Nothing

instance (GHasGenRep f, Selector c) => HasGenRep (M1 S c f Private) where
    getGenRoot _ = gGetGenRoot (Proxy :: Proxy (M1 S c f Private))
    getGenDecl _ = Nothing

-- Constructor Paramater
instance HasGenRep a => GHasGenRep (K1 R a) where
    gGetGenRoot _ = GenTypeFix (GenPart (PartCon (getGenRep (undefined :: Proxy a))))
    gGetGenDecl _ = Nothing

instance HasGenRep a => HasGenRep (K1 R a Private) where
    getGenRoot _ = gGetGenRoot (Proxy :: Proxy (K1 R a Private))
    getGenDecl _ = Nothing

-- Sum branch
instance (HasGenRep (f Private), HasGenRep (g Private)) => GHasGenRep (f :+: g) where
    gGetGenRoot _ = GenTypeFix (GenPart (PartSum (getGenRep (Proxy :: Proxy (f Private))) (getGenRep (Proxy :: Proxy (g Private)))))
    gGetGenDecl _ = Nothing

instance (HasGenRep (f Private), HasGenRep (g Private)) => HasGenRep ((f :+: g) Private) where
    getGenRoot _ = gGetGenRoot (Proxy :: Proxy ((f :+: g) Private))
    getGenDecl _ = Nothing

-- Product branch
instance (HasGenRep (f Private), HasGenRep (g Private)) => GHasGenRep (f :*: g) where
    gGetGenRoot _ = GenTypeFix (GenPart (PartProd (getGenRep (Proxy :: Proxy (f Private))) (getGenRep (Proxy :: Proxy (g Private)))))
    gGetGenDecl _ = Nothing

instance (HasGenRep (f Private), HasGenRep (g Private)) => HasGenRep ((f :*: g) Private) where
    getGenRoot _ = gGetGenRoot (Proxy :: Proxy ((f :*: g) Private))
    getGenDecl _ = Nothing

-- Empty branch
instance GHasGenRep U1 where
    gGetGenRoot _ = GenTypeFix (GenPart PartEmpty)
    gGetGenDecl _ = Nothing

instance HasGenRep (U1 Private) where
    getGenRoot _ = gGetGenRoot (Proxy :: Proxy (U1 Private))
    getGenDecl _ = Nothing

-- Wrappers

data GenRepNewtype n o = GenRepNewtype { unGenRepNewtype :: n }

instance (Newtype n, o ~ O n, HasGenRep o) => HasGenRep (GenRepNewtype n o) where
    getGenRoot _ = getGenRoot (Proxy :: Proxy o)
    getGenDecl _ = getGenDecl (Proxy :: Proxy o)

data GenRepInjection a b = GenRepInjection { unGenRepInjection :: a }

instance (Injection a, b ~ InjTarget a, HasGenRep b) => HasGenRep (GenRepInjection a b) where
    getGenRoot _ = getGenRoot (Proxy :: Proxy b)
    getGenDecl _ = getGenDecl (Proxy :: Proxy b)

data GenRepTag a = GenRepTag { unGenRepTag :: a }

instance (Generic a, GHasGenRep (Rep a), HasTagPrefix a) => HasGenRep (GenRepTag a) where
    -- TODO post-process and lop off tag prefixes
    getGenRoot _ = gGetGenRoot (Proxy :: Proxy (Rep a ()))
    getGenDecl _ = gGetGenDecl (Proxy :: Proxy (Rep a ()))

data GenRepFix a (f :: * -> *) = GenRepFix { unGenRepFix :: a }

instance (Newtype a, f a ~ O a, HasGenRep (f a)) => HasGenRep (GenRepFix a f) where
    -- TODO We need GenRep to reflect recursion directly - hide the fixpoint!
    getGenRoot _ = getGenRoot (Proxy :: Proxy (f a))
    getGenDecl _ = getGenDecl (Proxy :: Proxy (f a))