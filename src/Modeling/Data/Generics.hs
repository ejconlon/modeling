{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Modeling.Data.Generics where

import Control.Newtype.Generics
import Data.Proxy (Proxy (..))
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

data GenType d a =
      GenRef Text
    | GenEmbed (d a)
    | GenPart (Part a)
    deriving (Show, Eq)

data GenTypeFix d = GenTypeFix { unGenTypeFix :: GenType d (GenTypeFix d) }

deriving instance Show (d (GenTypeFix d)) => Show (GenTypeFix d)
deriving instance Eq (d (GenTypeFix d)) => Eq (GenTypeFix d)

class HasGenRep (d :: * -> *) a where
    getGenRoot :: Proxy (d a) -> GenTypeFix d
    getGenDecl :: Proxy (d a) -> Maybe Text

    default getGenRoot :: (Generic a, GHasGenRep d (Rep a)) => Proxy (d a) -> GenTypeFix d
    getGenRoot _ = gGetGenRoot (Proxy :: Proxy (d (Rep a Private)))

    default getGenDecl :: (Generic a, GHasGenRep d (Rep a)) => Proxy (d a) -> Maybe Text
    getGenDecl _ = gGetGenDecl (Proxy :: Proxy (d (Rep a Private)))

getGenRep :: HasGenRep d a => Proxy (d a) -> GenTypeFix d
getGenRep p = maybe (getGenRoot p) (GenTypeFix . GenRef) (getGenDecl p)

class GHasGenRep (d :: * -> *) (f :: * -> *) where
    gGetGenRoot :: Proxy (d (f a)) -> GenTypeFix d
    gGetGenDecl :: Proxy (d (f a)) -> Maybe Text

gGetGenRep :: GHasGenRep d f => Proxy (d (f a)) -> GenTypeFix d
gGetGenRep p = maybe (gGetGenRoot p) (GenTypeFix . GenRef) (gGetGenDecl p)

-- Datatype
instance (GHasGenRep d f, Datatype c) => GHasGenRep d (M1 D c f) where
    gGetGenRoot _ = GenTypeFix (GenPart (PartDatatype (T.pack (datatypeName m)) (gGetGenRep (Proxy :: Proxy (d (f Private))))))
        where m = (undefined :: t c f a)
    gGetGenDecl _ = Just (T.pack (datatypeName m))
        where m = (undefined :: t c f a)

instance (GHasGenRep d f, Datatype c) => HasGenRep d (M1 D c f Private) where
    getGenRoot _ = gGetGenRoot (Proxy :: Proxy (d (M1 D c f Private)))
    getGenDecl _ = Nothing

-- Constructor Metadata
instance (GHasGenRep d f, Constructor c) => GHasGenRep d (M1 C c f) where
    gGetGenRoot _ = GenTypeFix (GenPart (PartConstructor (T.pack (conName m)) (gGetGenRep (Proxy :: Proxy (d (f Private))))))
        where m = (undefined :: t c f a)
    gGetGenDecl _ = Nothing

instance (GHasGenRep d f, Constructor c) => HasGenRep d (M1 C c f Private) where
    getGenRoot _ = gGetGenRoot (Proxy :: Proxy (d (M1 C c f Private)))
    getGenDecl _ = Nothing

-- Selector Metadata
instance (GHasGenRep d f, Selector c) => GHasGenRep d (M1 S c f) where
    gGetGenRoot _ = GenTypeFix (GenPart (PartSelector (T.pack (selName m)) (gGetGenRep (Proxy :: Proxy (d (f Private))))))
        where m = (undefined :: t c f a)
    gGetGenDecl _ = Nothing

instance (GHasGenRep d f, Selector c) => HasGenRep d (M1 S c f Private) where
    getGenRoot _ = gGetGenRoot (Proxy :: Proxy (d (M1 S c f Private)))
    getGenDecl _ = Nothing

-- Constructor Paramater
instance HasGenRep d a => GHasGenRep d (K1 R a) where
    gGetGenRoot _ = GenTypeFix (GenPart (PartCon (getGenRep (Proxy :: Proxy (d a)))))
    gGetGenDecl _ = Nothing

instance HasGenRep d a => HasGenRep d (K1 R a Private) where
    getGenRoot _ = gGetGenRoot (Proxy :: Proxy (d (K1 R a Private)))
    getGenDecl _ = Nothing

-- Sum branch
instance (HasGenRep d (f Private), HasGenRep d (g Private)) => GHasGenRep d (f :+: g) where
    gGetGenRoot _ = GenTypeFix (GenPart (PartSum (getGenRep (Proxy :: Proxy (d (f Private)))) (getGenRep (Proxy :: Proxy (d (g Private))))))
    gGetGenDecl _ = Nothing

instance (HasGenRep d (f Private), HasGenRep d (g Private)) => HasGenRep d ((f :+: g) Private) where
    getGenRoot _ = gGetGenRoot (Proxy :: Proxy (d ((f :+: g) Private)))
    getGenDecl _ = Nothing

-- Product branch
instance (HasGenRep d (f Private), HasGenRep d (g Private)) => GHasGenRep d (f :*: g) where
    gGetGenRoot _ = GenTypeFix (GenPart (PartProd (getGenRep (Proxy :: Proxy (d (f Private)))) (getGenRep (Proxy :: Proxy (d (g Private))))))
    gGetGenDecl _ = Nothing

instance (HasGenRep d (f Private), HasGenRep d (g Private)) => HasGenRep d ((f :*: g) Private) where
    getGenRoot _ = gGetGenRoot (Proxy :: Proxy (d ((f :*: g) Private)))
    getGenDecl _ = Nothing

-- Empty branch
instance GHasGenRep d U1 where
    gGetGenRoot _ = GenTypeFix (GenPart PartEmpty)
    gGetGenDecl _ = Nothing

instance HasGenRep d (U1 Private) where
    getGenRoot _ = gGetGenRoot (Proxy :: Proxy (d (U1 Private)))
    getGenDecl _ = Nothing

-- Wrappers

data GenRepNewtype n o = GenRepNewtype { unGenRepNewtype :: n }

instance (Newtype n, o ~ O n, HasGenRep d o) => HasGenRep d (GenRepNewtype n o) where
    getGenRoot _ = getGenRoot (Proxy :: Proxy (d o))
    getGenDecl _ = getGenDecl (Proxy :: Proxy (d o))

data GenRepInjection a b = GenRepInjection { unGenRepInjection :: a }

instance (Injection a, b ~ InjTarget a, HasGenRep d b) => HasGenRep d (GenRepInjection a b) where
    getGenRoot _ = getGenRoot (Proxy :: Proxy (d b))
    getGenDecl _ = getGenDecl (Proxy :: Proxy (d b))

data GenRepTag a = GenRepTag { unGenRepTag :: a }

instance (Generic a, GHasGenRep d (Rep a), HasTagPrefix a) => HasGenRep d (GenRepTag a) where
    -- TODO post-process and lop off tag prefixes
    getGenRoot _ = gGetGenRoot (Proxy :: Proxy (d (Rep a ())))
    getGenDecl _ = gGetGenDecl (Proxy :: Proxy (d (Rep a ())))

data GenRepFix a (f :: * -> *) = GenRepFix { unGenRepFix :: a }

instance (Newtype a, f a ~ O a, HasGenRep d (f a)) => HasGenRep d (GenRepFix a f) where
    -- TODO We need GenRep to reflect recursion directly - hide the fixpoint!
    getGenRoot _ = getGenRoot (Proxy :: Proxy (d (f a)))
    getGenDecl _ = getGenDecl (Proxy :: Proxy (d (f a)))