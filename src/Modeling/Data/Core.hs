module Modeling.Data.Core where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Modeling.Data.Common
import Modeling.Data.Param
import Modeling.Data.Type

data ExtParam = ExtParam
    { ns :: Namespace
    , name :: ParamName
    , ty :: TypeFix
    } deriving (Generic, Show, Eq)

instance ToJSON ExtParam
instance FromJSON ExtParam

data Connection a = Connection
    { nspart :: NamespacePart
    , inputs :: Maybe (Map ParamName Param)
    , outputs :: Maybe (Map ParamName TypeFix)
    , named :: Maybe (Map ElementName a)
    , additional :: Maybe (Seq a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Connection a)
instance FromJSON a => FromJSON (Connection a)

data Space a = Space
    { connection :: Connection a
    , element :: a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Space a)
instance FromJSON a => FromJSON (Space a)

data Interface = Interface
    { params :: Seq ExtParam
    , tydefs :: Map Text TypeFix
    } deriving (Generic, Show, Eq)

instance ToJSON Interface
instance FromJSON Interface

data Bundle a = Bundle
    { interface :: Interface
    , root :: a
    } deriving (Generic, Show, Eq)

instance ToJSON a => ToJSON (Bundle a)
instance FromJSON a => FromJSON (Bundle a)
