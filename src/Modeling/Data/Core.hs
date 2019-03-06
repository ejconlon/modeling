module Modeling.Data.Core where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Modeling.Data.Aeson
import Modeling.Data.Common
import Modeling.Data.Param
import Modeling.Data.Type

data Connection a = Connection
    { nspart :: NamespacePart
    , inputs :: Maybe (Map ParamName Param)
    , named :: Maybe (Map ElementName a)
    , additional :: Maybe (Seq a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

deriving via (AesonWrapper (Connection a)) instance ToJSON a => ToJSON (Connection a)
deriving via (AesonWrapper (Connection a)) instance FromJSON a => FromJSON (Connection a)

data Space a = Space
    { connection :: Connection a
    , element :: a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

deriving via (AesonWrapper (Space a)) instance ToJSON a => ToJSON (Space a)
deriving via (AesonWrapper (Space a)) instance FromJSON a => FromJSON (Space a)

data Signature = Signature
    { inputs :: Maybe (Map ParamName TypeFix)
    , outputs :: Maybe (Map ParamName TypeFix)
    , tydefs :: Maybe (Map TypeName TypeFix)
    } deriving (Generic, Show, Eq)
      deriving (ToJSON, FromJSON) via (AesonWrapper Signature)

data Bundle a = Bundle
    { signature :: Maybe Signature
    , root :: a
    } deriving (Generic, Show, Eq)

deriving via (AesonWrapper (Bundle a)) instance ToJSON a => ToJSON (Bundle a)
deriving via (AesonWrapper (Bundle a)) instance FromJSON a => FromJSON (Bundle a)
