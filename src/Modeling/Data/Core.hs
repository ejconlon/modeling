module Modeling.Data.Core where

import Data.Aeson
import GHC.Generics (Generic, Generic1)
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
    } deriving (Generic1, Show, Eq, Functor, Foldable, Traversable)
      deriving (ToJSON1, FromJSON1) via (AesonWrapper1 Connection)

data Space a = Space
    { connection :: Connection a
    , element :: a
    } deriving (Generic1, Show, Eq, Functor, Foldable, Traversable)
      deriving (ToJSON1, FromJSON1) via (AesonWrapper1 Space)

data Signature = Signature
    { inputs :: Maybe (Map ParamName TypeFix)
    , outputs :: Maybe (Map ParamName TypeFix)
    , tydefs :: Maybe (Map TypeName TypeFix)
    } deriving (Generic, Show, Eq)
      deriving (ToJSON, FromJSON) via (AesonWrapper Signature)

data Bundle a = Bundle
    { signature :: Maybe Signature
    , root :: a
    } deriving (Generic1, Show, Eq)
      deriving (ToJSON1, FromJSON1) via (AesonWrapper1 Bundle)
