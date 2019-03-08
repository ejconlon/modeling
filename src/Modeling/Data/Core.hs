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

data Space a = Space
    { nspart :: NamespacePart
    , inputs :: Maybe (Map ParamName Param)
    , element :: a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance HasJSONOptions (Space a) where getJSONOptions _ = recordOptions
deriving via (AesonWrapper (Space a)) instance ToJSON a => ToJSON (Space a)
deriving via (AesonWrapper (Space a)) instance FromJSON a => FromJSON (Space a)

data Signature = Signature
    { inputs :: Maybe (Map ParamName TypeFix)
    , outputs :: Maybe (Map ParamName TypeFix)
    , tydefs :: Maybe (Map TypeName TypeFix)
    , external :: Maybe (Seq TypeName)
    } deriving (Generic, Show, Eq)
      deriving (ToJSON, FromJSON) via (AesonWrapper Signature)

instance HasJSONOptions Signature where getJSONOptions _ = recordOptions

data Bundle a = Bundle
    { signature :: Maybe Signature
    , root :: a
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance HasJSONOptions (Bundle a) where getJSONOptions _ = recordOptions
deriving via (AesonWrapper (Bundle a)) instance ToJSON a => ToJSON (Bundle a)
deriving via (AesonWrapper (Bundle a)) instance FromJSON a => FromJSON (Bundle a)
