module Modeling.Data.Common where

import Data.Aeson
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics (Generic)

-- newtype NamespacePart = NamespacePart { unNamespacePart :: Text } deriving (Show, Eq, Ord, IsString)
-- newtype ParamName = ParamName { unParamName :: Text } deriving (Show, Eq, Ord, IsString)
-- type Namespace = Seq NamespacePart

type NamespacePart = Text
type Namespace = Seq NamespacePart
type ParamName = Text

data Connection a = Connection
    { nspart :: NamespacePart
    , named :: Maybe (Map Text a)
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
