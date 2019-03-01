module Modeling.Data.Core where

import GHC.Generics (Generic)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)

-- newtype NamespacePart = NamespacePart { unNamespacePart :: Text } deriving (Show, Eq, Ord, IsString)
-- newtype ParamName = ParamName { unParamName :: Text } deriving (Show, Eq, Ord, IsString)
-- type Namespace = Seq NamespacePart

type NamespacePart = Text
type Namespace = Seq NamespacePart
type ParamName = Text

data Param = Param
    { namespace :: Namespace
    , name :: ParamName
    } deriving (Generic, Show, Eq)

data Type =
      StringType
    | LongType
    | DoubleType
    | OptionalType Type
    | ListType Type
    | StringMapType Type
    | StructType (Map Text Type)
    | ReferenceType Text
    deriving (Generic, Show, Eq)

-- Relationship between X and GenericX: the serialized version of X can round-trip
-- through the inverse round trip of GenericX

-- TODO consider not defining serializers, just convert to/from generics

-- instance ToJSON Type where
--     toJSON t =
--         case t of
--             StringType -> object ["name" .= ("string" :: Text)]
--             LongType -> object ["name" .= ("long" :: Text)]
--             DoubleType -> object ["name" .= ("double" :: Text)]
--             -- TODO fix up the rest of these
--             OptionalType a -> object ["name" .= ("optional" :: Text), "argument" .= a]
--             ListType a -> object ["name" .= ("list" :: Text), "argument" .= a]
--             MapType a -> object ["name" .= ("map" :: Text), "argument" .= a]
--             StructType ma -> object ["name" .= ("struct" :: Text), "struct" .= ma]
--             ReferenceType n -> object ["name" .= ("reference" :: Text), "reference" .= n]

-- TODO
-- instance FromJSON Type where
