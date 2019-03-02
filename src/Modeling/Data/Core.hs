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

data Type =
      StringType
    | LongType
    | DoubleType
    | OptionalType Type
    | ListType Type
    | StringMapType Type
    | StructType (Map Text Type)
    | ReferenceType Text
    | EnumType (Seq Text)
    deriving (Generic, Show, Eq)

data ExtParam = ExtParam
    { ns :: Namespace
    , name :: ParamName
    , ty :: Type
    } deriving (Generic, Show, Eq)

data Interface = Interface
    { params :: Seq ExtParam
    , tydefs :: Map Text Type
    } deriving (Generic, Show, Eq)

data Bundle a = Bundle
    { interface :: Interface
    , root :: a
    } deriving (Generic, Show, Eq)
