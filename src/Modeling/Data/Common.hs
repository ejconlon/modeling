module Modeling.Data.Common where

import Data.Sequence (Seq)
import Data.Text (Text)

-- newtype NamespacePart = NamespacePart { unNamespacePart :: Text } deriving (Show, Eq, Ord, IsString)
-- newtype ParamName = ParamName { unParamName :: Text } deriving (Show, Eq, Ord, IsString)
-- type Namespace = Seq NamespacePart

type NamespacePart = Text
type Namespace = Seq NamespacePart
type ParamName = Text
type ElementName = Text
