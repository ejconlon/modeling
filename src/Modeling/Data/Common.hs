module Modeling.Data.Common where

import Control.Newtype.Generics
import Data.Aeson
import Data.Sequence (Seq)
import Data.String   (IsString)
import Data.Text     (Text)
import GHC.Generics  (Generic)
import Modeling.Data.Aeson
import Modeling.Data.Generics

newtype NamespacePart = NamespacePart { unNamespacePart :: Text }
    deriving (Generic, Show, Eq, Ord, IsString, ToJSONKey, FromJSONKey)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonNewtype NamespacePart Text)
    deriving (HasGenRep) via (GenRepNewtype NamespacePart Text)

instance Newtype NamespacePart

newtype Namespace = Namespace { unNamespace :: Seq NamespacePart }
    deriving (Generic, Show, Eq, Semigroup, Monoid)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonNewtype Namespace (Seq NamespacePart))
    deriving (HasGenRep) via (GenRepNewtype Namespace (Seq NamespacePart))

instance Newtype Namespace

newtype ParamName = ParamName { unParamName :: Text }
    deriving (Generic, Show, Eq, Ord, IsString, ToJSONKey, FromJSONKey)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonNewtype ParamName Text)
    deriving (HasGenRep) via (GenRepNewtype ParamName Text)

instance Newtype ParamName

newtype ElementName = ElementName { unElementName :: Text }
    deriving (Generic, Show, Eq, Ord, IsString, ToJSONKey, FromJSONKey)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonNewtype ElementName Text)
    deriving (HasGenRep) via (GenRepNewtype ParamName Text)

instance Newtype ElementName

newtype TypeName = TypeName { unTypeName :: Text }
    deriving (Generic, Show, Eq, Ord, IsString, ToJSONKey, FromJSONKey)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonNewtype TypeName Text)
    deriving (HasGenRep) via (GenRepNewtype TypeName Text)

instance Newtype TypeName

newtype EnumName = EnumName { unEnumName :: Text }
    deriving (Generic, Show, Eq, Ord, IsString, ToJSONKey, FromJSONKey)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonNewtype EnumName Text)
    deriving (HasGenRep) via (GenRepNewtype EnumName Text)

instance Newtype EnumName

newtype FieldName = FieldName { unFieldName :: Text }
    deriving (Generic, Show, Eq, Ord, IsString, ToJSONKey, FromJSONKey)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonNewtype FieldName Text)
    deriving (HasGenRep) via (GenRepNewtype FieldName Text)

instance Newtype FieldName

newtype BranchName = BranchName { unBranchName :: Text }
    deriving (Generic, Show, Eq, Ord, IsString, ToJSONKey, FromJSONKey)
    deriving (HasJSONOptions, ToJSON, FromJSON) via (AesonNewtype BranchName Text)
    deriving (HasGenRep) via (GenRepNewtype BranchName Text)

instance Newtype BranchName
