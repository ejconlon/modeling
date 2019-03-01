module Modeling.Data.Mid where

import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Modeling.Data.Core

-- TODO This is probably overkill
-- data MidTypeName =
--       StringTypeName
--     | LongTypeName
--     | DoubleTypeName
--     | OptionalTypeName
--     | ListTypeName
--     | StringMapTypeName
--     | StructTypeName
--     | ReferenceTypeName
--     deriving (Generic, Eq, Show)

-- midTypeNameToText :: ErrorMsgInjection ErrorMsg MidTypeName Text
-- midTypeNameToText = Injection apply invert where
--     apply t =
--         case t of
--             StringTypeName -> "string"
--             LongTypeName -> "long"
--             DoubleTypeName -> "double"
--             OptionalTypeName -> "optional"
--             ListTypeName -> "list"
--             StringMapTypeName -> "stringmap"
--             StructTypeName -> "struct"
--             ReferenceTypeName -> "reference"
--     invert u =
--         case u of
--             "string" -> Right StringTypeName
--             "long" -> Right LongTypeName
--             "double" -> Right DoubleTypeName
--             "optional" -> Right OptionalTypeName
--             "list" -> Right ListTypeName
--             "stringmap" -> Right StringMapTypeName
--             "struct" -> Right StructTypeName
--             "reference" -> Right ReferenceTypeName
--             _ -> Left (ErrorMsg ("Unknown type name " <> u))

-- instance ToJSON MidTypeName where
--     toJSON = injectionToJSON midTypeNameToText

-- instance FromJSON MidTypeName where
--     parseJSON = injectionParseJSON midTypeNameToText

data MidTypeSingleAttrs = MidTypeSingleAttrs
    { ty :: MidType
    } deriving (Generic, Show, Eq)

instance ToJSON MidTypeSingleAttrs
instance FromJSON MidTypeSingleAttrs

data MidTypeReferenceAttrs = MidTypeReferenceAttrs
    { name :: Text
    } deriving (Generic, Show, Eq)

instance ToJSON MidTypeReferenceAttrs
instance FromJSON MidTypeReferenceAttrs

data MidTypeStructAttrs = MidTypeStructAttrs
    { fields :: Map Text MidType
    } deriving (Generic, Show, Eq)

instance ToJSON MidTypeStructAttrs
instance FromJSON MidTypeStructAttrs

data MidTypeAttrs = MidTypeAttrs
    { optional :: Maybe MidTypeSingleAttrs
    , list :: Maybe MidTypeSingleAttrs
    , stringmap :: Maybe MidTypeSingleAttrs
    , struct :: Maybe (Map Text MidType)
    , reference :: Maybe MidTypeReferenceAttrs
    } deriving (Generic, Show, Eq)

instance ToJSON MidTypeAttrs
instance FromJSON MidTypeAttrs

data MidType = MidType
    { name :: Text
    , attributes :: Maybe MidTypeAttrs
    } deriving (Generic, Show, Eq)

instance ToJSON MidType
instance FromJSON MidType


-- data MidModelName =
--       DirectModelName
--     | EmbeddedModelName
--     | SerialModelName
--     -- | ParallelModelName
--     -- | AdaptorModelName
--     | SplitModelName
--     deriving (Generic, Show, Eq)

-- midTypeNameToText :: ErrorMsgInjection ErrorMsg MidTypeName Text
-- midTypeNameToText = Injection apply invert where
--     apply t =
--         case t of
--             StringTypeName -> "string"
--             LongTypeName -> "long"
--             DoubleTypeName -> "double"
--             OptionalTypeName -> "optional"
--             ListTypeName -> "list"
--             StringMapTypeName -> "stringmap"
--             StructTypeName -> "struct"
--             ReferenceTypeName -> "reference"
--     invert u =
--         case u of
--             "string" -> Right StringTypeName
--             "long" -> Right LongTypeName
--             "double" -> Right DoubleTypeName
--             "optional" -> Right OptionalTypeName
--             "list" -> Right ListTypeName
--             "stringmap" -> Right StringMapTypeName
--             "struct" -> Right StructTypeName
--             "reference" -> Right ReferenceTypeName
--             _ -> Left (ErrorMsg ("Unknown type name " <> u))

data MidModelAttributes = MidModelAttributes
    {
    } deriving (Generic, Show, Eq)

data MidModel = MidModel
    { name :: Text
    , attributes :: Maybe MidModelAttributes
    } deriving (Generic, Show, Eq)
