{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Modeling where

import Data.Aeson
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics

data Type =
      StringType
    | LongType
    | DoubleType
    | OptionalType Type
    | ListType Type
    | MapType Type
    | StructType (Map Text Type)
    | ReferenceType Text
    deriving (Show, Eq)

data GenericType = GenericType
    { name :: Text
    , argument :: Maybe GenericType
    , struct :: Maybe (Map Text GenericType)
    , reference :: Maybe Text
    } deriving (Generic, Show, Eq)

instance ToJSON GenericType
instance FromJSON GenericType

-- Relationship between X and GenericX: the serialized version of X can round-trip
-- through the inverse round trip of GenericX

instance ToJSON Type where
    toJSON t =
        case t of
            StringType -> object ["name" .= ("string" :: Text)]
            LongType -> object ["name" .= ("long" :: Text)]
            DoubleType -> object ["name" .= ("double" :: Text)]
            OptionalType a -> object ["name" .= ("optional" :: Text), "argument" .= a]
            ListType a -> object ["name" .= ("list" :: Text), "argument" .= a]
            MapType a -> object ["name" .= ("map" :: Text), "argument" .= a]
            StructType ma -> object ["name" .= ("struct" :: Text), "struct" .= ma]
            ReferenceType n -> object ["name" .= ("reference" :: Text), "reference" .= n]

-- TODO
-- instance FromJSON Type where

-- newtype NamespacePart = NamespacePart { unNamespacePart :: Text } deriving (Show, Eq, Ord, IsString)
-- newtype ParamName = ParamName { unParamName :: Text } deriving (Show, Eq, Ord, IsString)
-- type Namespace = Seq NamespacePart

type NamespacePart = Text
type Namespace = Seq NamespacePart
type ParamName = Text

data Param = Param
    { namespace :: Namespace
    , name :: ParamName
    } deriving (Show, Eq)

class DSL m t where
    type InternalOp m
    type InternalParam m

    externalParam :: ParamName -> m (InternalParam m)

    build :: InternalOp m -> m t

type Builder m = forall t. DSL m t => m t

type Context m u = Builder m -> m u

main :: IO ()
main = putStrLn "hello, world"
