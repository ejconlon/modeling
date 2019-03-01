{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Modeling where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq, fromList)
import qualified Data.Sequence as Seq
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
    deriving (Generic, Show, Eq)

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

type DirectName = Text
type SubName = Text

type AttributeName = Text
type AttributeValue = Text

data Param = Param
    { namespace :: Namespace
    , name :: ParamName
    } deriving (Generic, Show, Eq)

data BaseOpts o p = BaseOpts
    { namespace :: NamespacePart
    , params :: Map ParamName p
    , namedSubs :: Map SubName o
    , unnamedSubs :: Seq o
    } deriving (Generic)

data SplitOpts = SplitOpts
    { attribute :: AttributeName
    , values :: Seq AttributeValue
    , other :: Maybe AttributeValue
    } deriving (Generic, Show, Eq)

data ParamDSL p (m :: * -> *) = ParamDSL
    { external :: ParamName -> Type -> m p
    , internal :: Type -> m p
    , literal :: Value -> m p
    } deriving (Generic)

-- ReaderT r m t
type Builder r (m :: * -> *) t = r -> m t

type Context r (m :: * -> *) t u = Builder r m t -> m u

data BaseDSL r o p (m :: * -> *) t = BaseDSL
    { direct :: BaseOpts o p -> DirectName -> m o
    , embed :: BaseOpts o p -> Builder r m t -> m o
    , build :: o -> m t
    } deriving (Generic)

data ExtDSL o (m :: * -> *) = ExtDSL
    { serial :: Seq o -> m o
    , split :: SplitOpts -> o -> m o
    } deriving (Generic)

data FullDSL o p m t = FullDSL
    { param :: ParamDSL p m
    , base :: BaseDSL (FullDSL o p m t) o p m t
    , ext :: ExtDSL o m
    } deriving (Generic)

type FullBuilder o p m = forall t. Builder (FullDSL o p m t) m t

simpleBuilder :: Monad m => FullBuilder o p m
simpleBuilder dsl = undefined

complexBuilder :: Monad m => FullBuilder o p m
complexBuilder dsl = do
    let firstOpts = BaseOpts "firstNs" Map.empty Map.empty Seq.empty
    firstModel <- (direct (base dsl)) firstOpts "firstOp"
    let secondOpts = BaseOpts "secondNs" Map.empty Map.empty Seq.empty
    secondModel <- (embed (base dsl)) secondOpts simpleBuilder
    serialModel <- (serial (ext dsl)) (fromList [firstModel, secondModel])
    let splitOpts = SplitOpts { attribute = "region", values = fromList ["SFO", "LAX"], other = Just "OTHER" }
    splitOp <- (split (ext dsl)) splitOpts serialModel
    (build (base dsl)) splitOp

main :: IO ()
main = putStrLn "hello, world"
