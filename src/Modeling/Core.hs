{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Modeling.Core where

import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Control.Monad.Trans (lift)
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

data family IntModel d
data family IntParam d

data BaseOpts d = BaseOpts
    { namespace :: NamespacePart
    , params :: Map ParamName (IntParam d)
    , namedSubs :: Map SubName (IntModel d)
    , unnamedSubs :: Seq (IntModel d)
    } deriving (Generic)

data SplitOpts = SplitOpts
    { attribute :: AttributeName
    , values :: Seq AttributeValue
    , other :: Maybe AttributeValue
    } deriving (Generic, Show, Eq)

data ParamDSL d (m :: * -> *) = ParamDSL
    { external :: ParamName -> Type -> m (IntParam d)
    , internal :: Type -> m (IntParam d)
    , literal :: Value -> m (IntParam d)
    }

-- ReaderT r m t
type Builder r (m :: * -> *) t = ReaderT r m t

type Context r (m :: * -> *) t u = Builder r m t -> m u

data BaseDSL r d (m :: * -> *) t = BaseDSL
    { direct :: BaseOpts d -> DirectName -> m (IntModel d)
    , embed :: BaseOpts d -> Builder r m t -> m (IntModel d)
    , build :: IntModel d -> m t
    }

data ExtDSL d (m :: * -> *) = ExtDSL
    { serial :: Seq (IntModel d) -> m (IntModel d)
    , split :: SplitOpts -> (IntModel d) -> m (IntModel d)
    }

data FullDSL d (m :: * -> *) t = FullDSL
    { param :: ParamDSL d m
    , base :: BaseDSL (FullDSL d m t) d m t
    , ext :: ExtDSL d m
    }

simpleBuilder :: (r ~ FullDSL d m t, MonadReader r m) => Builder r m t
simpleBuilder = undefined

complexBuilder :: (r ~ FullDSL d m t, MonadReader r m) => Builder r m t
complexBuilder = do
    dsl <- ask
    let firstOpts = BaseOpts "firstNs" Map.empty Map.empty Seq.empty
    firstModel <- lift ((direct (base dsl)) firstOpts "firstOp")
    let secondOpts = BaseOpts "secondNs" Map.empty Map.empty Seq.empty
    secondModel <- lift ((embed (base dsl)) secondOpts simpleBuilder)
    serialModel <- lift ((serial (ext dsl)) (fromList [firstModel, secondModel]))
    let splitOpts = SplitOpts { attribute = "region", values = fromList ["SFO", "LAX"], other = Just "OTHER" }
    splitOp <- lift ((split (ext dsl)) splitOpts serialModel)
    lift ((build (base dsl)) splitOp)
