module Modeling.Core where

import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, withReaderT)
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

data ParamOps d (m :: * -> *) = ParamOps
    { externalOp :: ParamName -> Type -> m (IntParam d)
    , internalOp :: Type -> m (IntParam d)
    , literalOp :: Value -> m (IntParam d)
    }

-- external :: (HasParamOps r d m, MonadReader r m)

-- ReaderT r m t
type Builder r (m :: * -> *) t = ReaderT r m t

convertBuilder :: (r -> s) -> Builder s m t -> Builder r m t
convertBuilder = withReaderT

type Context r (m :: * -> *) t u = Builder r m t -> m u

data BaseOps r d (m :: * -> *) t = BaseOps
    { directOp :: BaseOpts d -> DirectName -> m (IntModel d)
    , embedOp :: BaseOpts d -> Builder r m t -> m (IntModel d)
    , buildOp :: IntModel d -> m t
    }

convertBaseOps :: (r -> s) -> BaseOps r d m t -> BaseOps s d m t
convertBaseOps f (BaseOps { directOp, embedOp, buildOp }) = BaseOps
    { directOp = directOp
    , embedOp = \opts builder -> embedOp opts (convertBuilder f builder)
    , buildOp = buildOp
    }

data ExtOps d (m :: * -> *) = ExtOps
    { serialOp :: Seq (IntModel d) -> m (IntModel d)
    , splitOp :: SplitOpts -> (IntModel d) -> m (IntModel d)
    }

data FullOps d (m :: * -> *) t = FullOps
    { fullParamOps :: ParamOps d m
    , fullBaseOps :: BaseOps (FullOps d m t) d m t
    , fullExtOps :: ExtOps d m
    }

data PartialOps d (m :: * -> *) t = PartialOps
    { partialParamOps :: ParamOps d m
    , partialBaseOps :: BaseOps (PartialOps d m t) d m t
    }

fullToPartialOps :: FullOps d m t -> PartialOps d m t
fullToPartialOps (FullOps {fullParamOps, fullBaseOps}) =
    PartialOps fullParamOps (convertBaseOps fullToPartialOps fullBaseOps)

simpleBuilder :: (r ~ PartialOps d m t, Monad m) => Builder r m t
simpleBuilder = do
    PartialOps {..} <- ask
    simpleParam <- lift ((externalOp partialParamOps) "simpleExternalParam" StringType)
    let simpleOpts = BaseOpts "simpleNs" (Map.singleton "simpleParamInternal" simpleParam) Map.empty Seq.empty
    simpleModel <- lift ((directOp partialBaseOps) simpleOpts "simpleModel")
    lift ((buildOp partialBaseOps) simpleModel)

complexBuilder :: (r ~ FullOps d m t, Monad m) => Builder r m t
complexBuilder = do
    FullOps {..} <- ask
    let firstOpts = BaseOpts "firstNs" Map.empty Map.empty Seq.empty
    firstModel <- lift ((directOp fullBaseOps) firstOpts "firstModel")
    let secondOpts = BaseOpts "secondNs" Map.empty Map.empty Seq.empty
    secondModel <- lift ((embedOp fullBaseOps) secondOpts (convertBuilder fullToPartialOps simpleBuilder))
    serialModel <- lift ((serialOp fullExtOps) (fromList [firstModel, secondModel]))
    let splitOpts = SplitOpts { attribute = "region", values = fromList ["SFO", "LAX"], other = Just "OTHER" }
    splitModel <- lift ((splitOp fullExtOps) splitOpts serialModel)
    lift ((buildOp fullBaseOps) splitModel)
