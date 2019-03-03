module Modeling.Ops.Core where

import Control.Monad.Reader (ReaderT, withReaderT)
import Data.Aeson (Value)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Modeling.Data.Common
import Modeling.Data.Type

type DirectName = Text
type SubName = Text

data family IntModel d
data family IntParam d

data BaseOpts d = BaseOpts
    { params :: Map ParamName (IntParam d)
    , namedSubs :: Map SubName (IntModel d)
    , unnamedSubs :: Seq (IntModel d)
    }

data ParamOps d (m :: * -> *) = ParamOps
    { externalOp :: Namespace -> ParamName -> TypeFix -> m (IntParam d)
    , internalOp :: TypeFix -> m (IntParam d)
    , literalOp :: Value -> m (IntParam d)
    }

-- ReaderT r m t
type Builder r (m :: * -> *) t = ReaderT r m t

convertBuilder :: (r -> s) -> Builder s m t -> Builder r m t
convertBuilder = withReaderT

type Context r (m :: * -> *) t u = Builder r m t -> m u

data BaseOps d (m :: * -> *) t r = BaseOps
    { directOp :: NamespacePart -> BaseOpts d -> DirectName -> m (IntModel d)
    , embedOp :: NamespacePart -> BaseOpts d -> Builder r m t -> m (IntModel d)
    , buildOp :: IntModel d -> m t
    }

convertBaseOps :: (r -> s) -> BaseOps d m t r -> BaseOps d m t s
convertBaseOps f (BaseOps { directOp, embedOp, buildOp }) = BaseOps
    { directOp = directOp
    , embedOp = \nsp opts builder -> embedOp nsp opts (convertBuilder f builder)
    , buildOp = buildOp
    }
