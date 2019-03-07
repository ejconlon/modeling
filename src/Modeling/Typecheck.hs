module Modeling.Typecheck where

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Data.Void
import GHC.Generics (Generic)
import Modeling.Func
import Modeling.Data.Outside

data TypeError = Boom deriving (Generic, Show, Eq)

data TypeEnv = TypeEnv deriving (Generic, Show, Eq)

type TypeC m = (MonadReader TypeEnv m, MonadError TypeError m)
newtype TypeT m a = EvalT { unEvalT :: FuncT TypeEnv () TypeError m a }
    deriving (Functor, Applicative, Monad, MonadReader TypeEnv, MonadError TypeError)

proof :: Monad m => (forall n. TypeC n => n a) -> TypeT m a
proof = id

checkModel :: TypeC m => ModelSpaceFix -> m ()
checkModel = undefined

checkBundle :: ModelSpaceBundle -> Either TypeError ()
checkBundle = undefined
