module Modeling.Typecheck where

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Modeling.Func
import Modeling.Data.Common
import Modeling.Data.Core
import Modeling.Data.Model
import Modeling.Data.Param
import Modeling.Data.Outside

data TypeError = Boom deriving (Generic, Show, Eq)

data TypeEnv = TypeEnv deriving (Generic, Show, Eq)

type TypeC m = (MonadReader TypeEnv m, MonadError TypeError m)
newtype TypeT m a = EvalT { unEvalT :: FuncT TypeEnv () TypeError m a }
    deriving (Functor, Applicative, Monad, MonadReader TypeEnv, MonadError TypeError)

proof :: Monad m => (forall n. TypeC n => n a) -> TypeT m a
proof = id

data ModelType = ModelType deriving (Generic, Show, Eq)

inferModelType :: TypeC m => Model (ModelSpaceFix) -> m ModelType
inferModelType = undefined

withNsPart :: TypeC m => NamespacePart -> m a -> m a
withNsPart = undefined

checkInputs :: ModelType -> Map ParamName Param -> m ()
checkInputs = undefined

checkDependencies :: ModelType -> Dependencies ModelSpaceFix -> m ()
checkDependencies = undefined

checkModel :: TypeC m => ModelSpaceFix -> m ()
checkModel (ModelSpaceFix (ModelSpace Space {..})) = do
    let DepModel {..} = element
    withNsPart nspart $ do
        modelType <- inferModelType model
        traverse_ (checkInputs modelType) inputs
        traverse_ (checkDependencies modelType) dependencies

checkBundle :: ModelSpaceBundle -> Either TypeError ()
checkBundle = undefined
