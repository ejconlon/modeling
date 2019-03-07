module Modeling.Typecheck where

import Control.Lens (view)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Data.Foldable (traverse_)
import Data.Generics.Product (field)
import Data.Map (Map)
import Data.Sequence (Seq, (|>))
import GHC.Generics (Generic)
import Modeling.Func
import Modeling.Data.Common
import Modeling.Data.Core
import Modeling.Data.Model
import Modeling.Data.Param
import Modeling.Data.Outside
import Modeling.Util (localMod)

data RealTypeError = Boom deriving (Generic, Show, Eq)

data TypeError = TypeError
    { ns :: Namespace
    , error :: RealTypeError
    } deriving (Generic, Show, Eq)

data TypeEnv = TypeEnv
    { ns :: Namespace
    } deriving (Generic, Show, Eq)

type TypeC m = (MonadReader TypeEnv m, MonadError TypeError m)
newtype TypeT m a = EvalT { unEvalT :: FuncT TypeEnv () TypeError m a }
    deriving (Functor, Applicative, Monad, MonadReader TypeEnv, MonadError TypeError)

typeProof :: Monad m => (forall n. TypeC n => n a) -> TypeT m a
typeProof = id

data ModelType = ModelType deriving (Generic, Show, Eq)

makeTypeError :: TypeC m => RealTypeError -> m TypeError
makeTypeError e = TypeError <$> view (field @"ns") <*> pure e

throwTypeError :: TypeC m => RealTypeError -> m a
throwTypeError e = makeTypeError e >>= throwError

inferModelType :: TypeC m => Model (ModelSpaceFix) -> m ModelType
inferModelType = undefined

withNsPart :: TypeC m => NamespacePart -> m a -> m a
withNsPart p = localMod (field @"ns") (flip (|>) p)

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
