module Modeling.Typecheck where

import Control.Lens          (view)
import Control.Monad         (unless)
import Control.Monad.Except  (MonadError, throwError)
import Control.Monad.Reader  (MonadReader)
import Control.Monad.State   (MonadState)
import Data.Foldable         (traverse_)
import Data.Generics.Product (field)
import Data.Map              (Map)
import qualified Data.Map as Map
import Data.Sequence         (Seq, (|>))
import Data.Set              (Set)
import qualified Data.Set as Set
import GHC.Generics          (Generic)
import Modeling.Data.Common
import Modeling.Data.Core
import Modeling.Data.Model
import Modeling.Data.Outside
import Modeling.Data.Param
import Modeling.Data.Type
import Modeling.Func
import Modeling.Util

data ParamKind = Input | Output deriving (Generic, Show, Eq)

data Dir =
      NamespaceDir NamespacePart
    | NamedDepDir ElementName
    | AdditionalDepDir
    | ParamDir ParamKind ParamName
    deriving (Generic, Show, Eq)

data RealTypeError =
      MissingParamError ParamName
    | MissingTypeError TypeName
    | MissingReferencesError (Set TypeName)
    | MissingElementError ElementName
    deriving (Generic, Show, Eq)

-- TODO eventually we want this to be serializable
data TypeError = TypeError
    { position :: Seq Dir
    , error :: RealTypeError
    } deriving (Generic, Show, Eq)

data TypeEnv = TypeEnv
    { position :: Seq Dir
    , tydefs :: Map TypeName TypeFix
    , params :: Map ParamName TypeFix
    , elements :: Map ElementName ModelType
    } deriving (Generic, Show, Eq)

type TypeC m = (MonadReader TypeEnv m, MonadError TypeError m)
newtype TypeT m a = EvalT { unEvalT :: FuncT TypeEnv () TypeError m a }
    deriving (Functor, Applicative, Monad, MonadReader TypeEnv, MonadError TypeError)

typeProof :: Monad m => (forall n. TypeC n => n a) -> TypeT m a
typeProof = id

freeVars :: TypeFix -> Set TypeName
freeVars (TypeFix t) =
    case t of
        StringType -> Set.empty
        LongType -> Set.empty
        DoubleType -> Set.empty
        BooleanType -> Set.empty
        OptionalType (TypeSingleAttrs ty) -> freeVars ty
        ListType (TypeSingleAttrs ty) -> freeVars ty
        StringMapType (TypeSingleAttrs ty) -> freeVars ty
        StructType (TypeStructAttrs fields) -> foldMap freeVars fields
        ReferenceType (TypeReferenceAttrs name) -> Set.singleton name
        EnumType {} -> Set.empty
        UnionType (TypeUnionAttrs branches) -> foldMap freeVars branches
        AnyType -> Set.empty

data DepTypes = DepTypes
    { named :: Maybe (Map ElementName ModelType)
    , additional :: Maybe ModelType
    } deriving (Generic, Show, Eq)

data ModelType = ModelType
    { signature :: Maybe Signature
    , depTypes :: Maybe DepTypes
    } deriving (Generic, Show, Eq)

makeTypeError :: TypeC m => RealTypeError -> m TypeError
makeTypeError e = TypeError <$> view (field @"position") <*> pure e

throwTypeError :: TypeC m => RealTypeError -> m a
throwTypeError e = makeTypeError e >>= throwError

getElement :: TypeC m => ElementName -> m ModelType
getElement = readerGet (field @"elements") (throwTypeError . MissingElementError)

splitType :: TypeC m => Seq ModelType -> m ModelType
splitType = undefined

inferModelType :: TypeC m => Model (ModelSpaceFix) -> m ModelType
inferModelType m =
    case m of
        DirectModel (ModelDirectAttrs { name }) -> getElement name
        SerialModel (ModelSerialAttrs { models }) -> traverse (inferModelType . element . unModelSpace . unModelSpaceFix) models >>= splitType
        SplitModel (ModelSplitAttrs {}) -> undefined

withDir :: TypeC m => Dir -> m a -> m a
withDir p = localMod (field @"position") (flip (|>) p)

checkIsType :: TypeC m => TypeFix -> Param -> m ()
checkIsType = undefined

checkParam :: TypeC m => ParamKind -> TypeFix -> ParamName -> Param -> m ()
checkParam pk tf pm p = withDir (ParamDir pk pm) (checkIsType tf p)

checkInput :: TypeC m => ParamName -> Param -> m ()
checkInput paramName param = do
    paramType <- getParam paramName
    checkParam Input paramType paramName param

checkDependencies :: TypeC m => Model ModelSpaceFix -> m ()
checkDependencies = undefined

checkModelWith :: TypeC m => ModelType -> ModelSpaceFix -> m ()
checkModelWith (ModelType {..}) (ModelSpaceFix (ModelSpace Space { .. })) = do
    withDir (NamespaceDir nspart) $
        maybeLocal withSignatureTydefs signature $ do
            traverse_ (traverseMap_ checkInput) inputs
            -- checkDependencies element

lookupType :: TypeC m => TypeName -> m (Maybe TypeFix)
lookupType = readerLookup (field @"tydefs")

getType :: TypeC m => TypeName -> m TypeFix
getType =  readerGet (field @"tydefs") (throwTypeError . MissingTypeError)

lookupParam :: TypeC m => ParamName -> m (Maybe TypeFix)
lookupParam = readerLookup (field @"params")

getParam :: TypeC m => ParamName -> m TypeFix
getParam = readerGet (field @"params") (throwTypeError . MissingParamError)

checkSignatureExternal :: TypeC m => Signature -> m ()
checkSignatureExternal (Signature { external }) = traverse_ (traverse_ getType) external

checkWellFormed :: TypeC m => TypeFix -> m ()
checkWellFormed tf = let fv = freeVars tf in unless (Set.null fv) (throwTypeError (MissingReferencesError fv))

checkType :: TypeC m => ParamKind -> ParamName -> TypeFix -> m ()
checkType pk pn tf = withDir (ParamDir pk pn) (checkWellFormed tf)

checkSignatureInternal :: TypeC m => Signature -> m ()
checkSignatureInternal (Signature { inputs, outputs }) = do
    traverse_ (traverseMap_ (checkType Input)) inputs
    traverse_ (traverseMap_ (checkType Output)) outputs

checkDepTypes :: TypeC m => DepTypes -> m ()
checkDepTypes (DepTypes {..}) = do
    traverse_ (traverseMap_ (\k v -> withDir (NamedDepDir k) (checkModelType v))) named
    traverse_ (withDir AdditionalDepDir . checkModelType) additional

checkModelType :: TypeC m => ModelType -> m ()
checkModelType (ModelType {..}) = maybeLocal checkSignature signature (traverse_ checkDepTypes depTypes)

checkModel :: TypeC m => ModelSpaceFix -> m ()
checkModel msf@(ModelSpaceFix (ModelSpace Space { element })) = do
    modelType <- inferModelType element
    checkModelType modelType
    checkModelWith modelType msf

withSignatureTydefs :: TypeC m => Signature -> m a -> m a
withSignatureTydefs (Signature { tydefs }) = localMod (field @"tydefs") (maybe id mergeMaps tydefs)

checkSignature :: TypeC m => Signature -> m a -> m a
checkSignature sig act = do
    checkSignatureExternal sig
    withSignatureTydefs sig $ do
        checkSignatureInternal sig
        act

checkBundle :: TypeC m => ModelSpaceBundle -> m ()
checkBundle (ModelSpaceBundle (Bundle {..})) = maybeLocal checkSignature signature (checkModel root)
