module Modeling.Example where

import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift)
import Data.Fix (Fix (..))
import qualified Data.Map as Map
import Data.Sequence (fromList)
import qualified Data.Sequence as Seq
import Modeling.Data.Core
import Modeling.Ops.Core
import Modeling.Ops.Ext

data FullOps d (m :: * -> *) t r = FullOps
    { fullParamOps :: ParamOps d m
    , fullBaseOps :: BaseOps d m t r
    , fullExtOps :: ExtOps d m
    }

data PartialOps d (m :: * -> *) t r = PartialOps
    { partialParamOps :: ParamOps d m
    , partialBaseOps :: BaseOps d m t r
    }

fullToPartialOps :: Fix (FullOps d m t) -> Fix (PartialOps d m t)
fullToPartialOps (Fix (FullOps {fullParamOps, fullBaseOps})) =
    Fix (PartialOps fullParamOps (convertBaseOps fullToPartialOps fullBaseOps))

simpleBuilder :: (r ~ Fix (PartialOps d m t), Monad m) => Builder r m t
simpleBuilder = do
    Fix (PartialOps {..}) <- ask
    lift $ do
        simpleParam <- (externalOp partialParamOps) "simpleExternalParam" StringType
        let simpleOpts = BaseOpts "simpleNs" (Map.singleton "simpleParamInternal" simpleParam) Map.empty Seq.empty
        simpleModel <- (directOp partialBaseOps) simpleOpts "simpleModel"
        (buildOp partialBaseOps) simpleModel

complexBuilder :: (r ~ Fix (FullOps d m t), Monad m) => Builder r m t
complexBuilder = do
    Fix (FullOps {..}) <- ask
    lift $ do
        let firstOpts = BaseOpts "firstNs" Map.empty Map.empty Seq.empty
        firstModel <- (directOp fullBaseOps) firstOpts "firstModel"
        let secondOpts = BaseOpts "secondNs" Map.empty Map.empty Seq.empty
        secondModel <- (embedOp fullBaseOps) secondOpts (convertBuilder fullToPartialOps simpleBuilder)
        serialModel <- (serialOp fullExtOps) (fromList [firstModel, secondModel])
        let splitOpts = SplitOpts { attribute = "region", values = fromList ["SFO", "LAX"], other = Just "OTHER" }
        splitModel <- (splitOp fullExtOps) splitOpts serialModel
        (buildOp fullBaseOps) splitModel
