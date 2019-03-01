module Modeling.Ext where

import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics (Generic)
import Modeling.Core

type AttributeName = Text
type AttributeValue = Text

data SplitOpts = SplitOpts
    { attribute :: AttributeName
    , values :: Seq AttributeValue
    , other :: Maybe AttributeValue
    } deriving (Generic, Show, Eq)

data ExtOps d (m :: * -> *) = ExtOps
    { serialOp :: Seq (IntModel d) -> m (IntModel d)
    , splitOp :: SplitOpts -> (IntModel d) -> m (IntModel d)
    }
