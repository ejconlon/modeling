module Modeling.Ops.Ext where

import Data.Sequence        (Seq)
import Data.Text            (Text)
import GHC.Generics         (Generic)
import Modeling.Data.Common
import Modeling.Ops.Core

type AttributeName = Text
type AttributeValue = Text

data SplitOpts = SplitOpts
    { attribute :: AttributeName
    , values    :: Seq AttributeValue
    , other     :: Maybe AttributeValue
    } deriving (Generic, Show, Eq)

data ExtOps d (m :: * -> *) = ExtOps
    { serialOp :: NamespacePart -> Seq (IntModel d) -> m (IntModel d)
    , splitOp  :: NamespacePart -> SplitOpts -> IntModel d -> m (IntModel d)
    }
