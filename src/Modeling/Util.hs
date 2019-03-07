module Modeling.Util where

import           Control.Lens         (Lens', over)
import           Control.Monad.Reader (MonadReader, local)

localMod :: MonadReader x m => Lens' x y -> (y -> y) -> m z -> m z
localMod lens mod = local (over lens mod)
