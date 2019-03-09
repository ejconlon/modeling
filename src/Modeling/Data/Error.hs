module Modeling.Data.Error where

import Data.String (IsString)
import Data.Text
import qualified Data.Text as T

newtype ErrorMsg = ErrorMsg { unErrorMsg :: Text } deriving (Show, Eq, Ord, IsString)

renderErrorMsg :: ErrorMsg -> String
renderErrorMsg = T.unpack . unErrorMsg

class Injection a where
    type InjTarget a
    injApply :: a -> InjTarget a
    injInvert :: InjTarget a -> Either ErrorMsg a
