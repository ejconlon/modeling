module Modeling.Data.Util where

import Data.Aeson
import Data.Aeson.Encoding.Internal (pair)
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.HashMap.Strict as HM
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Generics (Generic, Generic1)
import Modeling.Data.Aeson
import Modeling.Data.Error

data Sum a = Sum Text (Maybe a) deriving (Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Sum a) where
    toJSON (Sum n ma) =
        object (("name" .= n):maybe [] (\a -> ["attributes" .= object [n .= a]]) ma)
    toEncoding (Sum n ma) =
        let s = "name" .= n
            t = case ma of
                    Nothing -> mempty
                    Just a ->
                        let u = pair n (toEncoding a)
                        in pair "attributes" (pairs u)
        in pairs (s <> t)

instance FromJSON a => FromJSON (Sum a) where
    parseJSON = withObject "Sum" $ \v -> do
        n <- v .: "name"
        mo <- v .:? "attributes"
        case mo of
            Nothing -> pure (Sum n Nothing)
            Just o ->
                if HM.size o /= 1
                    then fail ("Too many branches in sum for " <> T.unpack n)
                    else do
                        a <- o .: n
                        b <- parseJSON a
                        pure (Sum n (Just b))

missingAttrs, unexpectedAttrs :: ErrorMsg
missingAttrs = ErrorMsg "Missing attributes field"
unexpectedAttrs = ErrorMsg "Unexpected value in attributes field"

simpleWithoutAttrs :: Maybe a -> Either ErrorMsg (Maybe a)
simpleWithoutAttrs ma = case ma of { Nothing -> Right Nothing; Just _ -> Left unexpectedAttrs }

simpleWithAttrs :: (a -> Maybe b) -> Maybe a -> Either ErrorMsg (Maybe a)
simpleWithAttrs s ma = case ma >>= s of { Nothing -> Left missingAttrs; Just _ -> Right ma }

withoutAttrs :: b -> Maybe a -> Either ErrorMsg b
withoutAttrs y ma = case ma of { Nothing -> pure y; Just _ -> Left unexpectedAttrs }

withAttrs :: (a -> Maybe v) -> (v -> b) -> Maybe a -> Either ErrorMsg b
withAttrs s f ma = case ma >>= s of { Nothing -> Left missingAttrs; Just x -> Right (f x) }
