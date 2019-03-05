module Modeling.Data.Serde where

import Control.Arrow (left)
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Yaml (decodeFileEither)
import Modeling.Data.Bidi
import Modeling.Data.Util

-- jsonTextInjection :: IsString e => Injection e a Value -> Injection e a Text
-- jsonTextInjection = undefined

-- jsonTextInjection' :: (FromJSON a, IsString e) => Injection e a Text
-- jsonTextInjection' = undefined

-- decodeJsonFile :: IsString e => Injection e a Value -> FilePath -> IO (Either e a)
-- decodeJsonFile = undefined

encodeJsonText' :: ToJSON a => a -> Text
encodeJsonText' = decodeUtf8 . toStrict . encode

decodeJsonText' :: (FromJSON a, IsString e) => Text -> Either e a
decodeJsonText' = (left fromString) . eitherDecodeStrict' . encodeUtf8

decodeJsonFile' :: (FromJSON a, IsString e) => FilePath -> IO (Either e a)
decodeJsonFile' = (left fromString <$>) . eitherDecodeFileStrict'

-- yamlTextInjection :: IsString e => Injection e a Value -> Injection e a Text
-- yamlTextInjection = undefined

-- yamlTextInjection' :: (FromJSON a, IsString e) => Injection e a Text
-- yamlTextInjection' = undefined

-- decodeYamlFile :: IsString e => Injection e a Value -> FilePath -> IO (Either e a)
-- decodeYamlFile = undefined

decodeYamlFile' :: (FromJSON a, IsString e) => FilePath -> IO (Either e a)
decodeYamlFile' = (left (fromString . show) <$>) . decodeFileEither
