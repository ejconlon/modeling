module Modeling.Data.Serde where

import Control.Arrow (left)
import Data.Aeson
import Data.String (IsString, fromString)
import Data.Text
import Data.Yaml (decodeFileEither)
import Modeling.Data.Bidi
import Modeling.Data.Util

-- jsonTextInjection :: IsString e => Injection e a Value -> Injection e a Text
-- jsonTextInjection = undefined

-- jsonTextInjection' :: (FromJSON a, IsString e) => Injection e a Text
-- jsonTextInjection' = undefined

-- decodeJsonFile :: IsString e => Injection e a Value -> FilePath -> IO (Either e a)
-- decodeJsonFile = undefined

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
