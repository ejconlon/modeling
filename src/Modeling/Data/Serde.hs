module Modeling.Data.Serde where

import Data.Aeson
import Data.String (IsString, fromString)
import Data.Text
import Data.Yaml
import Modeling.Data.Bidi
import Modeling.Data.Util

-- jsonTextInjection :: IsString e => Injection e a Value -> Injection e a Text
-- jsonTextInjection = undefined

-- jsonTextInjection' :: (FromJSON a, IsString e) => Injection e a Text
-- jsonTextInjection' = undefined

-- decodeJsonFile :: IsString e => Injection e a Value -> FilePath -> IO (Either e a)
-- decodeJsonFile = undefined

decodeJsonFile' :: FromJSON a => FilePath -> IO (Either String a)
decodeJsonFile' = eitherDecodeFileStrict'

-- yamlTextInjection :: IsString e => Injection e a Value -> Injection e a Text
-- yamlTextInjection = undefined

-- yamlTextInjection' :: (FromJSON a, IsString e) => Injection e a Text
-- yamlTextInjection' = undefined

-- decodeYamlFile :: IsString e => Injection e a Value -> FilePath -> IO (Either e a)
-- decodeYamlFile = undefined

decodeYamlFile' :: FromJSON a => FilePath -> IO (Either ParseException a)
decodeYamlFile' = decodeFileEither
