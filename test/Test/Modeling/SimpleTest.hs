{-# LANGUAGE GADTs #-}

module Test.Modeling.SimpleTest where

import Data.Aeson
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Modeling.Data.Aeson
import Modeling.Data.Core
import Modeling.Data.Model
import Modeling.Data.Outside
import Modeling.Data.Serde
import Modeling.Data.Param
import Modeling.Data.Type
import Modeling.Data.Util
import Test.Tasty
import Test.Tasty.HUnit

test_something :: TestTree
test_something = testCase "something" $ do
    let actual = 1 + 1
        expected = 2
    actual @?= expected

-- test_example :: TestTree
-- test_example = testCase "example" $ do
--     let expected = ModelSpaceBundle (Bundle
--             { signature = Just (Signature
--                 { inputs = Just (Map.singleton "whatever" (TypeFix StringType))
--                 , outputs = Nothing
--                 , tydefs = Nothing
--                 })
--             , root = ModelSpaceFix (ModelSpace (Space
--                 { connection = Connection
--                     { nspart = "whatever"
--                     , inputs = Nothing
--                     , named = Nothing
--                     , additional = Nothing
--                     }
--                 , element = DirectModel (ModelDirectAttrs "something")
--                 } ))
--             })
--     actual <- decodeJsonFile' "testdata/example.json"
--     -- actual <- decodeYamlFile' "testdata/example.yaml"
--     actual @?= Right expected

data SerdeCase a = SerdeCase { name :: Text, value :: a, source :: Text } deriving (Functor)

data SomeSerdeCase where
    SomeSerdeCase :: (Eq a, Show a, ToJSON a, FromJSON a) => SerdeCase a -> SomeSerdeCase
    SomeSerdeCase1 :: (Eq (f a), Show (f a), ToJSON1 f, FromJSON1 f, ToJSON a, FromJSON a) => SerdeCase (f a) -> SomeSerdeCase

runSerdeCase :: (Eq a, Show a, ToJSON a, FromJSON a) => SerdeCase a -> TestTree
runSerdeCase (SerdeCase name value source) = testCase (T.unpack name) $ do
    let actualSource = encodeJsonText' value
    actualSource @?= source
    let actualValue = decodeJsonText' source
    actualValue @?= Right value

runSerdeCase1 :: (Eq (f a), Show (f a), ToJSON1 f, FromJSON1 f, ToJSON a, FromJSON a) => SerdeCase (f a) -> TestTree
runSerdeCase1 = runSerdeCase . (AesonWrapperApp <$>)

runSomeSerdeCase :: SomeSerdeCase -> TestTree
runSomeSerdeCase (SomeSerdeCase serdeCase) = runSerdeCase serdeCase
runSomeSerdeCase (SomeSerdeCase1 serdeCase) = runSerdeCase1 serdeCase

serdeCases :: [SomeSerdeCase]
serdeCases =
    [ SomeSerdeCase1 (SerdeCase "rawSumNothing" (RawSum "hi" Nothing :: RawSum Int) "{\"name\":\"hi\"}")
    , SomeSerdeCase1 (SerdeCase "rawSumJust" (RawSum "hi" (Just 1) :: RawSum Int) "{\"name\":\"hi\",\"attributes\":1}")
    , SomeSerdeCase1 (SerdeCase "sumNothing" (Sum "hi" Nothing :: Sum Int) "{\"name\":\"hi\"}")
    , SomeSerdeCase1 (SerdeCase "sumJust" (Sum "hi" (Just 1) :: Sum Int) "{\"name\":\"hi\",\"attributes\":{\"hi\":1}}")
    , SomeSerdeCase  (SerdeCase "innerParamAttrs" innerAttrs "{\"index\":1}")
    , SomeSerdeCase  (SerdeCase "outerParamAttrs" outerAttrs "{\"internal\":{\"index\":1}}")
    , SomeSerdeCase1 (SerdeCase "paramSum" paramSum paramSumSource)
    , SomeSerdeCase  (SerdeCase "param" param paramSumSource)
    , SomeSerdeCase  (SerdeCase "stringType" stringType stringTypeSource)
    , SomeSerdeCase1 (SerdeCase "stringTypeSum" stringTypeSum stringTypeSource)
    , SomeSerdeCase  (SerdeCase "optionalStringType" optionalStringType optionalStringTypeSource)
    ] where
        innerAttrs = InternalParamAttrs 1
        outerAttrs = emptyParamAttrs { internal = Just innerAttrs }
        paramSum = Sum "internal" (Just innerAttrs)
        param = InternalParam innerAttrs
        paramSumSource = "{\"name\":\"internal\",\"attributes\":{\"internal\":{\"index\":1}}}"
        stringType = TypeFix StringType
        stringTypeSum = Sum "string" Nothing :: Sum TypeSumFix
        stringTypeSource = "{\"name\":\"string\"}"
        optionalStringType = TypeFix (OptionalType (TypeSingleAttrs stringType))
        optionalStringTypeSource = "{\"name\":\"optional\",\"attributes\":{\"optional\":{\"ty\":{\"name\":\"string\"}}}}"

test_serde :: TestTree
test_serde = testGroup "serde" (runSomeSerdeCase <$> serdeCases)
