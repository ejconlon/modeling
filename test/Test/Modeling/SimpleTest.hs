module Test.Modeling.SimpleTest where

import qualified Data.Map as Map
import Modeling.Data.Core
import Modeling.Data.Fix
import Modeling.Data.Model
import Modeling.Data.Outside
import Modeling.Data.Serde
import Modeling.Data.Type
import Test.Tasty
import Test.Tasty.HUnit

-- test_something :: TestTree
-- test_something = testCase "something" $ do
--     let actual = 1 + 1
--         expected = 2
--     actual @?= expected

test_example :: TestTree
test_example = testCase "example" $ do
    let expected = ModelSpaceBundle (Bundle
            { signature = Just (Signature
                { inputs = Just (Map.singleton "whatever" (fixType StringType))
                , outputs = Nothing
                , tydefs = Nothing
                })
            , root = ModelSpaceFix (Fix (ModelSpace (Space
                { connection = Connection
                    { nspart = "whatever"
                    , inputs = Nothing
                    , named = Nothing
                    , additional = Nothing
                    }
                , element = DirectModel (ModelDirectAttrs "something")
                } )))
            })
    actual <- decodeJsonFile' "testdata/example.json"
    -- actual <- decodeYamlFile' "testdata/example.yaml"
    actual @?= Right expected
