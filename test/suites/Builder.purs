module Test.Suites.Builder
  ( suites
  ) where

import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Decode.Struct (decodeJson')
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(Just))
import Prelude (discard, pure, unit, (==), ($))
import Record.Builder (build)
import Test.Unit (TestSuite, suite, test)
import Test.Utils (assert, checkError, withErrorMsg)

suites :: TestSuite
suites =
  suite "Builder" do

    test "#0" do
      let
        value0 = { a0: 0
                 , a1: 1
                 , a2: Just 2
                 , a3: Just "hello"
                 , a4: Just true
                 }
        value1 = {}
        result = decodeJson' $ encodeJson value0
      assert $ checkError (lmap printJsonDecodeError result) withErrorMsg (\f -> build f value1 == value0)

    test "#1" do
      let
        value0 = { a0: 0
                 , a1: 1
                 , a2: Just 2
                 , a3: Just "hello"
                 , a4: Just true
                 }
        value1 = { b0: "b0" }
        result = decodeJson' $ encodeJson value0
      assert
        $ checkError
            (lmap printJsonDecodeError result)
            withErrorMsg
            (\f -> build f value1 == { a0: value0.a0
                                     , a1: value0.a1
                                     , a2: value0.a2
                                     , a3: value0.a3
                                     , a4: value0.a4
                                     , b0: value1.b0
                                     })

    test "#2" do
      let
        value0 = { a0: 0
                 , a1: 1
                 , a2: Just 2
                 , a3: Just "hello"
                 , a4: Just true
                 }
        value1 = { b0: "b0", b1: 1000, b2: 1002 }
        result = decodeJson' $ encodeJson value0
      assert
        $ checkError
            (lmap printJsonDecodeError result)
            withErrorMsg
            (\f -> build f value1 == { a0: value0.a0
                                     , a1: value0.a1
                                     , a2: value0.a2
                                     , a3: value0.a3
                                     , a4: value0.a4
                                     , b0: value1.b0
                                     , b1: value1.b1
                                     , b2: value1.b2
                                     })

    test "#3 -- Should Not Compile" $ pure unit
--       let
--         value0 = { a0: 0
--                  , a1: 1
--                  , a2: Just 2
--                  , a3: Just "hello"
--                  , a4: Just true
--                  }
--         value1 = { a0: 1000 }
--         result = decodeJson' $ encodeJson value0
--       assert
--         $ checkError
--             result
--             withErrorMsg
--             (\f -> build f value1 == { a0: value0.a0
--                                      , a1: value0.a1
--                                      , a2: value0.a2
--                                      , a3: value0.a3
--                                      , a4: value0.a4
--                                      })

    test "#4 -- Should Not Compile" $ pure unit
--       let
--         value0 = { a0: 0
--                  , a1: 1
--                  , a2: Just 2
--                  , a3: Just "hello"
--                  , a4: Just true
--                  }
--         value1 = { a0: "b0" }
--         result = decodeJson' $ encodeJson value0
--       assert
--         $ checkError
--             result
--             withErrorMsg
--             (\f -> build f value1 == { a0: value0.a0
--                                      , a1: value0.a1
--                                      , a2: value0.a2
--                                      , a3: value0.a3
--                                      , a4: value0.a4
--                                      })
