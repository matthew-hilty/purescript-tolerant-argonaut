module Test.Suites.Override
  ( suites
  ) where

import Prelude

import Data.Argonaut.Decode (JsonDecodeError(..), printJsonDecodeError)
import Data.Argonaut.Decode.Struct.Override (decodeJsonPer)
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just))
import Test.Unit (TestSuite, suite, test)
import Test.Utils (assert, check, fails, withErrorMsg)

suites :: TestSuite
suites =
  suite "Override" do
    suite "Maybe" do
      suite "{ a0 :: Int, a1 :: Int, a2 :: Maybe Int }" do
        suite ("Override " <> "a2") do
          suite "{ a0: 0, a1: 1, a2: Just 2 }" do
            test "Override with Just" do
              let
                result :: Either JsonDecodeError { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
                result =
                  decodeJsonPer
                    { a2: \json -> Right $ Just 1002 }
                    (encodeJson { a0: 0, a1: 1, a2: Just 2 })
              assert $ check result withErrorMsg
                (_ == { a0: 0, a1: 1, a2: Just 1002 })
      suite "{ a0 :: Int, a1 :: Int, a2 :: Maybe Int, a3 :: Maybe String, a4 :: Maybe Boolean }" do
        suite ("Override " <> "a2, a3, a4") do
          test "{ a0: 0, a1: 1, a2: Just 2 }" do
            let
              result
                :: Either
                    JsonDecodeError
                    { a0 :: Int
                    , a1 :: Int
                    , a2 :: Maybe Int
                    , a3 :: Maybe String
                    , a4 :: Maybe Boolean
                    }
              result =
                decodeJsonPer
                  { a2: \json -> Right $ Just 1002
                  , a3: \json -> Right $ Just "bye"
                  , a4: \json -> Right $ Just false
                  }
                  (encodeJson { a0: 0, a1: 1, a2: Just 2 })
            assert $ fails (lmap printJsonDecodeError result)
          test "{ a0: 0, a1: 1, a2: Just 2, a3: Just \"hello\", a4: Just true }" do
            let
              result
                :: Either
                    JsonDecodeError
                    { a0 :: Int
                    , a1 :: Int
                    , a2 :: Maybe Int
                    , a3 :: Maybe String
                    , a4 :: Maybe Boolean
                    }
              result =
                decodeJsonPer
                  { a2: \json -> Right $ Just 1002
                  , a3: \json -> Right $ Just "bye"
                  , a4: \json -> Right $ Just false
                  }
                  (encodeJson { a0: 0
                              , a1: 1
                              , a2: Just 2
                              , a3: Just "hello"
                              , a4: Just true
                              })
            assert $ check result withErrorMsg
              (_ == { a0: 0, a1: 1, a2: Just 1002, a3: Just "bye", a4: Just false })
        suite ("Override " <> "a1, a3") do
          suite "{ a0: 0, a1: 1, a2: Just 2, a3: Just \"hello\", a4: Just true }" do
            test "#0" do
              let
                result
                  :: Either
                      JsonDecodeError
                      { a0 :: Int
                      , a1 :: Int
                      , a2 :: Maybe Int
                      , a3 :: Maybe String
                      , a4 :: Maybe Boolean
                      }
                result =
                  decodeJsonPer
                    { a1: \json -> Right $ 1001
                    , a3: \json -> Right $ Just "bye"
                    }
                  (encodeJson { a0: 0
                              , a1: 1
                              , a2: Just 2
                              , a3: Just "hello"
                              , a4: Just true
                              })
              assert $ check result withErrorMsg
                (_ == { a0: 0
                      , a1: 1001
                      , a2: Just 2
                      , a3: Just "bye"
                      , a4: Just true
                      })
            test "#1" do
              let
                result
                  :: Either
                      JsonDecodeError
                      { a0 :: Int
                      , a1 :: Int
                      , a2 :: Maybe Int
                      , a3 :: Maybe String
                      , a4 :: Maybe Boolean
                      }
                result =
                  decodeJsonPer
                    { a1: \json -> Right $ 1002
                    , a3: \json -> Left MissingValue
                    }
                  (encodeJson { a0: 0
                              , a1: 1
                              , a2: Just 2
                              , a3: Just "hello"
                              , a4: Just true
                              })
              assert $ fails (lmap printJsonDecodeError result)
        suite ("Override " <> "a1, a4") do
          suite "{ a0: 0, a1: 1, a2: Just 2, a3: Just \"hello\", a4: Just true }" do
            test "#0" do
              let
                result
                  :: Either
                      JsonDecodeError
                      { a0 :: Int
                      , a1 :: Int
                      , a2 :: Maybe Int
                      , a3 :: Maybe String
                      , a4 :: Maybe Boolean
                      }
                result =
                  decodeJsonPer
                    { a1: \json -> Right $ 1001
                    , a4: \json -> Right $ Just false
                    }
                  (encodeJson { a0: 0
                              , a1: 1
                              , a2: Just 2
                              , a3: Just "hello"
                              , a4: Just true
                              })
              assert $ check result withErrorMsg
                (_ == { a0: 0
                      , a1: 1001
                      , a2: Just 2
                      , a3: Just "hello"
                      , a4: Just false
                      })
        suite "Overriding no labels" do
          suite "{ a0: 0, a1: 1, a2: Just 2, a3: Just \"hello\", a4: Just true }" do
            test "#0" do
              let
                result
                  :: Either
                      JsonDecodeError
                      { a0 :: Int
                      , a1 :: Int
                      , a2 :: Maybe Int
                      , a3 :: Maybe String
                      , a4 :: Maybe Boolean
                      }
                result =
                  decodeJsonPer
                    {}
                    (encodeJson { a0: 0
                                , a1: 1
                                , a2: Just 2
                                , a3: Just "hello"
                                , a4: Just true
                                })
              assert $ check result withErrorMsg
                (_ == { a0: 0
                      , a1: 1
                      , a2: Just 2
                      , a3: Just "hello"
                      , a4: Just true
                      })
