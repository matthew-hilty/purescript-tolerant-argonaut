module Test.Utils
  ( assert
  , assertEquivalence
  , check
  , checkError
  , doesntFail
  , fails
  , withErrorMsg
  ) where

import Data.Either (Either)
import Data.Foldable (class Foldable, foldr)
import Data.Maybe (Maybe(Just))
import Data.Operator.Bottom (class Bottom2, bottom2)
import Data.Operator.PartialOrd (class PartialOrd1, (.>=?))
import Data.Tuple (Tuple(Tuple), uncurry)
import Prelude (class Eq, class Show, show, (==), (<>), ($))
import Test.Unit (Test)
import Test.Unit.Assert as Assert

assert :: Tuple String Boolean -> Test
assert = uncurry Assert.assert

assertEquivalence
  :: forall f a
   . Foldable f
  => Eq a
  => Show a
  => f a
  -> a
  -> Test
assertEquivalence result value =
  assert $ checkEquivalence result value

check
  :: forall f a
   . Foldable f
  => f a
  -> String
  -> (a -> Boolean)
  -> Tuple String Boolean
check result msg predicate =
  foldr
    (\val _ ->
      let
        state = predicate val
        msg' = if state then successful else msg
      in Tuple msg' state)
    (Tuple failsUnexpectedly false)
    result

checkEquivalence
  :: forall f a
   . Foldable f
  => Eq a
  => Show a
  => f a
  -> a
  -> Tuple String Boolean
checkEquivalence result value =
  foldr
    (\val _ ->
      let
        state = (val == value)
        msg' = if state then successful else ("Should be " <> show value)
      in Tuple msg' state)
    (Tuple failsUnexpectedly false)
    result

checkError
  :: forall a
   . Either String a
  -> String
  -> (a -> Boolean)
  -> Tuple String Boolean
checkError = check

doesntFail :: String
doesntFail = "is decoded despite expectation of failure"

fails
  :: forall f a
   . Bottom2 f String
  => PartialOrd1 f
  => f a
  -> Tuple String Boolean
fails result =
  if isError result
    then Tuple successful true
    else Tuple doesntFail false

failsUnexpectedly :: String
failsUnexpectedly = "fails unexpectedly"

isError
  :: forall a f
   . Bottom2 f String
  => PartialOrd1 f
  => f a
  -> Boolean
isError x =
  (x .>=? bottom2 "") == Just true

successful :: String
successful = "successful test"

withErrorMsg :: String
withErrorMsg = "doesn't meet expectations"
