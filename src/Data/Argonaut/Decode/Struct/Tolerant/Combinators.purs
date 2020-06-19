module Data.Argonaut.Decode.Struct.Tolerant.Combinators
  ( getField          , (.::)
  , getFieldOptional  , (.::!)
  , getFieldOptional' , (.::?)
  ) where

import Prelude

import Data.Argonaut.Core (Json, isNull)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Decode.Struct.Tolerant.DecodeJson
  ( class DecodeJson
  , decodeJson
  )
import Data.Argonaut.Decode.Struct.Utils (elaborateFailure)
import Data.Either (Either(Left))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Foreign.Object (Object, lookup)

-- | Attempt to get the value for a given key on an `Object Json`.
-- |
-- | Use this accessor if the key and value *must* be present in your object.
-- | If the key and value are optional, use `getFieldOptional'` (`.::?`) instead.
getField :: forall a. DecodeJson a => Object Json -> String -> Either JsonDecodeError a
getField o s =
  maybe
    (Left $ "Expected field " <> show s)
    (elaborateFailure s <<< decodeJson)
    (lookup s o)

infix 7 getField as .::

-- | Attempt to get the value for a given key on an `Object Json`.
-- |
-- | The result will be `Right Nothing` if the key and value are not present,
-- | or if the key is present and the value is `null`.
-- |
-- | Use this accessor if the key and value are optional in your object.
-- | If the key and value are mandatory, use `getField` (`.::`) instead.
getFieldOptional'
  :: forall a
   . DecodeJson a
  => Object Json
  -> String
  -> Either JsonDecodeError (Maybe a)
getFieldOptional' o s =
  maybe
    (pure Nothing)
    decode
    (lookup s o)
  where
    decode json =
      if isNull json
        then pure Nothing
        else Just <$> (elaborateFailure s <<< decodeJson) json

infix 7 getFieldOptional' as .::?

-- | Attempt to get the value for a given key on an `Object Json`.
-- |
-- | The result will be `Right Nothing` if the key and value are not present,
-- | but will fail if the key is present but the value cannot be converted
-- | to the right type.
-- |
-- | This function will treat `null` as a value and attempt to decode it into
-- | your desired type. If you would like to treat `null` values the same as
-- | absent values, use `getFieldOptional'` (`.::?`) instead.
getFieldOptional
  :: forall a
   . DecodeJson a
  => Object Json
  -> String
  -> Either JsonDecodeError (Maybe a)
getFieldOptional o s =
  maybe
    (pure Nothing)
    decode
    (lookup s o)
  where
    decode json = Just <$> (elaborateFailure s <<< decodeJson) json

infix 7 getFieldOptional as .::!
