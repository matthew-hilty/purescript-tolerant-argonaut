module Data.Argonaut.Decode.Struct.Utils
  ( elaborateFailure
  , getMissingFieldErrorMessage
  , notObjectErrorMessage
  , reportJson
  , reportObject
  ) where

import Prelude

import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Class (class GDecodeJson, gDecodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Operator.Bottom (class Bottom2, bottom2)
import Data.Operator.Top (class Top1_, top1_)
import Foreign.Object (Object)
import Type.RowList (class RowToList, RLProxy(RLProxy))

elaborateFailure :: forall a. String -> Either String a -> Either String a
elaborateFailure s e = lmap msg e
  where
  msg m = "Failed to decode key '" <> s <> "': " <> m

getMissingFieldErrorMessage :: String -> String
getMissingFieldErrorMessage fieldName =
  "JSON was missing expected field: " <> fieldName

notObjectErrorMessage :: String
notObjectErrorMessage = "Could not convert JSON to object"

reportJson
  :: forall a f
   . Bottom2 f String
  => (Object Json -> f a)
  -> Json
  -> f a
reportJson f json =
  case toObject json of
    Just object -> f object
    Nothing -> bottom2 notObjectErrorMessage

reportObject
  :: forall f l r
   . Bottom2 f String
  => GDecodeJson r l
  => RowToList r l
  => Top1_ f
  => Object Json
  -> f (Record r)
reportObject object =
  case gDecodeJson object (RLProxy :: RLProxy l) of
    Left errorStr -> bottom2 errorStr
    Right record -> top1_ record
