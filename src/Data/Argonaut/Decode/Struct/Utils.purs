module Data.Argonaut.Decode.Struct.Utils
  ( reportJson
  , reportObject
  ) where

import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Argonaut.Decode.Class (class GDecodeJson, gDecodeJson)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Operator.Bottom (class Bottom2, bottom2)
import Data.Operator.Top (class Top1_, top1_)
import Foreign.Object (Object)
import Type.RowList (class RowToList, RLProxy(RLProxy))

reportJson
  :: forall a f
   . Bottom2 f JsonDecodeError
  => (Object Json -> f a)
  -> Json
  -> f a
reportJson f json =
  case toObject json of
    Just object -> f object
    Nothing -> bottom2 (TypeMismatch "Object")

reportObject
  :: forall f l r
   . Bottom2 f JsonDecodeError
  => GDecodeJson r l
  => RowToList r l
  => Top1_ f
  => Object Json
  -> f (Record r)
reportObject object =
  case gDecodeJson object (RLProxy :: RLProxy l) of
    Left decodeError -> bottom2 decodeError
    Right record -> top1_ record
