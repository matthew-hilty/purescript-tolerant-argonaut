module Data.Argonaut.Decode.Struct.Tolerant.DecodeJson
  ( class DecodeJson
  , decodeJson
  ) where

import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson) as D
import Data.Argonaut.Decode.Struct.Tolerant.GDecodeJson (class GDecodeJson, gDecodeJson) as G
import Data.Either (Either)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Operator.Bottom (bottom2)
import Data.Operator.Top (top1_)
import Prelude (bind, ($))
import Record.Builder (Builder, build)
import Type.RowList (class RowToList, Nil, RLProxy(RLProxy))

class DecodeJson a where
  decodeJson :: Json -> Either JsonDecodeError a

instance decodeJsonRecord
  :: ( G.GDecodeJson Builder (Either JsonDecodeError) Record Nil () l r
     , RowToList r l
     )
  => DecodeJson (Record r)
  where
  decodeJson json =
    case toObject json of
      Just object -> do
        builder <-
          G.gDecodeJson
            (RLProxy :: RLProxy Nil)
            (RLProxy :: RLProxy l)
            object
        top1_ $ build builder {}
      Nothing ->
        bottom2 (TypeMismatch "Object")

else instance decodeDecodeJson :: D.DecodeJson a => DecodeJson a where
  decodeJson = D.decodeJson
