module Data.Argonaut.Decode.Struct.Tolerant.DecodeJson
  ( class DecodeJson
  , decodeJson
  ) where

import Prelude (bind, ($))

import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson) as D
import Data.Argonaut.Decode.Struct.Tolerant.GDecodeJson
  ( class GDecodeJson
  , gDecodeJson
  ) as G
import Data.Argonaut.Decode.Struct.Utils (notObjectErrorMessage)
import Data.Either (Either)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Operator.Bottom (bottom2)
import Data.Operator.Top (top1_)
import Record.Builder (Builder, build)
import Type.RowList (class RowToList, Nil, RLProxy(RLProxy), kind RowList)

class DecodeJson a where
  decodeJson :: Json -> Either String a

instance decodeJsonRecord
  :: ( G.GDecodeJson Builder (Either String) Record Nil () l r
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
        bottom2 notObjectErrorMessage

else instance decodeDecodeJson :: D.DecodeJson a => DecodeJson a where
  decodeJson = D.decodeJson
