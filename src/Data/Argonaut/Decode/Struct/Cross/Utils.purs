module Data.Argonaut.Decode.Struct.Cross.Utils
  ( decodeJsonWith
  ) where

import Prelude (class Bind, bind, ($))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class GDecodeJson)
import Data.Argonaut.Decode.Struct.Cross.DecodeJsonWith
  ( class DecodeJsonWith
  , decodeJsonWith
  ) as D
import Data.Argonaut.Decode.Struct.Utils (reportJson, reportObject)
import Data.Operator.Bottom (class Bottom2)
import Data.Operator.Top (class Top1_, top1_)
import Foreign.Object (Object)
import Record.Builder (Builder, build)
import Type.Data.RowList (RLProxy(RLProxy))
import Type.Row (class RowToList)

decodeJsonWith
  :: forall f l0 l2 r0 r2 r3
   . Bind f
  => Bottom2 f String
  => D.DecodeJsonWith Builder f Record l0 r0 l2 r2 r3 (Record r2)
  => GDecodeJson r2 l2
  => RowToList r0 l0
  => RowToList r2 l2
  => Top1_ f
  => Record r0
  -> Json
  -> f (Record r3)
decodeJsonWith decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record r3)
  go object = do
    (record2 :: Record r2) <- reportObject object
    (addFields0 :: Builder (Record r2) (Record r3)) <-
      D.decodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy l2)
        decoderRecord
        object
        record2
    top1_ $ build addFields0 record2
