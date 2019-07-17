module Data.Argonaut.Decode.Struct.Tolerant.Cross.Utils
  ( decodeJsonWith
  ) where

import Prelude (class Bind, bind, ($))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Struct.Cross.DecodeJsonWith
  ( class DecodeJsonWith
  , decodeJsonWith
  ) as D
import Data.Argonaut.Decode.Struct.Utils (reportJson)
import Data.Argonaut.Decode.Struct.Tolerant.GDecodeJson
  ( class GDecodeJson
  , gDecodeJson
  )
import Data.Operator.Bottom (class Bottom2)
import Data.Operator.Top (class Top1_, top1_)
import Foreign.Object (Object)
import Record.Builder (Builder, build)
import Type.Data.RowList (RLProxy(RLProxy))
import Type.Row (class RowToList, Nil)

decodeJsonWith
  :: forall f l0 l2 r0 r2 r3
   . Bind f
  => Bottom2 f String
  => D.DecodeJsonWith Builder f Record l0 r0 l2 r2 r3 (Record r2)
  => GDecodeJson Builder f Record Nil () l2 r2
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
    addFields2 <-
      gDecodeJson
        (RLProxy :: RLProxy Nil)
        (RLProxy :: RLProxy l2)
        object
    let record2 = build addFields2 {}
    addFields0 <-
      D.decodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy l2)
        decoderRecord
        object
        record2
    top1_ $ build addFields0 record2
