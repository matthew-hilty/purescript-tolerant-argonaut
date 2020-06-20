module Data.Argonaut.Decode.Struct.Tolerant.Cross.Utils
  ( decodeJsonPer
  , decodeJsonWith
  ) where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Decode.Struct.Cross.DecodeJsonWith (class DecodeJsonWith, decodeJsonWith) as C
import Data.Argonaut.Decode.Struct.Override.DecodeJsonPer (class DecodeJsonPer, decodeJsonPer) as O
import Data.Argonaut.Decode.Struct.Tolerant.GDecodeJson (class GDecodeJson, gDecodeJson)
import Data.Argonaut.Decode.Struct.Utils (reportJson)
import Data.Operator.Bottom (class Bottom2)
import Data.Operator.Top (class Top1_, top1_)
import Foreign.Object (Object)
import Prelude (class Bind, bind, ($), (<<<))
import Record.Builder (Builder, build)
import Type.RowList (class RowToList, Nil, RLProxy(RLProxy))

decodeJsonPer
  :: forall f l0 l2 r0 r2 r3
   . Bind f
  => Bottom2 f JsonDecodeError
  => O.DecodeJsonPer Builder f Record l0 r0 l2 r2 r3
  => GDecodeJson Builder f Record Nil () l2 r2
  => RowToList r0 l0
  => RowToList r2 l2
  => Top1_ f
  => Record r0
  -> Json
  -> f (Record r3)
decodeJsonPer decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record r3)
  go object = do
    (addFields2 :: Builder (Record ()) (Record r2)) <-
      gDecodeJson
        (RLProxy :: RLProxy Nil)
        (RLProxy :: RLProxy l2)
        object
    (addFields0 :: Builder (Record r2) (Record r3)) <-
      O.decodeJsonPer
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy l2)
        decoderRecord
        object
    top1_ $ build (addFields0 <<< addFields2) {}

decodeJsonWith
  :: forall f l0 l2 r0 r2 r3
   . Bind f
  => Bottom2 f JsonDecodeError
  => C.DecodeJsonWith Builder f Record l0 r0 l2 r2 r3 (Record r2)
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
    (addFields2 :: Builder (Record ()) (Record r2)) <-
      gDecodeJson
        (RLProxy :: RLProxy Nil)
        (RLProxy :: RLProxy l2)
        object
    let record2 = build addFields2 {}
    (addFields0 :: Builder (Record r2) (Record r3)) <-
      C.decodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy l2)
        decoderRecord
        object
        record2
    top1_ $ build addFields0 record2
