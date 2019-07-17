module Data.Argonaut.Decode.Struct.Override.Utils
  ( decodeJsonWith
  ) where

import Prelude (class Bind, bind, ($))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class GDecodeJson)
import Data.Argonaut.Decode.Struct.Override.DecodeJsonWith
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
  :: forall f l0 l1 r0 r1 r2
   . Bind f
  => Bottom2 f String
  => D.DecodeJsonWith Builder f Record l0 r0 l1 r1 r2
  => GDecodeJson r1 l1
  => RowToList r0 l0
  => RowToList r1 l1
  => Top1_ f
  => Record r0
  -> Json
  -> f (Record r2)
decodeJsonWith decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record r2)
  go object = do
    (record1 :: Record r1) <- reportObject object
    (addFields0 :: Builder (Record r1) (Record r2)) <- D.decodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy l1)
        decoderRecord
        object
    top1_ $ build addFields0 record1
