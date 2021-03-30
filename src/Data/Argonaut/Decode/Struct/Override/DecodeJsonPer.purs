module Data.Argonaut.Decode.Struct.Override.DecodeJsonPer
  ( class DecodeJsonPer
  , decodeJsonPer
  ) where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Operator.Bottom (class Bottom2, bottom2)
import Data.Operator.Top (class Top1_, top1_)
import Data.Struct (class RGet, class RInsert, rget, rinsert)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Foreign.Object (Object, lookup)
import Prelude (class Bind, class Category, class Semigroupoid, bind, identity, ($), (<<<))
import Type.Equality (class TypeEquals, to)
import Type.Row (class Cons, class Lacks)
import Type.RowList (Cons, Nil, RLProxy(RLProxy), RowList)
import Unsafe.Coerce (unsafeCoerce)

class DecodeJsonPer
  (p  :: Type -> Type -> Type)
  (f  :: Type -> Type)
  (g  :: Row Type -> Type)
  (l0 :: RowList Type)
  (r0 :: Row Type)
  (l1 :: RowList Type)
  (r1 :: Row Type)
  (r2 :: Row Type)
  | l0 -> r0
  , l1 -> r1
  , l0 l1 -> r2
  where
  decodeJsonPer
    :: forall (h :: RowList Type -> Type)
     . h l0
    -> h l1
    -> g r0
    -> Object Json
    -> f (p (g r1) (g r2))

instance decodeJsonPerNil
  :: ( Category p
     , Top1_ f
     )
  => DecodeJsonPer p f g Nil () l r r
  where
  decodeJsonPer _ _ _ _ = top1_ identity

instance decodeJsonPerCons
  :: ( Bind f
     , Bottom2 f JsonDecodeError
     , Cons s fn r0' r0
     , Cons s v r2' r2
     , DecodeJsonPer p f g l0' r0' l1 r1 r2'
     , IsSymbol s
     , Lacks s r2'
     , RGet g SProxy s l0 r0
     , RInsert p g SProxy s l2' r2' l2 r2
     , Semigroupoid p
     , Top1_ f
     , TypeEquals fn (Json -> f v)
     )
  => DecodeJsonPer p f g (Cons s fn l0') r0 l1 r1 r2
  where
  decodeJsonPer _ _ decoderStruct object = do
    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal
        doRest <- decodeJsonPer l0' l1 decoderStruct' object
        top1_ $ rinsert l2' l2 s val <<< doRest
      Nothing ->
        bottom2 $ AtKey fieldName MissingValue
    where
    decoder :: Json -> f v
    decoder = to $ rget l0 s decoderStruct

    -- To prevent unnecessary creation of intermediate decoder structs,
    -- coercion is used rather than calling `delete s`
    -- to induce the next expected type.
    decoderStruct' :: g r0'
    decoderStruct' = unsafeCoerce decoderStruct

    fieldName :: String
    fieldName = reflectSymbol s

    l0' = RLProxy :: RLProxy l0'
    l0  = RLProxy :: RLProxy l0
    l1  = RLProxy :: RLProxy l1
    l2' = RLProxy :: RLProxy l2'
    l2  = RLProxy :: RLProxy l2
    s   = SProxy  :: SProxy s
