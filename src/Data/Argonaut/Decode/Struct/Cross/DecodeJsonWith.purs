module Data.Argonaut.Decode.Struct.Cross.DecodeJsonWith
  ( class DecodeJsonWith
  , decodeJsonWith
  ) where

import Prelude
  ( class Bind
  , class Category
  , class Semigroupoid
  , bind
  , identity
  , ($)
  , (<<<)
  )

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Struct.Utils (getMissingFieldErrorMessage)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Operator.Bottom (class Bottom2, bottom2)
import Data.Operator.Top (class Top1_, top1_)
import Data.Struct (class RGet, class RInsert, rget, rinsert)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Foreign.Object (Object, lookup)
import Type.Data.RowList (RLProxy(RLProxy))
import Type.Equality (class TypeEquals, to)
import Type.Proxying (class RLProxying)
import Type.Row (class Cons, class Lacks, Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)

class DecodeJsonWith
  (p  :: Type -> Type -> Type)
  (f  :: Type -> Type)
  (g  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l2 :: RowList)
  (r2 :: # Type)
  (r3 :: # Type)
  a
  | l0 -> r0 a
  , l2 -> r2
  , l0 l2 -> r3
  where
  decodeJsonWith
    :: forall h
     . RLProxying h l0
    => RLProxying h l2
    => h l0
    -> h l2
    -> g r0
    -> Object Json
    -> a
    -> f (p (g r2) (g r3))

instance decodeJsonWithNil
  :: ( Category p
     , Top1_ f
     )
  => DecodeJsonWith p f g Nil () l r r a
  where
  decodeJsonWith _ _ _ _ _ = top1_ identity

instance decodeJsonWithCons
  :: ( Bind f
     , Bottom2 f String
     , Cons s fn r0' r0
     , Cons s v r3' r3
     , DecodeJsonWith p f g l0' r0' l2 r2 r3' a
     , IsSymbol s
     , Lacks s r3'
     , RGet g SProxy s l0 r0
     , RInsert p g SProxy s l3' r3' l3 r3
     , Semigroupoid p
     , Top1_ f
     , TypeEquals fn (Json -> a -> f v)
     )
  => DecodeJsonWith p f g (Cons s fn l0') r0 l2 r2 r3 a
  where
  decodeJsonWith _ _ decoderStruct object x = do
    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal x
        doRest <- decodeJsonWith l0' l2 decoderStruct' object x
        top1_ $ rinsert l3' l3 s val <<< doRest
      Nothing ->
        bottom2 $ getMissingFieldErrorMessage fieldName
    where
    decoder :: Json -> a -> f v
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
    l2  = RLProxy :: RLProxy l2
    l3' = RLProxy :: RLProxy l3'
    l3  = RLProxy :: RLProxy l3
    s   = SProxy  :: SProxy s
