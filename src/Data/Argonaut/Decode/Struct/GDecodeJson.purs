module Data.Argonaut.Decode.Struct.GDecodeJson
  ( class GDecodeJson
  , decodeJson
  , decodeJson'
  , gDecodeJson
  ) where

import Prelude (class Category, class Semigroupoid, bind, identity, ($), (<<<))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson) as D
import Data.Argonaut.Decode.Struct.Utils
  ( getMissingFieldErrorMessage
  , reportJson
  )
import Data.Either (Either)
import Data.Functor (class Functor, map)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Operator.Bottom (class Bottom2, bottom2)
import Data.Operator.Top (class Top1_, top1_)
import Data.Struct (class RInsert, rinsert)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Foreign.Object (Object, lookup)
import Record.Builder (Builder, build)
import Type.Row (class Cons, class Lacks)
import Type.RowList (class RowToList, Cons, Nil, RLProxy(RLProxy), kind RowList)

class GDecodeJson
  (p  :: Type -> Type -> Type)
  (f  :: Type -> Type)
  (g  :: # Type -> Type)
  (l0 :: RowList)
  (l1 :: RowList)
  (r1 :: # Type)
  (l2 :: RowList)
  (r2 :: # Type)
  | l1 -> r1
  , l2 -> r2
  , l1 l2 -> l0
  where
  gDecodeJson
    :: forall (h :: RowList -> Type)
     . h l1
    -> h l2
    -> Object Json
    -> f (p (g r1) (g r2))

instance gDecodeJson_NilNilNil
  :: ( Category p
     , Top1_ f
     )
  => GDecodeJson p f g Nil Nil () Nil ()
  where
  gDecodeJson _ _ _ = top1_ identity

instance gDecodeJson_ConsNilCons
  :: ( Cons s v r' r
     , D.DecodeJson v
     , GDecodeJson p (Either JsonDecodeError) g l' Nil () l' r'
     , IsSymbol s
     , Lacks s r'
     , RInsert p g SProxy s l' r' l r
     , Semigroupoid p
     )
  => GDecodeJson p (Either JsonDecodeError) g (Cons s v l') Nil () (Cons s v l') r
  where
  gDecodeJson _ _ object = do
    case lookup fieldName object of
      Just jsonVal -> do
        val <- D.decodeJson jsonVal
        doRest <- gDecodeJson nil l' object
        top1_ $ rinsert l' l s val <<< doRest
      Nothing ->
        bottom2 $ getMissingFieldErrorMessage fieldName
    where
    fieldName :: String
    fieldName = reflectSymbol s

    l'  = RLProxy :: RLProxy l'
    l   = RLProxy :: RLProxy l
    nil = RLProxy :: RLProxy Nil
    s   = SProxy  :: SProxy s

instance gDecodeJson_NilConsCons
  :: ( Category p
     , Top1_ f
     )
  => GDecodeJson p f g Nil (Cons s v l') r (Cons s v l') r
  where
  gDecodeJson _ _ _ = top1_ identity

else instance gDecodeJson_ConsConsCons
  :: ( Cons s v r2' r2
     , D.DecodeJson v
     , GDecodeJson p (Either JsonDecodeError) g l0' (Cons s1 v1 l1') r1 l2' r2'
     , IsSymbol s
     , Lacks s r1
     , Lacks s r2'
     , RInsert p g SProxy s l2' r2' l2 r2
     , Semigroupoid p
     )
  => GDecodeJson
        p
        (Either JsonDecodeError)
        g
        (Cons s v l0')
        (Cons s1 v1 l1')
        r1
        (Cons s v l2')
        r2
  where
  gDecodeJson _ _ object = do
    case lookup fieldName object of
      Just jsonVal -> do
        val <- D.decodeJson jsonVal
        doRest <- gDecodeJson l1 l2' object
        top1_ $ rinsert l2' l2 s val <<< doRest
      Nothing ->
        bottom2 $ getMissingFieldErrorMessage fieldName
    where
    fieldName :: String
    fieldName = reflectSymbol s

    l1  = RLProxy :: RLProxy (Cons s1 v1 l1')
    l2' = RLProxy :: RLProxy l2'
    l2  = RLProxy :: RLProxy l2
    s   = SProxy  :: SProxy s

decodeJson
  :: forall f l r
   . Bottom2 f String
  => GDecodeJson Builder f Record l Nil () l r
  => Functor f
  => RowToList r l
  => RowToList r l
  => Json
  -> f (Record r)
decodeJson json =
  map (\builder -> build builder {}) (decodeJson' json)

decodeJson'
  :: forall f g l0 l1 l2 p r1 r2
   . Bottom2 f String
  => GDecodeJson p f g l0 l1 r1 l2 r2
  => RowToList r1 l1
  => RowToList r2 l2
  => Json
  -> f (p (g r1) (g r2))
decodeJson' json = reportJson go json
  where
  go :: Object Json -> f (p (g r1) (g r2))
  go object =
    gDecodeJson
      (RLProxy :: RLProxy l1)
      (RLProxy :: RLProxy l2)
      object
