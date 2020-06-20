module Data.Argonaut.Decode.Struct.Tolerant.GDecodeJson
  ( class GDecodeJson
  , gDecodeJson
  ) where

import Control.Plus (class Plus, empty)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson) as D
import Data.Either (Either)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Operator.Bottom (bottom2)
import Data.Operator.Top (class Top1_, top1_)
import Data.Struct (class RInsert, rinsert)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Foreign.Object (Object, lookup)
import Prelude (class Category, class Semigroupoid, bind, identity, ($), (<<<))
import Type.Row (class Cons, class Lacks)
import Type.RowList (Cons, Nil, RLProxy(RLProxy), kind RowList)

class GDecodeJson
  (p  :: Type -> Type -> Type)
  (f  :: Type -> Type)
  (g  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  gDecodeJson
    :: forall (h :: RowList -> Type)
     . h l0
    -> h l1
    -> Object Json
    -> f (p (g r0) (g r1))

instance gDecodeJson_NilNilNil
  :: ( Category p
     , Top1_ f
     )
  => GDecodeJson p f g Nil () Nil ()
  where
  gDecodeJson _ _ _ = top1_ identity

instance gDecodeJson_ConsNilCons_Plus
  :: ( Cons s (f v) r' r
     , D.DecodeJson (f v)
     , GDecodeJson p (Either JsonDecodeError) g Nil () l' r'
     , IsSymbol s
     , Lacks s r'
     , Plus f
     , RInsert p g SProxy s l' r' l r
     , Semigroupoid p
     )
  => GDecodeJson p (Either JsonDecodeError) g Nil () (Cons s (f v) l') r
  where
  gDecodeJson _ _ object = do
    doRest <- gDecodeJson nil l' object
    case lookup fieldName object of
      Just jsonVal -> do
        (val :: f v) <- D.decodeJson jsonVal
        top1_ $ rinsert l' l s val <<< doRest
      Nothing ->
        top1_ $ rinsert l' l s (empty :: f v) <<< doRest
    where
    fieldName :: String
    fieldName = reflectSymbol s

    l'  = RLProxy :: RLProxy l'
    l   = RLProxy :: RLProxy l
    nil = RLProxy :: RLProxy Nil
    s   = SProxy  :: SProxy s

else instance gDecodeJson_ConsNilCons_nonPlus
  :: ( Cons s v r' r
     , D.DecodeJson v
     , GDecodeJson p (Either JsonDecodeError) g Nil () l' r'
     , IsSymbol s
     , Lacks s r'
     , RInsert p g SProxy s l' r' l r
     , Semigroupoid p
     )
  => GDecodeJson p (Either JsonDecodeError) g Nil () (Cons s v l') r
  where
  gDecodeJson _ _ object = do
    case lookup fieldName object of
      Just jsonVal -> do
        val <- D.decodeJson jsonVal
        doRest <- gDecodeJson nil l' object
        top1_ $ rinsert l' l s val <<< doRest
      Nothing ->
        bottom2 $ AtKey fieldName MissingValue
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
  => GDecodeJson p f g (Cons s v l') r (Cons s v l') r
  where
  gDecodeJson _ _ _ = top1_ identity

else instance gDecodeJson_ConsConsCons_Plus
  :: ( Cons s (f v) r1' r1
     , D.DecodeJson (f v)
     , GDecodeJson p (Either JsonDecodeError) g (Cons s1 v1 l0') r0 l1' r1'
     , IsSymbol s
     , Lacks s r0
     , Lacks s r1'
     , Plus f
     , RInsert p g SProxy s l1' r1' l1 r1
     , Semigroupoid p
     )
  => GDecodeJson p (Either JsonDecodeError) g (Cons s1 v1 l0') r0 (Cons s (f v) l1') r1
  where
  gDecodeJson _ _ object = do
    doRest <- gDecodeJson l0 l1' object
    case lookup fieldName object of
      Just jsonVal -> do
        (val :: f v) <- D.decodeJson jsonVal
        top1_ $ rinsert l1' l1 s val <<< doRest
      Nothing ->
        top1_ $ rinsert l1' l1 s (empty :: f v) <<< doRest
    where
    fieldName :: String
    fieldName = reflectSymbol s

    l0  = RLProxy :: RLProxy (Cons s1 v1 l0')
    l1' = RLProxy :: RLProxy l1'
    l1  = RLProxy :: RLProxy l1
    s   = SProxy  :: SProxy s

else instance gDecodeJson_ConsConsCons_nonPlus
  :: ( Cons s v r1' r1
     , D.DecodeJson v
     , GDecodeJson p (Either JsonDecodeError) g (Cons s1 v1 l0') r0 l1' r1'
     , IsSymbol s
     , Lacks s r0
     , Lacks s r1'
     , RInsert p g SProxy s l1' r1' l1 r1
     , Semigroupoid p
     )
  => GDecodeJson p (Either JsonDecodeError) g (Cons s1 v1 l0') r0 (Cons s v l1') r1
  where
  gDecodeJson _ _ object = do
    case lookup fieldName object of
      Just jsonVal -> do
        val <- D.decodeJson jsonVal
        doRest <- gDecodeJson l0 l1' object
        top1_ $ rinsert l1' l1 s val <<< doRest
      Nothing ->
        bottom2 $ AtKey fieldName MissingValue
    where
    fieldName :: String
    fieldName = reflectSymbol s

    l0  = RLProxy :: RLProxy (Cons s1 v1 l0')
    l1' = RLProxy :: RLProxy l1'
    l1  = RLProxy :: RLProxy l1
    s   = SProxy  :: SProxy s
