# purescript-tolerant-argonaut

Facilities for decoding JSON records with Argonaut

## Installation

```shell
bower install purescript-tolerant-argonaut
```

## Documentation

Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-tolerant-argonaut).

## Overview

This package provides [Data.Argonaut.Decode](https://pursuit.purescript.org/packages/purescript-argonaut-codecs/docs/Data.Argonaut.Decode)-compatible decoding utilities.

It also provides a [decodeJson](https://pursuit.purescript.org/packages/purescript-tolerant-argonaut/docs/Data.Argonaut.Decode.Struct#v:decodeJson)
function with behavior modeled after the standard Argonaut `decodeJson`, the salient difference being use of `Builder` [insertions](https://pursuit.purescript.org/packages/purescript-record/docs/Record.Builder#v:insert) instead of `Record` [insertions](https://pursuit.purescript.org/packages/purescript-record/docs/Record#v:insert).

## Example 1 -- Decoding a JSON object with absent fields

Whereas `decodeJson` from [Data.Argonaut.Decode](https://pursuit.purescript.org/packages/purescript-argonaut-codecs/docs/Data.Argonaut.Decode) fails to decode a JSON object by design if the object lacks a field, `decodeJson` from [Data.Argonaut.Decode.Struct.Tolerant](https://pursuit.purescript.org/packages/purescript-tolerant-argonaut/docs/Data.Argonaut.Decode.Struct.Tolerant) interprets a field's absence, should the field have a value of a [Plus](https://pursuit.purescript.org/packages/purescript-control/docs/Control.Plus#t:Plus)-instance type, as the instance's [empty](https://pursuit.purescript.org/packages/purescript-control/docs/Control.Plus#v:empty) value.

```purescript
import Data.Argonaut.Decode (decodeJson) as D
import Data.Argonaut.Decode.Struct.Tolerant (decodeJson) as T
import Data.Argonaut.Encode (encodeJson)

emptyJson = encodeJson {}
value1 = D.decodeJson emptyJson :: Either String { a :: Maybe Int }
-- value1 == Left "JSON was missing expected field: a"
value2 = T.decodeJson emptyJson :: Either String { a :: Maybe Int }
-- value2 == Right { a: Nothing }
```

## Example 2 -- Overriding the decoding procedure for specific fields

JSON representations of data do not always match data structures in code.
`decodeJsonWith` from [Data.Argonaut.Decode.Struct.Override](https://pursuit.purescript.org/packages/purescript-tolerant-argonaut/docs/Data.Argonaut.Decode.Struct.Override), by default, delegates to standard [purescript-argonaut-codecs](https://pursuit.purescript.org/packages/purescript-argonaut-codecs/docs/Data.Argonaut.Decode.Class#v:decodeJson) JSON decoding. However, it also permits customized decoding of specific fields when their representation in JSON does not accord with their target representation.

```purescript
import Data.Argonaut.Decode (decodeJson) as D
import Data.Argonaut.Decode.Struct.Override (decodeJsonWith) as O
import Data.Argonaut.Encode (encodeJson)

data Scoops = One | Two
type Flavor = String
type IceCream = { flavor :: Flavor, scoops :: Scoops }

jsonIceCream = encodeJson { flavor: "vanilla", scoops: 2 }

iceCream :: Either String IceCream
iceCream =
    O.decodeJsonWith
      { scoops: \scoopsJson -> convert <$> D.decodeJson scoopsJson }
      jsonIceCream
  where
  convert :: Int -> Scoops
  convert 2 = Two
  convert _ = One
```

## Example 3 -- "Folding" over JSON fields.

`decodeJsonWith` in [Data.Argonaut.Decode.Struct.Cross](https://pursuit.purescript.org/packages/purescript-tolerant-argonaut/docs/Data.Argonaut.Decode.Struct.Cross), like `decodeJsonWith` in [Data.Argonaut.Decode.Struct.Override](https://pursuit.purescript.org/packages/purescript-tolerant-argonaut/docs/Data.Argonaut.Decode.Struct.Override), overrides standard JSON decoding for specified fields of a JSON object. However, unlike its counterpart, `decodeJsonWith` in Data.Argonaut.Decode.Struct.Cross, also enables a simple kind of structure folding. Data derivable via standard decoding is accumulated and made available to the decoding customizations of the remaining fields.

An example should make this clearer:

```purescript
import Data.Argonaut.Decode (decodeJson) as D
import Data.Argonaut.Decode.Struct.Cross (decodeJsonWith) as C
import Data.Argonaut.Encode (encodeJson)

data Scoops = One | Two
type Flavor = String
type Promotion = Boolean
type IceCream = { flavor :: Flavor, promotion :: Promotion, scoops :: Scoops }

jsonIceCream = encodeJson { flavor: "vanilla", promotion: true, scoops: 2 }

iceCream :: Either String IceCream
iceCream =
    C.decodeJsonWith
      { scoops: \scoopsJson { promotion } ->
                    convert promotion <$> D.decodeJson scoopsJson
      }
      jsonIceCream
  where
  convert :: Promotion -> Int -> Scoops
  convert true _ = Two
  convert _    2 = Two
  convert _    _ = One
```

In the above example, all fields of a JSON object are decoded in standard fashion, except the 'scoops' field, since its decoding depends on another field of the JSON object, the 'promotion' field. As decoding for the 'promotion' field is not overridden, `decodeJsonWith` in [Data.Argonaut.Decode.Struct.Cross](https://pursuit.purescript.org/packages/purescript-tolerant-argonaut/docs/Data.Argonaut.Decode.Struct.Cross) can make the derivation of promotion data available to the customized decoder for 'scoops'.
