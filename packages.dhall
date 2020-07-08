let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200615/packages.dhall sha256:5d0cfad9408c84db0a3fdcea2d708f9ed8f64297e164dc57a7cf6328706df93a

let additions =
      { higher-order =
          { dependencies =
              [ "catenable-lists"
              , "const"
              , "effect"
              , "errors"
              , "generics-rep"
              , "lists"
              , "ordered-collections"
              , "orders"
              , "profunctor"
              ]
          , repo =
              "https://github.com/matthew-hilty/purescript-higher-order.git"
          , version =
              "v0.2.0"
          }
      , proxying =
          { dependencies =
              [ "console"
              , "effect"
              , "generics-rep"
              , "prelude"
              , "test-unit"
              , "typelevel-prelude"
              ]
          , repo =
              "https://github.com/matthew-hilty/purescript-proxying.git"
          , version =
              "v1.1.0"
          }
      , struct =
          { dependencies =
              [ "argonaut"
              , "argonaut-codecs"
              , "console"
              , "effect"
              , "proxying"
              , "record"
              , "record-extra"
              , "subcategory"
              , "test-unit"
              , "variant"
              ]
          , repo =
              "https://github.com/matthew-hilty/purescript-struct.git"
          , version =
              "v1.1.0"
          }
      , subcategory =
          { dependencies =
              [ "prelude", "profunctor", "record" ]
          , repo =
              "https://github.com/matthew-hilty/purescript-subcategory.git"
          , version =
              "v0.2.0"
          }
      }

in  upstream // additions
