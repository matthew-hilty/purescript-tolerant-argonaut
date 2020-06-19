let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190725/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200615/packages.dhall sha256:5d0cfad9408c84db0a3fdcea2d708f9ed8f64297e164dc57a7cf6328706df93a

let overrides =
      { argonaut-codecs =
              upstream.argonaut-codecs
          //  { repo = "https://github.com/purescript-contrib/purescript-argonaut-codecs.git"
              , version = "master"
              }
      , argonaut-generic =
          upstream.argonaut-generic // { repo = "https://github.com/srghma/purescript-argonaut-generic.git", version = "master" }
      , argonaut-traversals =
          upstream.argonaut-traversals // { repo = "https://github.com/srghma/purescript-argonaut-traversals.git", version = "master" }
      , argonaut =
          upstream.argonaut // { repo = "https://github.com/srghma/purescript-argonaut.git", version = "patch-1" }
      }

let additions =
      { higher-order =
          mkPackage
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
            "https://github.com/matthew-hilty/purescript-higher-order.git"
            "v0.2.0"
      , proxying =
          mkPackage
            [ "console"
            , "effect"
            , "generics-rep"
            , "prelude"
            , "test-unit"
            , "typelevel-prelude"
            ]
            "https://github.com/matthew-hilty/purescript-proxying.git"
            "v1.1.0"
      , struct =
          mkPackage
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
            "https://github.com/matthew-hilty/purescript-struct.git"
            "v1.1.0"
      , subcategory =
          mkPackage
            [ "prelude", "profunctor", "record" ]
            "https://github.com/matthew-hilty/purescript-subcategory.git"
            "v0.2.0"
      }

in  upstream // overrides // additions
