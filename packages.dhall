let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190725/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190725/src/packages.dhall sha256:60cc03d2c3a99a0e5eeebb16a22aac219fa76fe6a1686e8c2bd7a11872527ea3

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
        [ "prelude"
        , "profunctor"
        , "record"
        ]
        "https://github.com/matthew-hilty/purescript-subcategory.git"
        "v0.2.0"
  }

in  upstream // additions
