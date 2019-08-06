{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "tolerant-argonaut"
, dependencies =
    [ "argonaut-codecs"
    , "argonaut-core"
    , "arrays"
    , "console"
    , "effect"
    , "higher-order"
    , "lists"
    , "psci-support"
    , "record"
    , "struct"
    , "typelevel-prelude"
    ]
, packages =
    ./packages.dhall
}
