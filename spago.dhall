{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "tolerant-argonaut"
, dependencies =
    [ "argonaut-codecs"
    , "argonaut-core"
    , "arrays"
    , "console"
    , "effect"
    , "higher-order"
    , "lists"
    , "proxying"
    , "psci-support"
    , "record"
    , "struct"
    ]
, packages =
    ./packages.dhall
}
