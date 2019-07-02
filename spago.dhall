{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "mastodon-client"
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, dependencies =
    [ "prelude"
    , "console"
    , "effect"
    , "aff"
    , "halogen"
    , "routing"
    , "routing-duplex"
    , "remotedata"
    , "nullable"
    , "simple-json"
    , "simple-ajax"
    ]
}
