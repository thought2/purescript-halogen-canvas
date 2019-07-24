{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "halogen-canvas"
, dependencies =
    [ "effect"
    , "console"
    , "psci-support"
    , "css"
    , "halogen"
    , "web-dom"
    , "sized-vectors"
    , "debug"
    , "foreign"
    , "halogen-css"
    ]
, packages =
    ../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
