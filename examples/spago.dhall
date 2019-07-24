{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "halogen-canvas-examples"
, dependencies =
    [ "effect"
    , "console"
    , "psci-support"
    , "css"
    , "halogen"
    , "web-dom"
    , "sized-vectors"
    , "debug"
    , "halogen-css"
    ]
, packages =
    ../packages.dhall
, sources =
    [ "../lib/src/**/*.purs", "../test/**/*.purs", "src/**/*.purs" ]
}
