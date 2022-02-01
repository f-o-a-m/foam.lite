let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201007/packages.dhall sha256:35633f6f591b94d216392c9e0500207bb1fec42dd355f4fecdfd186956567b6b
      with nodetrout.repo = "https://github.com/iostat/purescript-nodetrout"
      with nodetrout.version = "c56b25c20a3b366634a7197938e0717194bd2630"

let overrides = {=}

let additions =
      { web3 =
        { dependencies =
          [ "aff"
          , "avar"
          , "console"
          , "coroutines"
          , "coroutine-transducers"
          , "debug"
          , "effect"
          , "errors"
          , "eth-core"
          , "foreign"
          , "foreign-generic"
          , "fork"
          , "free"
          , "heterogeneous"
          , "identity"
          , "parsing"
          , "partial"
          , "profunctor-lenses"
          , "proxy"
          , "psci-support"
          , "tagged"
          , "transformers"
          , "typelevel-prelude"
          , "variant"
          ]
        , repo = "https://github.com/f-o-a-m/purescript-web3"
        , version = "v3.0.0"
        }
      , web3-generator =
        { dependencies =
          [ "ansi"
          , "argonaut"
          , "console"
          , "effect"
          , "errors"
          , "eth-core"
          , "fixed-points"
          , "mkdirp"
          , "node-fs-aff"
          , "ordered-collections"
          , "prelude"
          , "psci-support"
          , "record-extra"
          , "string-parsers"
          , "web3"
          , "yargs"
          ]
        , repo = "https://github.com/f-o-a-m/purescript-web3-generator"
        , version = "v3.0.0"
        }
      , chanterelle =
        { dependencies =
          [ "console"
          , "debug"
          , "effect"
          , "foreign-object"
          , "logging"
          , "mkdirp"
          , "node-process"
          , "optparse"
          , "prelude"
          , "psci-support"
          , "solc"
          , "validation"
          , "web3"
          , "web3-generator"
          ]
        , repo = "https://github.com/f-o-a-m/chanterelle"
        , version = "v5.0.0"
        }
      , eth-core =
        { dependencies =
          [ "argonaut"
          , "bytestrings"
          , "console"
          , "debug"
          , "effect"
          , "foreign-generic"
          , "ordered-collections"
          , "parsing"
          , "prelude"
          , "psci-support"
          , "ring-modules"
          , "simple-json"
          ]
        , repo = "https://github.com/f-o-a-m/purescript-eth-core.git"
        , version = "v6.1.0"
        }
      , coroutine-transducers =
        { dependencies =
          [ "aff", "coroutines", "effect", "maybe", "psci-support" ]
        , repo =
            "https://github.com/blinky3713/purescript-coroutine-transducers"
        , version = "v1.0.0"
        }
      , solc =
        { dependencies =
          [ "aff"
          , "argonaut"
          , "console"
          , "effect"
          , "node-path"
          , "prelude"
          , "psci-support"
          , "web3"
          ]
        , repo = "https://github.com/f-o-a-m/purescript-solc"
        , version = "v2.0.0"
        }
      , mkdirp =
        { dependencies =
          [ "console"
          , "effect"
          , "either"
          , "exceptions"
          , "functions"
          , "node-fs"
          , "nullable"
          , "prelude"
          , "psci-support"
          ]
        , repo = "https://github.com/f-o-a-m/purescript-mkdirp"
        , version = "v1.0.0"
        }
      , tagged =
        { dependencies = [ "identity", "profunctor" ]
        , repo = "https://github.com/LiamGoodacre/purescript-tagged"
        , version = "v3.0.0"
        }
      , halogen-renderless =
        { dependencies = [ "prelude", "control" ]
        , repo =
            "https://github.com/purescript-deprecated/purescript-halogen-renderless"
        , version = "v0.0.4"
        }
      , react-map-gl =
        { dependencies =
          [ "prelude", "react", "web-mercator", "simple-json", "generics-rep" ]
        , repo = "https://github.com/f-o-a-m/purescript-react-map-gl.git"
        , version = "master"
        }
      , map-gl =
        { dependencies =
          [ "aff", "aff-bus", "effect", "prelude", "react-dom", "web-mercator" ]
        , repo = "https://github.com/f-o-a-m/purescript-react-map-gl.git"
        , version = "master"
        }
      , deck-gl =
        { dependencies =
          [ "effect"
          , "foreign"
          , "foreign-object"
          , "prelude"
          , "psci-support"
          , "web-mercator"
          , "react-dom"
          ]
        , repo = "https://github.com/f-o-a-m/purescript-deck-gl.git"
        , version = "master"
        }
      , web-mercator =
        { dependencies = [ "partial", "prelude", "functions" ]
        , repo = "https://github.com/f-o-a-m/purescript-web-mercator.git"
        , version = "master"
        }
      , purescript-node-datagram =
        { dependencies = [] : List Text
        , repo = "https://github.com/brandonhamilton/purescript-node-datagram.git"
        , version = "v4.0.0"
        }  
      }

in  upstream // overrides // additions
