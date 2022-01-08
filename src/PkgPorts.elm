port module PkgPorts exposing (audioPortFromJS, audioPortToJS, localstorage_load, localstorage_loaded, localstorage_store, ports)

import Json.Decode
import Json.Encode


ports :
    { localstorage_store : String -> Cmd msg
    , localstorage_load : {} -> Cmd msg
    , localstorage_loaded : (String -> msg) -> Sub msg
    , audioPortToJS : Json.Encode.Value -> Cmd msg
    , audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg
    }
ports =
    { localstorage_store = localstorage_store
    , localstorage_load = localstorage_load
    , localstorage_loaded = localstorage_loaded
    , audioPortToJS = audioPortToJS
    , audioPortFromJS = audioPortFromJS
    }


port localstorage_store : String -> Cmd msg


port localstorage_load : {} -> Cmd msg


port localstorage_loaded : (String -> msg) -> Sub msg


port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg
