port module PkgPorts exposing (localstorage_load, localstorage_loaded, localstorage_store, ports)


ports :
    { localstorage_store : String -> Cmd msg
    , localstorage_load : {} -> Cmd msg
    , localstorage_loaded : (String -> msg) -> Sub msg
    }
ports =
    { localstorage_store = localstorage_store
    , localstorage_load = localstorage_load
    , localstorage_loaded = localstorage_loaded
    }


port localstorage_store : String -> Cmd msg


port localstorage_load : {} -> Cmd msg


port localstorage_loaded : (String -> msg) -> Sub msg
