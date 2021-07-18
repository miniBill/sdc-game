module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import File
import Lamdera
import Set


type alias Scene =
    { text : String
    , next : List ( String, String )
    , image : String
    }


type alias Data =
    Dict.Dict String Scene


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , data : Maybe Data
    }


type alias BackendModel =
    { connectedClients : Set.Set Lamdera.ClientId
    , data : Data
    }


type FrontendMsg
    = FileSelect
    | FileSelected File
    | ReadFile String
    | Replace String ( String, Scene )
    | ReplaceNext String (Maybe Int) ( String, String )
    | GenerateC
    | DownloadJson
    | UrlClicked Browser.UrlRequest
    | UrlChanged Lamdera.Url


type ToBackend
    = TBReplace String ( String, Scene )
    | TBData Data


type BackendMsg
    = Connected Lamdera.SessionId Lamdera.ClientId
    | Disconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = TFReplace String ( String, Scene )
    | TFData Data
