module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import File exposing (File)
import Http
import Lamdera exposing (ClientId, SessionId, Url)
import Set exposing (Set)


type alias FrontendModel =
    { key : Key
    , data : Maybe Data
    , images : List String
    }


type alias Data =
    Dict String Scene


type alias BackendModel =
    { connectedClients : Set ClientId
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
    | UrlClicked UrlRequest
    | UrlChanged Url
    | GotImageList (Result Http.Error (List String))


type ToBackend
    = TBReplace String ( String, Scene )
    | TBData Data


type BackendMsg
    = Connected SessionId ClientId
    | Disconnected SessionId ClientId


type ToFrontend
    = TFReplace String ( String, Scene )
    | TFData Data


type alias Scene =
    { text : String
    , next : List ( String, String )
    , image : String
    }
