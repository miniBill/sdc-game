module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import File exposing (File)
import Lamdera exposing (ClientId, SessionId, Url)
import Model exposing (City, Data, Id)
import Set exposing (Set)


type alias FrontendModel =
    { key : Key
    , data : Maybe Data
    , lastError : String
    }


type alias BackendModel =
    { connectedClients : Set ClientId
    , data : Data
    , images : Dict String Bytes
    }


type FrontendMsg
    = FileSelect
    | FileSelected File
    | ReadFile String
    | UpdateCity Id (Maybe City)
    | DownloadJson
    | UrlClicked UrlRequest
    | UrlChanged Url


type ToBackend
    = TBUpdateCity Id (Maybe City)
    | TBData Data


type BackendMsg
    = Connected SessionId ClientId
    | Disconnected SessionId ClientId


type ToFrontend
    = TFUpdateCity Id (Maybe City)
    | TFData Data
