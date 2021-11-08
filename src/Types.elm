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
    , page : Page
    }


type Page
    = Editor { preview : Preview }
    | Game GameModel


type alias GameModel =
    {}


type Preview
    = PreviewNone
    | PreviewSmall Id
    | PreviewBig Id


type alias BackendModel =
    { connectedClients : Set ClientId
    , data : Data
    , images : Dict String Bytes
    }


type FrontendMsg
    = -- URL management
      UrlClicked UrlRequest
    | UrlChanged Url
      -- File management
    | FileSelect
    | FileSelected File
    | ReadFile String
    | DownloadJson
      -- Cities editor
    | AddCity
    | UpdateCity Id (Maybe City)
    | Preview Preview


type ToBackend
    = TBUpdateCity Id (Maybe City)
    | TBData Data


type BackendMsg
    = Connected SessionId ClientId
    | Disconnected SessionId ClientId


type ToFrontend
    = TFUpdateCity Id (Maybe City)
    | TFData Data
