module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import File exposing (File)
import Lamdera exposing (ClientId, SessionId, Url)
import Model exposing (Data, Id, Person)
import Set exposing (Set)


type alias FrontendModel =
    { key : Key
    , data : Maybe Data
    , lastError : String
    , page : Page
    }


type Page
    = Editor {}
    | Game GameModel


type alias GameModel =
    {}


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
    | AddPerson
    | UpdatePerson Id (Maybe Person)


type ToBackend
    = TBUpdatePerson Id (Maybe Person)
    | TBData Data


type BackendMsg
    = Connected SessionId ClientId
    | Disconnected SessionId ClientId


type ToFrontend
    = TFUpdatePerson Id (Maybe Person)
    | TFData Data
