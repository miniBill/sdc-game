module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import File exposing (File)
import Lamdera exposing (ClientId, SessionId, Url)
import Model exposing (Data, Dialog, Id, Person)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Set exposing (Set)


type alias FrontendModel =
    { key : Key
    , size : Maybe Size
    , page : Page
    }


type alias Size =
    { width : Quantity Float Pixels
    , height : Quantity Float Pixels
    }


type Page
    = Editor (Maybe Data) EditorModel
    | Game GameModel


type alias EditorModel =
    { lastError : String
    , currentPerson : Maybe Id
    }


type GameModel
    = LoadingData
    | DataEmpty
    | ViewingMap Data { currentPerson : Id }
    | ViewingPerson Data { currentPerson : Id }
    | Talking Data { currentPerson : Id, currentDialog : Dialog }


type alias BackendModel =
    { connectedClients : Set ClientId
    , data : Data
    , images : Dict String Bytes
    }


type FrontendMsg
    = -- Size
      GotResized
    | Resized (Quantity Float Pixels) (Quantity Float Pixels)
      -- URL management
    | UrlClicked UrlRequest
    | UrlChanged Url
      -- File management
    | FileSelect
    | FileSelected File
    | ReadFile String
    | DownloadJson
      -- Cities editor
    | AddPerson
    | UpdatePerson Id (Maybe Person)
    | EditPerson Id
      -- Game
    | ViewPerson Id
    | TalkTo Id Dialog
    | ViewMap Id


type ToBackend
    = TBUpdatePerson Id (Maybe Person)
    | TBData Data


type BackendMsg
    = Connected SessionId ClientId
    | Disconnected SessionId ClientId


type ToFrontend
    = TFUpdatePerson Id (Maybe Person)
    | TFData Data
