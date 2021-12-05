module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import File exposing (File)
import Lamdera exposing (ClientId, SessionId, Url)
import Model exposing (ChatHistory, Data, Dialog, GameModel, Id, Person, Quiz, SharedGameModel)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Set exposing (Set)


type alias FrontendModel =
    { key : Key

    -- TODO: s/size/screenSize
    , size : Maybe Size
    , page : Page
    }


type alias Size =
    { width : Quantity Float Pixels
    , height : Quantity Float Pixels
    }


type Page
    = Editor (Maybe Data) EditorModel
    | Game OuterGameModel


type alias EditorModel =
    { lastError : String
    , currentPerson : Maybe Id
    }


type OuterGameModel
    = LoadingData
    | DataEmpty
    | LoadedData Data SharedGameModel GameModel


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
      -- Page-specific messages
    | EditorMsg EditorMsg
    | GameMsg GameMsg


type EditorMsg
    = AddPerson
    | UpdatePerson Id (Maybe Person)
    | EditPerson Id
      -- File management
    | FileSelect
    | FileSelected File
    | ReadFile String
    | DownloadJson


type GameMsg
    = ViewPerson Id
    | ViewTalking Dialog ChatHistory
    | ViewMap
    | ViewQuiz Quiz
    | ViewMenu { background : String }
    | PickQuiz
    | GiveTicketAndViewMap
    | GotRandomTicket Id
    | BackTo GameModel
    | Reset
    | Cheat
    | LSLoaded String


type ToBackend
    = TBUpdatePerson Id (Maybe Person)
    | TBData Data


type BackendMsg
    = Connected SessionId ClientId
    | Disconnected SessionId ClientId


type ToFrontend
    = TFUpdatePerson Id (Maybe Person)
    | TFData Data
