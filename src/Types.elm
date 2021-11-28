module Types exposing (..)

import AltMath.Matrix3 exposing (Mat3)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import File exposing (File)
import Lamdera exposing (ClientId, SessionId, Url)
import Model exposing (Data, Dialog, Id, Person, Quiz)
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


type alias SharedGameModel =
    { currentPerson : Id
    , tickets : Set Id
    }


type GameModel
    = ViewingMap MapModel
    | ViewingPerson
    | Talking TalkingModel
    | Quizzing Quiz


type alias MapModel =
    { transformation : Mat3 }


type alias TalkingModel =
    { chatHistory : ChatHistory
    , currentDialog : Dialog
    }


type alias ChatHistory =
    List ( Maybe { image : String, name : String }, String )


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
    | ViewDialog Dialog ChatHistory
    | ViewMap
    | PickQuiz
    | ViewQuiz Quiz
    | GiveTicketAndViewMap
    | GotRandomTicket Id


type ToBackend
    = TBUpdatePerson Id (Maybe Person)
    | TBData Data


type BackendMsg
    = Connected SessionId ClientId
    | Disconnected SessionId ClientId


type ToFrontend
    = TFUpdatePerson Id (Maybe Person)
    | TFData Data
