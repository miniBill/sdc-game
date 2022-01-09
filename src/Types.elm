module Types exposing (..)

import Audio
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
import Time


type alias FrontendModel =
    Audio.Model InnerFrontendMsg InnerFrontendModel


type alias InnerFrontendModel =
    { key : Key
    , a11y : A11yOptions
    , screenSize : Maybe Size
    , page : Page
    , audio : AudioModel
    }


type alias AudioModel =
    { sources : Dict String Audio.Source
    , mainVolume : Float
    , playing : List Track
    }


type alias Sound =
    { name : String
    , duration : Int -- In milliseconds
    }


type alias Track =
    { from : Time.Posix
    , sound : Sound
    , fadingOutFrom : Maybe Time.Posix
    }


type alias A11yOptions =
    { unlockEverything : Bool
    , openDyslexic : Bool
    , fontSize : Float
    , opaqueBackgrounds : Bool
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


type alias FrontendMsg =
    Audio.Msg InnerFrontendMsg


type InnerFrontendMsg
    = -- Size
      GotResized
    | Resized (Quantity Float Pixels) (Quantity Float Pixels)
      -- URL management
    | UrlClicked UrlRequest
    | UrlChanged Url
      -- Page-specific messages
    | EditorMsg EditorMsg
    | GameMsg GameMsgTuple
      -- Audio
    | LoadedAudio Sound Audio.Source
    | TimedAudioMsg AudioMsg Time.Posix
      -- Nop
    | Nop


type alias GameMsgTuple =
    ( GameMsg, Maybe AudioMsg )


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
    = TravellingTo Float Id
    | ViewTalking Dialog ChatHistory
    | ViewMap
    | ViewQuiz Quiz
    | ViewMenu { background : String }
    | PickQuiz
    | GiveTicketAndViewMap
    | GotRandomTicket Id
    | BackTo GameModel
    | Reset
    | A11y A11yOptions
    | LocalStorageLoaded String
    | MainVolume Float


type AudioMsg
    = AudioStop
    | AudioPlay Sound
    | AudioVolume Float


type ToBackend
    = TBUpdatePerson Id (Maybe Person)
    | TBData Data


type BackendMsg
    = Connected SessionId ClientId
    | Disconnected SessionId ClientId


type ToFrontend
    = TFUpdatePerson Id (Maybe Person)
    | TFData Data
