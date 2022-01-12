module Evergreen.V26.Types exposing (..)

import Audio
import Browser
import Browser.Navigation
import Dict
import Evergreen.V26.Model
import File
import Lamdera
import Pixels
import Quantity
import Set
import Time


type EditorMsg
    = AddPerson
    | UpdatePerson Evergreen.V26.Model.Id (Maybe Evergreen.V26.Model.Person)
    | EditPerson Evergreen.V26.Model.Id
    | FileSelect
    | FileSelected File.File
    | ReadFile String
    | DownloadJson


type GameMsg
    = TravellingTo Float Evergreen.V26.Model.Id
    | ViewTalking Evergreen.V26.Model.Dialog Evergreen.V26.Model.ChatHistory
    | ViewMap
    | ViewQuiz Evergreen.V26.Model.Quiz
    | ViewMenu
        { background : String
        }
    | PickQuiz
    | GiveTicketAndViewMap
    | GotRandomTicket Evergreen.V26.Model.Id
    | BackTo Evergreen.V26.Model.GameModel
    | Reset
    | A11y Evergreen.V26.Model.A11yOptions
    | LocalStorageLoaded String
    | MainVolume Float
    | MusicVolume Float
    | EffectsVolume Float


type TrackKind
    = Music
    | Effect


type AudioMsg
    = AudioStop
    | AudioPlay Evergreen.V26.Model.Sound Bool TrackKind
    | AudioMainVolume Float
    | AudioMusicVolume Float
    | AudioEffectsVolume Float


type alias GameMsgTuple =
    ( GameMsg, Maybe AudioMsg )


type InnerFrontendMsg
    = GotResized
    | Resized (Quantity.Quantity Float Pixels.Pixels) (Quantity.Quantity Float Pixels.Pixels)
    | UrlClicked Browser.UrlRequest
    | UrlChanged Lamdera.Url
    | EditorMsg EditorMsg
    | GameMsg GameMsgTuple
    | LoadedAudio Evergreen.V26.Model.Sound Audio.Source
    | TimedAudioMsg AudioMsg Time.Posix
    | LoadSoundLibrary
    | Nop


type alias Size =
    { width : Quantity.Quantity Float Pixels.Pixels
    , height : Quantity.Quantity Float Pixels.Pixels
    }


type alias EditorModel =
    { lastError : String
    , currentPerson : Maybe Evergreen.V26.Model.Id
    }


type OuterGameModel
    = LoadingData
    | DataEmpty
    | LoadedData Evergreen.V26.Model.Data Evergreen.V26.Model.SharedGameModel Evergreen.V26.Model.GameModel


type Page
    = Editor (Maybe Evergreen.V26.Model.Data) EditorModel
    | Game OuterGameModel


type alias Track =
    { from : Time.Posix
    , sound : Evergreen.V26.Model.Sound
    , fadingOutFrom : Maybe Time.Posix
    , loop : Bool
    , kind : TrackKind
    }


type alias AudioModel =
    { sources : Dict.Dict String Audio.Source
    , mainVolume : Float
    , musicVolume : Float
    , effectsVolume : Float
    , playing : List Track
    }


type alias InnerFrontendModel =
    { key : Browser.Navigation.Key
    , a11y : Evergreen.V26.Model.A11yOptions
    , screenSize : Maybe Size
    , page : Page
    , audio : AudioModel
    }


type alias FrontendModel =
    Audio.Model InnerFrontendMsg InnerFrontendModel


type alias BackendModel =
    { connectedClients : Set.Set Lamdera.ClientId
    , data : Evergreen.V26.Model.Data
    }


type alias FrontendMsg =
    Audio.Msg InnerFrontendMsg


type ToBackend
    = TBUpdatePerson Evergreen.V26.Model.Id (Maybe Evergreen.V26.Model.Person)
    | TBData Evergreen.V26.Model.Data


type BackendMsg
    = Connected Lamdera.SessionId Lamdera.ClientId
    | Disconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = TFUpdatePerson Evergreen.V26.Model.Id (Maybe Evergreen.V26.Model.Person)
    | TFData Evergreen.V26.Model.Data
