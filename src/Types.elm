module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import File exposing (File)
import Lamdera exposing (ClientId, SessionId, Url)
import Set exposing (Set)


type alias FrontendModel =
    { key : Key
    , data : Maybe Data
    , images : Dict String Bytes
    , scale : Int
    , hoveredScene : Maybe String
    }


type alias Data =
    Dict String Scene


type alias BackendModel =
    { connectedClients : Set ClientId
    , data : Data
    , images : Dict String Bytes
    }


type FrontendMsg
    = FileSelect
    | FileSelected File
    | ReadFile String
    | ImageSelect
    | ImageSelected File
    | ReadImage String Bytes
    | Replace String ( String, Scene )
    | ReplaceNext String (Maybe Int) ( String, String )
    | GenerateC
    | DownloadJson
    | UrlClicked UrlRequest
    | UrlChanged Url
    | Scale Int
    | SelectScene (Maybe String)


type ToBackend
    = TBReplace String ( String, Scene )
    | TBData Data
    | TBGetImageList
    | TBImage String Bytes


type BackendMsg
    = Connected SessionId ClientId
    | Disconnected SessionId ClientId


type ToFrontend
    = TFReplace String ( String, Scene )
    | TFData Data
    | TFGotImageList (Dict String Bytes)
    | TFImage String Bytes


type alias Scene =
    { text : String
    , next : List ( String, String )
    , image : String
    }
