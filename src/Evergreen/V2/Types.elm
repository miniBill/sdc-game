module Evergreen.V2.Types exposing (..)

import Browser
import Browser.Navigation
import Bytes
import Dict
import File
import Lamdera
import Set


type alias Scene =
    { text : String
    , next : List ( String, String )
    , image : String
    }


type alias Data =
    Dict.Dict String Scene


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , data : Maybe Data
    , images : Dict.Dict String Bytes.Bytes
    , scale : Int
    , hoveredScene : Maybe String
    }


type alias BackendModel =
    { connectedClients : Set.Set Lamdera.ClientId
    , data : Data
    , images : Dict.Dict String Bytes.Bytes
    }


type FrontendMsg
    = FileSelect
    | FileSelected File.File
    | ReadFile String
    | ImageSelect
    | ImageSelected File.File
    | ReadImage String Bytes.Bytes
    | Replace String ( String, Scene )
    | ReplaceNext String (Maybe Int) ( String, String )
    | GenerateC
    | DownloadJson
    | UrlClicked Browser.UrlRequest
    | UrlChanged Lamdera.Url
    | Scale Int
    | SelectScene (Maybe String)


type ToBackend
    = TBReplace String ( String, Scene )
    | TBData Data
    | TBGetImageList
    | TBImage String Bytes.Bytes


type BackendMsg
    = Connected Lamdera.SessionId Lamdera.ClientId
    | Disconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = TFReplace String ( String, Scene )
    | TFData Data
    | TFGotImageList (Dict.Dict String Bytes.Bytes)
    | TFImage String Bytes.Bytes
