module Backend exposing (..)

import Dict
import Lamdera exposing (ClientId, SessionId)
import Model exposing (replaceScene)
import Set
import Types exposing (..)


type alias Model =
    BackendModel


app :
    { init : ( Model, Cmd BackendMsg )
    , update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
    , updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
    , subscriptions : Model -> Sub BackendMsg
    }
app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { connectedClients = Set.empty
      , data = Dict.empty
      , images = Dict.empty
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        Connected _ clientId ->
            ( { model | connectedClients = Set.insert clientId model.connectedClients }
            , Cmd.batch
                [ Lamdera.sendToFrontend clientId <| TFData model.data
                , Lamdera.sendToFrontend clientId <| TFGotImageList model.images
                ]
            )

        Disconnected _ clientId ->
            ( { model | connectedClients = Set.remove clientId model.connectedClients }, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        TBReplace oldKey ( newKey, newValue ) ->
            ( { model | data = replaceScene oldKey newKey newValue model.data }
            , almostBroadcast model clientId <| TFReplace oldKey ( newKey, newValue )
            )

        TBData data ->
            ( { model | data = data }
            , almostBroadcast model clientId <| TFData data
            )

        TBGetImageList ->
            ( model
            , Lamdera.sendToFrontend clientId <| TFGotImageList model.images
            )

        TBImage name image ->
            ( { model | images = Dict.insert name image model.images }
            , almostBroadcast model clientId <| TFImage name image
            )


almostBroadcast : Model -> ClientId -> ToFrontend -> Cmd BackendMsg
almostBroadcast model clientId msg =
    model.connectedClients
        |> Set.toList
        |> List.filter ((/=) clientId)
        |> List.map (\cid -> Lamdera.sendToFrontend cid msg)
        |> Cmd.batch


subscriptions : Model -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect Connected
        , Lamdera.onDisconnect Disconnected
        ]
