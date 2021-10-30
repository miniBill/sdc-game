module Backend exposing (..)

import Dict
import Lamdera exposing (ClientId, SessionId)
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
            , Lamdera.sendToFrontend clientId <| TFData model.data
            )

        Disconnected _ clientId ->
            ( { model | connectedClients = Set.remove clientId model.connectedClients }, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        TBUpdateCity id city ->
            ( { model | data = Dict.update id (always city) model.data }
            , almostBroadcast model clientId <| TFUpdateCity id city
            )

        TBData data ->
            ( { model | data = data }
            , almostBroadcast model clientId <| TFData data
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
