module Backend exposing (..)

import Dict
import Lamdera exposing (ClientId, SessionId)
import Set
import Types exposing (..)


app :
    { init : ( BackendModel, Cmd BackendMsg )
    , update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , subscriptions : BackendModel -> Sub BackendMsg
    }
app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { connectedClients = Set.empty
      , data = Dict.empty
      , images = Dict.empty
      }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        Connected _ clientId ->
            ( { model | connectedClients = Set.insert clientId model.connectedClients }
            , Lamdera.sendToFrontend clientId <| TFData model.data
            )

        Disconnected _ clientId ->
            ( { model | connectedClients = Set.remove clientId model.connectedClients }, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        TBUpdatePerson id person ->
            ( { model | data = Dict.update id (always person) model.data }
            , almostBroadcast model clientId <| TFUpdatePerson id person
            )

        TBData data ->
            ( { model | data = data }
            , almostBroadcast model clientId <| TFData data
            )


almostBroadcast : BackendModel -> ClientId -> ToFrontend -> Cmd BackendMsg
almostBroadcast model clientId msg =
    model.connectedClients
        |> Set.toList
        |> List.filter ((/=) clientId)
        |> List.map (\cid -> Lamdera.sendToFrontend cid msg)
        |> Cmd.batch


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect Connected
        , Lamdera.onDisconnect Disconnected
        ]
