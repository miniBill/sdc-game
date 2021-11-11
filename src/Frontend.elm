module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Codec
import Codecs
import Dict
import Editors
import Element exposing (Element, fill, height, width)
import File
import File.Download
import File.Select
import Frontend.Editor exposing (viewEditor)
import Frontend.Game exposing (viewGame)
import Hex
import Html
import Json.Decode
import Lamdera exposing (Key, Url)
import List.Extra
import Model exposing (Data, Id, Person)
import Random
import Task
import Theme
import Types exposing (EditorModel, FrontendModel, FrontendMsg(..), GameModel(..), Page(..), ToBackend(..), ToFrontend(..))
import Url
import Url.Parser


app :
    { init : Url -> Key -> ( FrontendModel, Cmd FrontendMsg )
    , view : FrontendModel -> Browser.Document FrontendMsg
    , update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , subscriptions : FrontendModel -> Sub FrontendMsg
    , onUrlRequest : UrlRequest -> FrontendMsg
    , onUrlChange : Url -> FrontendMsg
    }
app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , view =
            \model ->
                { title = "SDC Game"
                , body =
                    [ css
                    , Element.layout [ Theme.fontSizes.normal, height fill, width fill ] <|
                        view model
                    ]
                }
        , update = update
        , subscriptions = subscriptions
        , updateFromBackend = updateFromBackend
        }


css : Html.Html FrontendMsg
css =
    let
        content =
            """
            select {
                font-size: """ ++ String.fromInt Theme.fontSize ++ """px;
            }
            """
    in
    Html.node "style" [] [ Html.text content ]


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        TFUpdatePerson id person ->
            ( updatePersonInModel id person model
            , Cmd.none
            )

        TFData data ->
            ( updatePage model
                (\_ editorModel ->
                    ( Just data, editorModel )
                )
                (\gameModel ->
                    case gameModel of
                        LoadingData ->
                            gotGameData data

                        DataEmpty ->
                            gotGameData data

                        ViewingMap _ m ->
                            ViewingMap data m

                        ViewingPerson _ m ->
                            ViewingPerson data m

                        Talking _ m ->
                            Talking data m
                )
            , Cmd.none
            )


gotGameData : Data -> GameModel
gotGameData data =
    case
        data
            |> Dict.toList
            |> List.Extra.find (\( _, p ) -> p.name == "Orla")
    of
        Just ( id, _ ) ->
            ViewingPerson data { currentPerson = id }

        Nothing ->
            DataEmpty


updatePersonInModel : Id -> Maybe Person -> FrontendModel -> FrontendModel
updatePersonInModel id person model =
    let
        updater data =
            Dict.update id (always person) data
    in
    updatePage model
        (\data editorModel ->
            ( Maybe.map updater data, editorModel )
        )
        (\gameModel ->
            case gameModel of
                LoadingData ->
                    LoadingData

                DataEmpty ->
                    case person of
                        Nothing ->
                            DataEmpty

                        Just p ->
                            ViewingMap
                                (Dict.singleton id p)
                                { currentPerson = id }

                ViewingMap data m ->
                    ViewingMap (updater data) m

                ViewingPerson data m ->
                    ViewingPerson (updater data) m

                Talking data m ->
                    Talking (updater data) m
        )


updatePage :
    FrontendModel
    -> (Maybe Data -> EditorModel -> ( Maybe Data, EditorModel ))
    -> (GameModel -> GameModel)
    -> FrontendModel
updatePage model editor game =
    { model
        | page =
            case model.page of
                Editor data editorModel ->
                    let
                        ( data_, editorModel_ ) =
                            editor data editorModel
                    in
                    Editor data_ editorModel_

                Game gameModel ->
                    Game <| game gameModel
    }


urlToPage : Url -> Page
urlToPage url =
    let
        initEditor =
            Editor Nothing { lastError = "" }

        parser =
            Url.Parser.oneOf
                [ Url.Parser.map initEditor <|
                    Url.Parser.s "editor"
                , Url.Parser.map (Game LoadingData) Url.Parser.top
                ]
    in
    Url.Parser.parse parser url
        |> Maybe.withDefault (Game LoadingData)


init : Url -> Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , page = urlToPage url
      }
    , Cmd.none
    )


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions _ =
    Sub.none


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case ( msg, model.page ) of
        ( _, Editor Nothing _ ) ->
            ( model, Cmd.none )

        ( _, Game LoadingData ) ->
            ( model, Cmd.none )

        ( _, Game DataEmpty ) ->
            ( model, Cmd.none )

        ( DownloadJson, Game _ ) ->
            ( model, Cmd.none )

        ( AddPerson, Game _ ) ->
            ( model, Cmd.none )

        ( ReadFile _, Game _ ) ->
            ( model, Cmd.none )

        ( ViewPerson _, Editor _ _ ) ->
            ( model, Cmd.none )

        ( ViewMap _, Editor _ _ ) ->
            ( model, Cmd.none )

        ( TalkTo _ _, Editor _ _ ) ->
            ( model, Cmd.none )

        ( UrlClicked urlRequest, _ ) ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        ( UrlChanged url, _ ) ->
            ( { model | page = urlToPage url }, Cmd.none )

        ( FileSelect, _ ) ->
            ( model, File.Select.file [ "application/json" ] FileSelected )

        ( FileSelected file, _ ) ->
            ( model, Task.perform ReadFile <| File.toString file )

        ( UpdatePerson id person, _ ) ->
            ( updatePersonInModel id person model
            , Lamdera.sendToBackend <| TBUpdatePerson id person
            )

        ( ReadFile str, Editor data _ ) ->
            let
                ( data_, editorModel_, cmd ) =
                    case Codec.decodeString Codecs.dataCodec str of
                        Err err ->
                            ( data
                            , { lastError = Json.Decode.errorToString err }
                            , Cmd.none
                            )

                        Ok newData ->
                            ( Just newData
                            , { lastError = "" }
                            , Lamdera.sendToBackend <| TBData newData
                            )
            in
            ( { model | page = Editor data_ editorModel_ }
            , cmd
            )

        ( DownloadJson, Editor (Just data) _ ) ->
            ( model
            , File.Download.string "sdc-game.json" "application/json" <|
                Codec.encodeToString 0 Codecs.dataCodec data
            )

        ( AddPerson, Editor (Just _) _ ) ->
            ( model
            , Random.int 0 Random.maxInt
                |> Random.map Hex.toString
                |> Random.generate (\newId -> UpdatePerson newId (Just Editors.personDefault))
            )

        ( ViewPerson id, Game (ViewingMap data _) ) ->
            ( { model
                | page =
                    Game
                        (ViewingPerson data { currentPerson = id })
              }
            , Cmd.none
            )

        ( ViewMap id, Game (ViewingPerson data _) ) ->
            ( { model
                | page =
                    Game
                        (ViewingMap data { currentPerson = id })
              }
            , Cmd.none
            )

        ( ViewMap id, Game (Talking data _) ) ->
            ( { model
                | page =
                    Game
                        (ViewingMap data { currentPerson = id })
              }
            , Cmd.none
            )

        ( TalkTo id dialog, Game (ViewingPerson data _) ) ->
            ( { model
                | page =
                    Game
                        (Talking data { currentPerson = id, currentDialog = dialog })
              }
            , Cmd.none
            )

        ( TalkTo id dialog, Game (Talking data _) ) ->
            ( { model
                | page =
                    Game
                        (Talking data { currentPerson = id, currentDialog = dialog })
              }
            , Cmd.none
            )

        ( TalkTo _ _, Game _ ) ->
            ( model, Cmd.none )

        ( ViewPerson _, Game _ ) ->
            ( model, Cmd.none )

        ( ViewMap _, Game _ ) ->
            ( model, Cmd.none )


view : FrontendModel -> Element FrontendMsg
view model =
    case model.page of
        Game gameModel ->
            viewGame gameModel

        Editor data editorModel ->
            viewEditor data editorModel
