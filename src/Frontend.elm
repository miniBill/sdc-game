module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Codec
import Codecs
import Dict
import Editors
import Element exposing (Element, fill, height, width)
import Element.WithUnits
import File
import File.Download
import File.Select
import Frontend.Common
import Frontend.Editor
import Frontend.Game
import Hex
import Html
import Html.Attributes
import Json.Decode
import Lamdera exposing (Key, Url)
import List.Extra
import Model exposing (Data, Id, Person)
import Pixels
import Random
import Task
import Theme
import Types exposing (EditorModel, EditorMsg(..), FrontendModel, FrontendMsg(..), GameModel(..), GameMsg(..), OuterGameModel(..), Page(..), ToBackend(..), ToFrontend(..))
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
                    , Element.layout
                        [ Theme.fontSizes.normal
                        , height fill
                        , width fill
                        , Element.htmlAttribute <| Html.Attributes.id "main"
                        ]
                        (view model)
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

                        LoadedData _ shared inner ->
                            LoadedData data shared inner
                )
            , Cmd.none
            )


gotGameData : Data -> OuterGameModel
gotGameData data =
    case
        data
            |> Dict.toList
            |> List.Extra.find (\( _, p ) -> p.name == "Orla")
    of
        Just ( id, _ ) ->
            LoadedData data { currentPerson = id } ViewingPerson

        Nothing ->
            -- Data must contain "Orla", as a starting point
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
                            gotGameData (Dict.singleton id p)

                LoadedData data shared inner ->
                    LoadedData (updater data) shared inner
        )


updatePage :
    FrontendModel
    -> (Maybe Data -> EditorModel -> ( Maybe Data, EditorModel ))
    -> (OuterGameModel -> OuterGameModel)
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
            Editor Nothing
                { lastError = ""
                , currentPerson = Nothing
                }

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
      , size = Nothing
      }
    , getSizeCmd
    )


getSizeCmd : Cmd FrontendMsg
getSizeCmd =
    Task.perform
        (\{ viewport } ->
            Resized
                (Pixels.pixels viewport.width)
                (Pixels.pixels viewport.height)
        )
        Browser.Dom.getViewport


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions _ =
    Browser.Events.onResize (\_ _ -> GotResized)


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case ( msg, model.page ) of
        -- Handle generic messages
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

        ( Resized width height, _ ) ->
            ( { model
                | size =
                    Just
                        { width = width
                        , height = height
                        }
              }
            , Cmd.none
            )

        ( GotResized, _ ) ->
            ( model, getSizeCmd )

        -- Ignore stray cross-page messages
        ( EditorMsg _, Game _ ) ->
            ( model, Cmd.none )

        ( GameMsg _, Editor _ _ ) ->
            ( model, Cmd.none )

        -- Handle page-specifc messages
        ( _, Editor Nothing _ ) ->
            -- Ignore messages received while loading
            -- TODO: queue them instead?
            ( model, Cmd.none )

        ( EditorMsg editorMsg, Editor (Just data) editorModel ) ->
            let
                ( data_, editorModel_, cmd ) =
                    updateEditor editorMsg data editorModel
            in
            ( { model | page = Editor (Just data_) editorModel_ }
            , Cmd.map EditorMsg cmd
            )

        ( GameMsg gameMsg, Game gameModel ) ->
            let
                ( gameModel_, gameCmd ) =
                    updateGame gameMsg gameModel
            in
            ( { model | page = Game gameModel_ }
            , Cmd.map GameMsg gameCmd
            )


updateEditor : EditorMsg -> Data -> EditorModel -> ( Data, EditorModel, Cmd EditorMsg )
updateEditor msg data model =
    case ( msg, model ) of
        ( FileSelect, _ ) ->
            ( data
            , model
            , File.Select.file [ "application/json" ] FileSelected
            )

        ( FileSelected file, _ ) ->
            ( data
            , model
            , Task.perform ReadFile <| File.toString file
            )

        ( UpdatePerson id person, _ ) ->
            ( Dict.update id (always person) data
            , model
            , Lamdera.sendToBackend <| TBUpdatePerson id person
            )

        ( ReadFile str, _ ) ->
            case Codec.decodeString Codecs.dataCodec str of
                Err err ->
                    ( data
                    , { lastError = Json.Decode.errorToString err
                      , currentPerson = Nothing
                      }
                    , Cmd.none
                    )

                Ok newData ->
                    ( newData
                    , { lastError = ""
                      , currentPerson = Nothing
                      }
                    , Lamdera.sendToBackend <| TBData newData
                    )

        ( DownloadJson, _ ) ->
            ( data
            , model
            , File.Download.string "sdc-game.json" "application/json" <|
                Codec.encodeToString 0 Codecs.dataCodec data
            )

        ( AddPerson, _ ) ->
            ( data
            , model
            , Random.int 0 Random.maxInt
                |> Random.map Hex.toString
                |> Random.generate (\newId -> UpdatePerson newId (Just Editors.personDefault))
            )

        ( EditPerson id, editorModel ) ->
            ( data
            , { editorModel | currentPerson = Just id }
            , Cmd.none
            )


updateGame : GameMsg -> OuterGameModel -> ( OuterGameModel, Cmd GameMsg )
updateGame msg outerModel =
    case outerModel of
        LoadingData ->
            -- Ignore messages received while loading
            -- TODO: queue them instead?
            ( outerModel, Cmd.none )

        DataEmpty ->
            ( outerModel, Cmd.none )

        LoadedData data sharedModel model ->
            let
                ( sharedModel_, model_, cmd ) =
                    case msg of
                        ViewPerson id ->
                            ( { sharedModel | currentPerson = id }, ViewingPerson, Cmd.none )

                        ViewMap ->
                            ( sharedModel, ViewingMap, Cmd.none )

                        ViewDialog dialog ->
                            ( sharedModel, Talking { currentDialog = dialog }, Cmd.none )

                        ViewQuiz ->
                            case Dict.get sharedModel.currentPerson data of
                                Nothing ->
                                    ( sharedModel, model, Cmd.none )

                                Just person ->
                                    ( sharedModel, model, Debug.todo "Pick a quiz" )
            in
            ( LoadedData data sharedModel_ model_, cmd )


view : FrontendModel -> Element FrontendMsg
view model =
    case ( model.page, model.size ) of
        ( Game _, Nothing ) ->
            Frontend.Common.loading

        ( Game gameModel, Just size ) ->
            Element.map GameMsg <| Element.WithUnits.run size <| Frontend.Game.view gameModel

        ( Editor data editorModel, _ ) ->
            Element.map EditorMsg <| Frontend.Editor.view data editorModel
