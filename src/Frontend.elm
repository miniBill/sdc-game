module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Codec exposing (Codec)
import Codecs exposing (gameModelCodec, sharedGameModelCodec)
import Dict
import Editors
import Element.WithContext as Element exposing (fill, height, width)
import File
import File.Download
import File.Select
import Frontend.Common
import Frontend.Editor
import Frontend.Game
import Frontend.Theme exposing (Element)
import Hex
import Html
import Html.Attributes
import Json.Decode
import Lamdera exposing (Key, Url)
import List.Extra
import Model exposing (City, Data, GameModel(..), Id, Nation(..), Person, SharedGameModel)
import Pixels
import PkgPorts
import Random
import Set
import Task
import Types exposing (EditorModel, EditorMsg(..), FrontendModel, FrontendMsg(..), GameMsg(..), OuterGameModel(..), Page(..), ToBackend(..), ToFrontend(..))
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
        , view = outerView
        , update = update
        , subscriptions = subscriptions
        , updateFromBackend = updateFromBackend
        }


outerView : FrontendModel -> { title : String, body : List (Html.Html FrontendMsg) }
outerView model =
    let
        attrs =
            [ Frontend.Theme.fontSizes.normal
            , height fill
            , width fill
            , Element.htmlAttribute <| Html.Attributes.id "main"
            ]
    in
    { title = "SDC Game"
    , body =
        [ css
        , case model.size of
            Nothing ->
                Element.layout () attrs Frontend.Common.loading

            Just size ->
                Element.layout { screenSize = size } attrs (view model)
        ]
    }


css : Html.Html FrontendMsg
css =
    let
        content =
            """
            select {
                font-size: """ ++ String.fromInt Frontend.Theme.fontSize ++ """px;
            }
            """
    in
    Html.node "style" [] [ Html.text content ]


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        TFUpdatePerson id person ->
            ( updatePersonInModel id person model
            , getSizeCmd
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
            , Cmd.batch
                [ getSizeCmd
                , PkgPorts.localstorage_load {}
                ]
            )


gotGameData : Data -> OuterGameModel
gotGameData data =
    case
        ( findOrla data
        , Dict.get "" data
        )
    of
        ( Just ( orlaId, _ ), Just initial ) ->
            (let
                _ =
                    Debug.todo
             in
             \_ q ->
                q
            )
                (LoadedData data
                    { currentPerson = orlaId
                    , tickets = Set.singleton orlaId
                    }
                    (ViewingMap {})
                )
                (LoadedData
                    data
                    { currentPerson = ""
                    , tickets = Set.singleton orlaId
                    }
                    (ViewingTalking { chatHistory = [], currentDialog = initial.dialog })
                )

        ( Nothing, _ ) ->
            DataEmpty

        ( _, Nothing ) ->
            DataEmpty


findOrla : Data -> Maybe ( Id, Person )
findOrla data =
    data
        |> Dict.toList
        |> List.Extra.find (\( id, p ) -> id /= "" && p.name == "Orla")


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
    Sub.batch
        [ Browser.Events.onResize (\_ _ -> GotResized)
        , PkgPorts.localstorage_loaded (GameMsg << LSLoaded)
        ]


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case ( msg, model.page ) of
        -- Handle generic messages
        ( UrlClicked urlRequest, _ ) ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                External url ->
                    ( model, Nav.load url )

        ( UrlChanged url, _ ) ->
            ( { model | page = urlToPage url }, Cmd.none )

        ( Resized width height, _ ) ->
            ( { model
                | size =
                    { width = width
                    , height = height
                    }
                        |> Just
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
            , Cmd.batch [ getSizeCmd, Cmd.map EditorMsg cmd ]
            )

        ( GameMsg gameMsg, Game gameModel ) ->
            let
                ( gameModel_, gameCmd ) =
                    updateGame gameMsg gameModel
            in
            ( { model | page = Game gameModel_ }
            , Cmd.batch [ getSizeCmd, Cmd.map GameMsg gameCmd ]
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

                        ViewMenu { background } ->
                            ( sharedModel
                            , ViewingMenu
                                { previous = model
                                , background = background
                                }
                            , Cmd.none
                            )

                        ViewMap ->
                            ( { sharedModel
                                | currentPerson =
                                    if String.isEmpty sharedModel.currentPerson then
                                        findOrla data
                                            |> Maybe.map Tuple.first
                                            |> Maybe.withDefault ""

                                    else
                                        sharedModel.currentPerson
                              }
                            , ViewingMap {}
                            , Cmd.none
                            )

                        ViewTalking dialog chatHistory ->
                            ( sharedModel
                            , ViewingTalking
                                { chatHistory = chatHistory
                                , currentDialog = dialog
                                }
                            , Cmd.none
                            )

                        PickQuiz ->
                            case Dict.get sharedModel.currentPerson data of
                                Nothing ->
                                    ( sharedModel, model, Cmd.none )

                                Just person ->
                                    ( sharedModel
                                    , model
                                    , case person.quizzes of
                                        [] ->
                                            Cmd.none

                                        h :: t ->
                                            Random.uniform h t
                                                |> Random.generate ViewQuiz
                                    )

                        ViewQuiz quiz ->
                            ( sharedModel, Quizzing quiz, Cmd.none )

                        GiveTicketAndViewMap ->
                            ( { sharedModel
                                | currentPerson =
                                    if String.isEmpty sharedModel.currentPerson then
                                        findOrla data
                                            |> Maybe.map Tuple.first
                                            |> Maybe.withDefault ""

                                    else
                                        sharedModel.currentPerson
                              }
                            , ViewingMap {}
                            , pickNewTicket data sharedModel
                            )

                        GotRandomTicket id ->
                            ( { sharedModel | tickets = Set.insert id sharedModel.tickets }, model, Cmd.none )

                        BackTo previous ->
                            ( sharedModel, previous, Cmd.none )

                        Reset ->
                            case gotGameData data of
                                LoadedData _ s m ->
                                    ( s, m, Cmd.none )

                                _ ->
                                    ( sharedModel, model, Cmd.none )

                        LSLoaded localStorage ->
                            localStorage
                                |> Codec.decodeString localStorageCodec
                                |> Result.withDefault ( sharedModel, model )
                                |> (\( s, m ) -> ( s, m, Cmd.none ))
            in
            ( LoadedData data sharedModel_ model_
            , Cmd.batch
                [ cmd
                , PkgPorts.localstorage_store <|
                    Codec.encodeToString 0
                        localStorageCodec
                        ( sharedModel_, model_ )
                ]
            )


localStorageCodec : Codec ( SharedGameModel, GameModel )
localStorageCodec =
    Codec.tuple sharedGameModelCodec gameModelCodec


type Region
    = EnglandRegion
    | EuropeRegion
    | NetherlandsRegion
    | Nijmegen


pickNewTicket : Data -> SharedGameModel -> Cmd GameMsg
pickNewTicket data model =
    let
        { ownedEngland, missingEngland, ownedEurope, missingEurope, ownedNetherlands, missingNetherlands, ownedNijmegen, missingNijmegen } =
            data
                |> Dict.toList
                |> List.foldr
                    (\( id, { name, city } ) acc ->
                        if String.isEmpty id || name == "Orla" then
                            -- Orla doesn't count here
                            acc

                        else
                            let
                                region =
                                    toRegion city
                            in
                            if Set.member id model.tickets then
                                case region of
                                    EnglandRegion ->
                                        { acc | ownedEngland = id :: acc.ownedEngland }

                                    EuropeRegion ->
                                        { acc | ownedEurope = id :: acc.ownedEurope }

                                    NetherlandsRegion ->
                                        { acc | ownedNetherlands = id :: acc.ownedNetherlands }

                                    Nijmegen ->
                                        { acc | ownedNijmegen = id :: acc.ownedNijmegen }

                            else
                                case region of
                                    EnglandRegion ->
                                        { acc | missingEngland = id :: acc.missingEngland }

                                    EuropeRegion ->
                                        { acc | missingEurope = id :: acc.missingEurope }

                                    NetherlandsRegion ->
                                        { acc | missingNetherlands = id :: acc.missingNetherlands }

                                    Nijmegen ->
                                        { acc | missingNijmegen = id :: acc.missingNijmegen }
                    )
                    { ownedEngland = []
                    , ownedEurope = []
                    , ownedNetherlands = []
                    , missingEngland = []
                    , missingEurope = []
                    , missingNetherlands = []
                    , ownedNijmegen = []
                    , missingNijmegen = []
                    }

        candidates =
            if List.length ownedEngland < List.length missingEngland then
                missingEngland

            else if List.length ownedEurope < List.length missingEurope then
                missingEurope

            else if List.length ownedNetherlands < List.length missingNetherlands then
                missingNetherlands

            else if List.length ownedNijmegen < List.length missingNijmegen then
                missingNijmegen

            else
                missingEngland ++ missingEurope ++ missingNetherlands
    in
    case candidates of
        [] ->
            -- This means you've visited everyone. Nothing to do.
            Cmd.none

        h :: t ->
            Random.uniform h t
                |> Random.generate GotRandomTicket


toRegion : City -> Region
toRegion city =
    if city.name == "Nijmegen" then
        Nijmegen

    else
        case city.nation of
            England ->
                EnglandRegion

            Netherlands ->
                NetherlandsRegion

            _ ->
                EuropeRegion


view : FrontendModel -> Element FrontendMsg
view model =
    case model.page of
        Game gameModel ->
            Element.map GameMsg <| Frontend.Game.view gameModel

        Editor data editorModel ->
            Element.map EditorMsg <| Frontend.Editor.view data editorModel
