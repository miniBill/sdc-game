module Frontend exposing (app)

import Audio exposing (Audio, AudioCmd, AudioData)
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Codec exposing (Codec)
import Codecs exposing (a11yOptionsCodec, gameModelCodec, sharedGameModelCodec)
import Dict
import Duration
import Editors
import Element.WithContext as Element exposing (fill, height, width)
import Element.WithContext.Font as Font
import Env
import File
import File.Download
import File.Select
import Frontend.Common
import Frontend.Editor
import Frontend.EditorTheme exposing (Element)
import Frontend.Game
import Frontend.GameTheme
import Hex
import Html
import Html.Attributes
import Json.Decode
import Lamdera exposing (Key, Url)
import List.Extra
import Model exposing (City, Data, GameModel(..), Id, Nation(..), Person, SharedGameModel, Sound)
import Pixels
import PkgPorts
import Process
import Quantity
import Random
import Set
import SoundLibrary
import Task
import Time
import Types exposing (A11yOptions, AudioMsg(..), EditorModel, EditorMsg(..), FrontendModel, FrontendMsg, GameMsg(..), GameMsgTuple, InnerFrontendModel, InnerFrontendMsg(..), OuterGameModel(..), Page(..), ToBackend(..), ToFrontend(..), Track, TrackKind(..))
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
    Audio.lamderaFrontendWithAudio
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , view = outerView
        , update = update
        , subscriptions = subscriptions
        , updateFromBackend = updateFromBackend
        , audioPort =
            { toJS = PkgPorts.audioPortToJS
            , fromJS = PkgPorts.audioPortFromJS
            }
        , audio = audioView
        }


audioView : AudioData -> InnerFrontendModel -> Audio
audioView _ { audio } =
    audio.playing
        |> List.map
            (\{ from, sound, fadingOutFrom, loop, kind } ->
                case Dict.get sound.name audio.sources of
                    Nothing ->
                        Audio.silence

                    Just source ->
                        let
                            raw =
                                if loop then
                                    Audio.audioWithConfig
                                        { loop =
                                            Just
                                                { loopStart = Quantity.zero
                                                , loopEnd = Duration.milliseconds <| toFloat sound.duration
                                                }
                                        , playbackRate = 1
                                        , startAt = Quantity.zero
                                        }
                                        source
                                        from

                                else
                                    Audio.audio source from

                            volume =
                                case kind of
                                    Music ->
                                        audio.mainVolume * audio.musicVolume * 0.5

                                    Effect ->
                                        audio.mainVolume * audio.effectsVolume
                        in
                        case fadingOutFrom of
                            Nothing ->
                                Audio.scaleVolume volume raw

                            Just fadingTime ->
                                Audio.scaleVolumeAt
                                    [ ( fadingTime, volume )
                                    , ( Time.millisToPosix <|
                                            Time.posixToMillis fadingTime
                                                + Frontend.GameTheme.fadeOutTime
                                      , 0
                                      )
                                    ]
                                    raw
            )
        |> Audio.group


outerView : AudioData -> InnerFrontendModel -> { title : String, body : List (Html.Html InnerFrontendMsg) }
outerView _ model =
    let
        attrs =
            [ height fill
            , width fill
            , Element.htmlAttribute <| Html.Attributes.id "main"
            , Element.withAttribute .a11y <| \{ openDyslexic } ->
            if openDyslexic then
                Font.family [ Font.typeface "OpenDyslexic", Font.serif ]

            else
                Font.family [ Font.typeface "ComicSansMS3", Font.serif ]
            ]
    in
    { title = "SDC Game"
    , body =
        [ Html.node "link" [ Html.Attributes.rel "preconnect", Html.Attributes.href Env.filesBaseUrl ] []
        , css
        , case model.screenSize of
            Nothing ->
                Element.layout { a11y = model.a11y } attrs Frontend.Common.loading

            Just size ->
                Element.layout { screenSize = size, a11y = model.a11y } attrs (view model)
        ]
    }


css : Html.Html InnerFrontendMsg
css =
    let
        content =
            """
            @font-face {
                font-family: OpenDyslexic;
                src: local('OpenDyslexic'), local('Open Dyslexic'), url('""" ++ Env.imageToUrl "OpenDyslexic3-Regular.ttf" ++ """');
                font-display: swap;
            }

            @font-face {
                font-family: ComicSansMS3;
                src: local('Comic Sans MS'), local('Comic Sans'), url('""" ++ Env.imageToUrl "ComicSansMS3.ttf" ++ """');
                font-display: swap;
            }"""
    in
    Html.node "style" [] [ Html.text content ]


updateFromBackend : AudioData -> ToFrontend -> InnerFrontendModel -> ( InnerFrontendModel, Cmd InnerFrontendMsg, AudioCmd InnerFrontendMsg )
updateFromBackend _ msg model =
    case msg of
        TFUpdatePerson id person ->
            ( updatePersonInModel id person model
            , getSizeCmd
            , Audio.cmdNone
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
            , Audio.cmdNone
            )


gotGameData : Data -> OuterGameModel
gotGameData data =
    case
        ( findOrla data
        , Dict.get "" data
        )
    of
        ( Just ( orlaId, _ ), Just initial ) ->
            LoadedData
                data
                { currentPerson = ""
                , tickets = Set.singleton orlaId
                , usedTickets = Set.empty
                }
                (ViewingTalking { chatHistory = [], currentDialog = initial.dialog })

        ( Nothing, _ ) ->
            DataEmpty

        ( _, Nothing ) ->
            DataEmpty


findOrla : Data -> Maybe ( Id, Person )
findOrla data =
    data
        |> Dict.toList
        |> List.Extra.find (\( id, p ) -> id /= "" && p.name == "Orla")


updatePersonInModel : Id -> Maybe Person -> InnerFrontendModel -> InnerFrontendModel
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
    InnerFrontendModel
    -> (Maybe Data -> EditorModel -> ( Maybe Data, EditorModel ))
    -> (OuterGameModel -> OuterGameModel)
    -> InnerFrontendModel
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


init : Url -> Key -> ( InnerFrontendModel, Cmd InnerFrontendMsg, AudioCmd InnerFrontendMsg )
init url key =
    ( { key = key
      , page = urlToPage url
      , screenSize = Nothing
      , audio =
            { sources = Dict.empty
            , mainVolume = 1
            , musicVolume = 1
            , effectsVolume = 1
            , playing = []
            }
      , a11y = defaultA11yOptions
      }
    , getSizeCmd
    , SoundLibrary.all
        |> List.map (\sound -> loadAudio sound)
        |> Audio.cmdBatch
    )


loadAudio : Sound -> AudioCmd InnerFrontendMsg
loadAudio sound =
    let
        toMsg name res =
            case res of
                Ok s ->
                    LoadedAudio name s

                Err _ ->
                    Nop
    in
    Audio.loadAudio (toMsg sound) (Env.imageToUrl sound.name)


defaultA11yOptions : A11yOptions
defaultA11yOptions =
    { fontSize = Frontend.GameTheme.defaultFontSize
    , openDyslexic = False
    , unlockEverything = False
    , opaqueBackgrounds = False
    }


getSizeCmd : Cmd InnerFrontendMsg
getSizeCmd =
    Task.perform
        (\{ viewport } ->
            Resized
                (Pixels.pixels viewport.width)
                (Pixels.pixels viewport.height)
        )
        Browser.Dom.getViewport


subscriptions : AudioData -> InnerFrontendModel -> Sub InnerFrontendMsg
subscriptions _ { page } =
    Sub.batch
        [ Browser.Events.onResize (\_ _ -> GotResized)
        , PkgPorts.localstorage_loaded (\ls -> GameMsg ( LocalStorageLoaded ls, Nothing ))
        , case page of
            Game (LoadedData data { currentPerson } (ViewingMap { travellingTo })) ->
                case travellingTo of
                    Just ( fraction, id ) ->
                        let
                            travelTimeInMilliseconds =
                                case ( Dict.get currentPerson data, Dict.get id data ) of
                                    ( Just from, Just to ) ->
                                        let
                                            fromC =
                                                from.city.coordinates

                                            toC =
                                                to.city.coordinates
                                        in
                                        10 * sqrt ((fromC.x - toC.x) ^ 2 + (fromC.y - toC.y) ^ 2)

                                    _ ->
                                        0
                        in
                        Browser.Events.onAnimationFrameDelta
                            (\d ->
                                GameMsg ( TravellingTo (fraction + d / travelTimeInMilliseconds) id, Nothing )
                            )

                    Nothing ->
                        Sub.none

            _ ->
                Sub.none
        ]


update : AudioData -> InnerFrontendMsg -> InnerFrontendModel -> ( InnerFrontendModel, Cmd InnerFrontendMsg, AudioCmd InnerFrontendMsg )
update _ msg ({ audio } as model) =
    case ( msg, model.page ) of
        -- Handle generic messages
        ( Nop, _ ) ->
            ( model, Cmd.none, Audio.cmdNone )

        ( LoadedAudio sound source, _ ) ->
            ( { model
                | audio =
                    { audio
                        | sources =
                            Dict.insert sound.name source audio.sources
                    }
              }
            , Cmd.none
            , Audio.cmdNone
            )

        ( TimedAudioMsg amsg time, _ ) ->
            ( { model
                | audio =
                    case amsg of
                        AudioMainVolume mainVolume ->
                            { audio | mainVolume = mainVolume }

                        AudioEffectsVolume effectsVolume ->
                            { audio | effectsVolume = effectsVolume }

                        AudioMusicVolume musicVolume ->
                            { audio | musicVolume = musicVolume }

                        AudioPlay sound loop kind ->
                            { audio
                                | playing =
                                    cleanupAudio time <|
                                        { from = time
                                        , sound = sound
                                        , fadingOutFrom = Nothing
                                        , loop = loop
                                        , kind = kind
                                        }
                                            :: audio.playing
                            }

                        AudioStop ->
                            { audio
                                | playing =
                                    audio.playing
                                        |> cleanupAudio time
                                        |> List.map
                                            (\t ->
                                                { t
                                                    | fadingOutFrom =
                                                        t.fadingOutFrom
                                                            |> Maybe.withDefault time
                                                            |> Just
                                                }
                                            )
                            }
              }
            , Cmd.none
            , Audio.cmdNone
            )

        ( UrlClicked urlRequest, _ ) ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url), Audio.cmdNone )

                External url ->
                    ( model, Nav.load url, Audio.cmdNone )

        ( UrlChanged url, _ ) ->
            ( { model | page = urlToPage url }, Cmd.none, Audio.cmdNone )

        ( Resized width height, _ ) ->
            ( { model
                | screenSize =
                    { width = width
                    , height = height
                    }
                        |> Just
              }
            , Cmd.none
            , Audio.cmdNone
            )

        ( GotResized, _ ) ->
            ( model, getSizeCmd, Audio.cmdNone )

        -- Ignore stray cross-page messages
        ( EditorMsg _, Game _ ) ->
            ( model, Cmd.none, Audio.cmdNone )

        ( GameMsg _, Editor _ _ ) ->
            ( model, Cmd.none, Audio.cmdNone )

        -- Handle page-specifc messages
        ( _, Editor Nothing _ ) ->
            -- Ignore messages received while loading
            -- TODO: queue them instead?
            ( model, Cmd.none, Audio.cmdNone )

        ( EditorMsg editorMsg, Editor (Just data) editorModel ) ->
            let
                ( data_, editorModel_, cmd ) =
                    updateEditor editorMsg data editorModel
            in
            ( { model | page = Editor (Just data_) editorModel_ }
            , Cmd.batch [ getSizeCmd, Cmd.map EditorMsg cmd ]
            , Audio.cmdNone
            )

        ( GameMsg gameMsg, Game gameModel ) ->
            let
                ( gameModel_, ( gameCmd, audioCmd ), a11y ) =
                    updateGame gameMsg model.a11y gameModel
            in
            ( { model
                | page = Game gameModel_
                , a11y = a11y
              }
            , Cmd.batch [ getSizeCmd, gameCmd ]
            , audioCmd
            )


cleanupAudio : Time.Posix -> List Track -> List Track
cleanupAudio now =
    List.filter
        (\{ from, sound, loop, fadingOutFrom } ->
            case fadingOutFrom of
                Just fade ->
                    Time.posixToMillis fade + Frontend.GameTheme.fadeOutTime > Time.posixToMillis now

                Nothing ->
                    loop || Time.posixToMillis from + sound.duration > Time.posixToMillis now
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


updateGame :
    GameMsgTuple
    -> A11yOptions
    -> OuterGameModel
    ->
        ( OuterGameModel
        , ( Cmd InnerFrontendMsg
          , AudioCmd InnerFrontendMsg
          )
        , A11yOptions
        )
updateGame msg a11y outerModel =
    case outerModel of
        LoadingData ->
            -- Ignore messages received while loading
            -- TODO: queue them instead?
            ( outerModel, ( Cmd.none, Audio.cmdNone ), a11y )

        DataEmpty ->
            ( outerModel, ( Cmd.none, Audio.cmdNone ), a11y )

        LoadedData data sharedModel model ->
            let
                default =
                    { sharedModel = sharedModel
                    , model = model
                    , cmd = Cmd.none
                    , a11y = a11y
                    , audioCmd = Audio.cmdNone
                    }

                result =
                    case Tuple.first msg of
                        ViewMenu { background } ->
                            { default
                                | model =
                                    ViewingMenu
                                        { previous = model
                                        , background = background
                                        }
                            }

                        ViewMap ->
                            { default
                                | sharedModel =
                                    { sharedModel
                                        | currentPerson =
                                            if String.isEmpty sharedModel.currentPerson then
                                                findOrla data
                                                    |> Maybe.map Tuple.first
                                                    |> Maybe.withDefault ""

                                            else
                                                sharedModel.currentPerson
                                    }
                                , model = ViewingMap { travellingTo = Nothing }
                            }

                        ViewTalking dialog chatHistory ->
                            { default
                                | model =
                                    ViewingTalking
                                        { chatHistory = chatHistory
                                        , currentDialog = dialog
                                        }
                            }

                        PickQuiz ->
                            case Dict.get sharedModel.currentPerson data of
                                Nothing ->
                                    default

                                Just person ->
                                    case person.quizzes of
                                        [] ->
                                            default

                                        h :: t ->
                                            { default
                                                | cmd =
                                                    Random.uniform h t
                                                        |> Random.generate (\quiz -> GameMsg ( ViewQuiz quiz, Nothing ))
                                            }

                        ViewQuiz quiz ->
                            { default | model = Quizzing quiz }

                        GiveTicketAndViewMap ->
                            { default
                                | sharedModel =
                                    { sharedModel
                                        | currentPerson =
                                            if String.isEmpty sharedModel.currentPerson then
                                                findOrla data
                                                    |> Maybe.map Tuple.first
                                                    |> Maybe.withDefault ""

                                            else
                                                sharedModel.currentPerson
                                    }
                                , model = ViewingMap { travellingTo = Nothing }
                                , cmd = Cmd.map (\m -> GameMsg ( m, Nothing )) <| pickNewTicket data sharedModel
                            }

                        GotRandomTicket id ->
                            { default
                                | sharedModel =
                                    { sharedModel | tickets = Set.insert id sharedModel.tickets }
                            }

                        BackTo previous ->
                            { default | model = previous }

                        Reset ->
                            case gotGameData data of
                                LoadedData _ s m ->
                                    { default | sharedModel = s, model = m }

                                _ ->
                                    default

                        A11y a ->
                            { default | a11y = a }

                        LocalStorageLoaded localStorage ->
                            localStorage
                                |> Codec.decodeString localStorageCodec
                                |> Result.withDefault ( sharedModel, model, a11y )
                                |> (\( s, m, a ) ->
                                        { default
                                            | sharedModel = s
                                            , model = m
                                            , a11y = a
                                        }
                                   )

                        TravellingTo fraction id ->
                            let
                                usedTickets =
                                    Set.insert id sharedModel.usedTickets
                            in
                            if fraction >= 1 then
                                { default
                                    | sharedModel =
                                        { sharedModel
                                            | currentPerson = id
                                            , usedTickets = usedTickets
                                        }
                                    , model = ViewingPerson
                                    , cmd =
                                        Cmd.batch
                                            [ Task.perform (TimedAudioMsg AudioStop) Time.now
                                            , case Dict.get id data of
                                                Just { city } ->
                                                    if String.isEmpty city.sound.name then
                                                        Cmd.none

                                                    else
                                                        Task.perform (TimedAudioMsg (AudioPlay city.sound True Music)) <|
                                                            Task.map2 always
                                                                Time.now
                                                                (Process.sleep 1)

                                                Nothing ->
                                                    Cmd.none
                                            ]
                                }

                            else
                                { default
                                    | sharedModel =
                                        { sharedModel
                                            | usedTickets = usedTickets
                                        }
                                    , model =
                                        ViewingMap { travellingTo = Just ( fraction, id ) }
                                    , cmd =
                                        if fraction == 0 then
                                            Cmd.batch
                                                [ Task.perform (TimedAudioMsg AudioStop) Time.now
                                                , Task.perform (TimedAudioMsg (AudioPlay SoundLibrary.train False Effect)) <|
                                                    Task.map2 always
                                                        Time.now
                                                        (Process.sleep 1)
                                                ]

                                        else
                                            Cmd.none
                                    , audioCmd =
                                        if fraction == 0 then
                                            case Dict.get id data of
                                                Just { city } ->
                                                    if String.isEmpty city.sound.name then
                                                        Audio.cmdNone

                                                    else
                                                        loadAudio city.sound

                                                Nothing ->
                                                    Audio.cmdNone

                                        else
                                            Audio.cmdNone
                                }

                        MainVolume mainVolume ->
                            { default | cmd = Task.perform (TimedAudioMsg (AudioMainVolume mainVolume)) Time.now }

                        MusicVolume musicVolume ->
                            { default | cmd = Task.perform (TimedAudioMsg (AudioMusicVolume musicVolume)) Time.now }

                        EffectsVolume effectsVolume ->
                            { default | cmd = Task.perform (TimedAudioMsg (AudioEffectsVolume effectsVolume)) Time.now }
            in
            ( LoadedData data result.sharedModel result.model
            , ( Cmd.batch
                    [ result.cmd
                    , Cmd.map (\r -> GameMsg ( r, Nothing )) <|
                        PkgPorts.localstorage_store <|
                            Codec.encodeToString 0
                                localStorageCodec
                                ( result.sharedModel, result.model, result.a11y )
                    , case Tuple.second msg of
                        Nothing ->
                            Cmd.none

                        Just audioMsg ->
                            Task.perform (TimedAudioMsg audioMsg) Time.now
                    ]
              , result.audioCmd
              )
            , result.a11y
            )


localStorageCodec : Codec ( SharedGameModel, GameModel, A11yOptions )
localStorageCodec =
    Codec.triple sharedGameModelCodec gameModelCodec a11yOptionsCodec


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


view : InnerFrontendModel -> Element InnerFrontendMsg
view model =
    case model.page of
        Game gameModel ->
            Element.map GameMsg <| Frontend.Game.view model.audio gameModel

        Editor data editorModel ->
            Element.map EditorMsg <| Frontend.Editor.view data editorModel
