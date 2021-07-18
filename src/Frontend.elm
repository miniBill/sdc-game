module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Codec
import Dict
import Element exposing (Attribute, Element, Length, alignTop, behindContent, centerX, centerY, column, el, fill, height, link, none, padding, row, shrink, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File
import File.Download
import File.Select
import Generator
import Html
import Html.Attributes
import Http
import Lamdera exposing (Key, Url)
import List.Extra as List
import Model exposing (Tree(..), dfsSort, emptyScene, replaceScene)
import Task
import Theme exposing (input, multiline, rythm, select)
import Types exposing (Data, FrontendModel, FrontendMsg(..), ToBackend(..), ToFrontend(..))
import Url


type alias Model =
    FrontendModel


type alias Msg =
    FrontendMsg


fontSize : number
fontSize =
    16


app :
    { init : Url -> Key -> ( Model, Cmd Msg )
    , view : Model -> Browser.Document Msg
    , update : Msg -> Model -> ( Model, Cmd Msg )
    , updateFromBackend : ToFrontend -> Model -> ( Model, Cmd Msg )
    , subscriptions : Model -> Sub Msg
    , onUrlRequest : UrlRequest -> Msg
    , onUrlChange : Url -> Msg
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
                    , Element.layout [ Font.size fontSize ] <| view model
                    ]
                }
        , update = update
        , subscriptions = subscriptions
        , updateFromBackend = updateFromBackend
        }


css : Html.Html Msg
css =
    let
        content =
            """
            select {
                font-size: """ ++ String.fromInt fontSize ++ """px;
            }
            """
    in
    Html.node "style"
        []
        [ Html.text content
        ]


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd Msg )
updateFromBackend msg model =
    case msg of
        TFReplace oldKey ( newKey, newValue ) ->
            ( { model | data = Maybe.map (replaceScene oldKey newKey newValue) model.data }, Cmd.none )

        TFData data ->
            ( { model | data = Just data }, Cmd.none )


init : Url -> Key -> ( Model, Cmd Msg )
init _ key =
    ( { key = key
      , data = Nothing
      , images = []
      }
    , Http.get
        { url = "art/list.json"
        , expect =
            Http.expectJson GotImageList
                (Codec.decoder <| Codec.list Codec.string)
        }
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.data ) of
        ( GotImageList (Err err), _ ) ->
            let
                _ =
                    log "Error in reading image list" err
            in
            ( model, Cmd.none )

        ( GotImageList (Ok images), _ ) ->
            ( { model | images = images }, Cmd.none )

        ( _, Nothing ) ->
            ( model, Cmd.none )

        ( UrlClicked urlRequest, _ ) ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch [ Nav.pushUrl model.key (Url.toString url) ]
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        ( UrlChanged _, _ ) ->
            ( model, Cmd.none )

        ( FileSelect, _ ) ->
            ( model, File.Select.file [ "application/json" ] FileSelected )

        ( Replace oldKey ( newKey, newValue ), Just data ) ->
            ( { model | data = Just <| replaceScene oldKey newKey newValue data }
            , Lamdera.sendToBackend <| TBReplace oldKey ( newKey, newValue )
            )

        ( ReplaceNext sceneName index newNext, Just data ) ->
            case Dict.get sceneName data of
                Nothing ->
                    ( model, Cmd.none )

                Just scene ->
                    let
                        newValue =
                            { scene
                                | next =
                                    case index of
                                        Just i ->
                                            List.setAt i newNext scene.next

                                        Nothing ->
                                            scene.next ++ [ newNext ]
                            }
                    in
                    ( { model | data = Just <| replaceScene sceneName sceneName newValue data }
                    , Lamdera.sendToBackend <| TBReplace sceneName ( sceneName, newValue )
                    )

        ( FileSelected file, _ ) ->
            ( model, Task.perform ReadFile <| File.toString file )

        ( ReadFile str, _ ) ->
            case Codec.decodeString Model.dataCodec str of
                Err err ->
                    let
                        _ =
                            log "Error in reading file" err
                    in
                    ( model, Cmd.none )

                Ok newData ->
                    ( { model | data = Just newData }, Lamdera.sendToBackend <| TBData newData )

        ( GenerateC, Just data ) ->
            ( model
            , File.Download.string "logic.c" "text/x-c" <|
                Generator.generate data
            )

        ( DownloadJson, Just data ) ->
            ( model
            , File.Download.string "data.json" "application/json" <|
                Codec.encodeToString 0 Model.dataCodec data
            )


log : String -> a -> a
log =
    if True then
        Debug.log

    else
        always identity


view : Model -> Element Msg
view model =
    case model.data of
        Nothing ->
            el [ Font.size 40, centerX, centerY, Font.center ] <| text "Loading..."

        Just data ->
            let
                scenes =
                    dfsSort "main" data ++ [ Node "" emptyScene [] ]

                sceneViews =
                    List.map
                        (viewScene data model.images)
                        scenes
            in
            column [ width fill, spacing rythm, padding rythm ]
                [ fileControls
                , wrappedRow [ spacing rythm ] sceneViews
                ]


fileControls : Element Msg
fileControls =
    row [ spacing rythm ]
        [ Input.button [ Border.width 1, padding rythm ]
            { onPress = Just FileSelect
            , label = text "Upload JSON"
            }
        , Input.button [ Border.width 1, padding rythm ]
            { onPress = Just DownloadJson
            , label = text "Save as JSON"
            }
        , Input.button [ Border.width 1, padding rythm ]
            { onPress = Just GenerateC
            , label = text "Generate C"
            }
        ]


style : String -> String -> Attribute msg
style k v =
    Element.htmlAttribute <| Html.Attributes.style k v


widthWithMinimum : Length -> Attribute msg
widthWithMinimum =
    width << Element.minimum 240


viewScene : Data -> List String -> Tree -> Element Msg
viewScene data images (Node name scene children) =
    let
        viewNext_ i d =
            row segmentAttrs <| viewNext name data i d

        nexts =
            List.indexedMap (Just >> viewNext_) scene.next
                ++ (if
                        List.isEmpty scene.next
                            || List.any (not << String.isEmpty << Tuple.first) scene.next
                    then
                        [ viewNext_ Nothing ( "", "" ) ]

                    else
                        []
                   )

        elems =
            if String.isEmpty name && scene == emptyScene then
                [ el segmentAttrs <|
                    input
                        [ alignTop
                        , width fill
                        ]
                        { label = "Name"
                        , text = name
                        , onChange = \newName -> Replace name ( newName, scene )
                        }
                ]

            else
                fixed :: nexts

        fixed =
            column segmentAttrs
                [ row [ spacing rythm, width fill ]
                    [ input [ alignTop, width fill ]
                        { label = "Name"
                        , text = name
                        , onChange = \newName -> Replace name ( newName, scene )
                        }
                    , if List.isEmpty images then
                        input [ alignTop, widthWithMinimum shrink ]
                            { label = "Image"
                            , text = scene.image
                            , onChange = \newImage -> Replace name ( name, { scene | image = newImage } )
                            }

                      else
                        select []
                            { onInput = \newImage -> Replace name ( name, { scene | image = newImage } )
                            , selected = scene.image
                            , options = "" :: List.map (String.replace ".png" "") images
                            }
                    ]
                , multiline
                    [ alignTop
                    , height fill
                    , widthWithMinimum fill
                    ]
                    { label = "Text"
                    , text = scene.text
                    , onChange = \newText -> Replace name ( name, { scene | text = newText } )
                    }
                ]

        segmentAttrs =
            [ spacing rythm
            , padding rythm
            , Border.width 1
            , width fill
            , height fill
            ]

        backgroundImage =
            if String.isEmpty scene.image then
                none

            else
                el
                    [ style "background-image" <| "url(art/" ++ scene.image ++ ".png)"
                    , style "background-repeat" "no-repeat"
                    , style "background-size" "contain"
                    , style "background-position" "right"
                    , width fill
                    , height fill
                    ]
                    none
    in
    column [ spacing rythm, alignTop ]
        [ column
            [ Element.htmlAttribute <| Html.Attributes.id name
            , Border.width 1
            , width <| Element.minimum 510 fill
            , behindContent backgroundImage
            , alignTop
            ]
            elems
        , row [ spacing rythm ]
            (List.map (viewScene data images) children)
        ]


viewNext : String -> Data -> Maybe Int -> ( String, String ) -> List (Element Msg)
viewNext sceneName data i ( k, v ) =
    [ multiline
        [ alignTop
        , height fill
        , widthWithMinimum fill
        ]
        { label = "Label"
        , text = k
        , onChange = \newValue -> ReplaceNext sceneName i ( newValue, v )
        }
    , select []
        { onInput = \newValue -> ReplaceNext sceneName i ( k, newValue )
        , selected = v
        , options = "" :: Dict.keys data ++ [ "end" ]
        }
    , link
        [ Border.width 1
        , padding rythm
        , Background.color Theme.colors.semitransparent
        ]
        { label = text "Scroll to"
        , url = "#" ++ v
        }
    ]
