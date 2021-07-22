module Frontend exposing (app)

import Base64
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Bytes exposing (Bytes)
import Codec
import Dict exposing (Dict)
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
    Html.node "style" [] [ Html.text content ]


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd Msg )
updateFromBackend msg model =
    case msg of
        TFReplace oldKey ( newKey, newValue ) ->
            ( { model | data = Maybe.map (replaceScene oldKey newKey newValue) model.data }, Cmd.none )

        TFData data ->
            ( { model | data = Just data }, Cmd.none )

        TFGotImageList images ->
            ( { model | images = images }, Cmd.none )

        TFImage name image ->
            ( { model | images = Dict.insert name image model.images }, Cmd.none )


init : Url -> Key -> ( Model, Cmd Msg )
init _ key =
    ( { key = key
      , data = Nothing
      , images = Dict.empty
      }
    , Lamdera.sendToBackend TBGetImageList
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.data ) of
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

        ( FileSelected file, _ ) ->
            ( model, Task.perform ReadFile <| File.toString file )

        ( ImageSelect, _ ) ->
            ( model, File.Select.file [ "image/png" ] ImageSelected )

        ( ImageSelected file, _ ) ->
            ( model, Task.perform (ReadImage <| File.name file) <| File.toBytes file )

        ( ReadImage filename image, _ ) ->
            let
                name =
                    String.replace ".png" "" filename
            in
            ( { model | images = Dict.insert name image model.images }
            , Lamdera.sendToBackend <| TBImage name image
            )

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
            { onPress = Just ImageSelect
            , label = text "Upload Image"
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


viewScene : Data -> Dict String Bytes -> Tree -> Element Msg
viewScene data images (Node name scene children) =
    let
        keys =
            Dict.keys data

        viewNext_ i d =
            row segmentAttrs <| viewNext { keys = keys, toMsg = ReplaceNext name i } d

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

        nameInput =
            input [ alignTop, width fill ]
                { label = "Name"
                , text = name
                , onChange = \newName -> Replace name ( newName, scene )
                }

        elems =
            if String.isEmpty name && scene == emptyScene then
                [ column segmentAttrs
                    [ row [ spacing rythm, width fill ]
                        [ nameInput ]
                    ]
                ]

            else
                fixed :: nexts

        fixed =
            column segmentAttrs
                [ row [ spacing rythm, width fill ]
                    [ nameInput
                    , if Dict.isEmpty images then
                        input [ width fill ]
                            { label = "Image"
                            , text = scene.image
                            , onChange = \newImage -> Replace name ( name, { scene | image = newImage } )
                            }

                      else
                        select [ width fill ]
                            { onInput = \newImage -> Replace name ( name, { scene | image = newImage } )
                            , selected = scene.image
                            , options = "" :: Dict.keys images
                            }
                    ]
                , multiline
                    [ alignTop
                    , height fill
                    , width fill
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
            case Dict.get scene.image images of
                Nothing ->
                    none

                Just img ->
                    el
                        [ style "background-image" <|
                            "url(\""
                                ++ (img
                                        |> Base64.fromBytes
                                        |> Maybe.withDefault ""
                                        |> (++) "data:image/png;base64,"
                                   )
                                ++ "\")"
                        , style "background-repeat" "no-repeat"
                        , style "background-size" "contain"
                        , style "background-position" "right"
                        , style "image-rendering" "pixelated"
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
            , Background.color <| Element.rgba 0.2 0.2 0.2 0.2
            ]
            elems
        , row [ spacing rythm ]
            (List.map (viewScene data images) children)
        ]


viewNext : { keys : List String, toMsg : ( String, String ) -> msg } -> ( String, String ) -> List (Element msg)
viewNext { keys, toMsg } ( k, v ) =
    [ multiline
        [ alignTop
        , height fill
        , width fill
        ]
        { label = "Label"
        , text = k
        , onChange = \newValue -> toMsg ( newValue, v )
        }
    , select []
        { onInput = \newValue -> toMsg ( k, newValue )
        , selected = v
        , options = "" :: keys ++ [ "end" ]
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
