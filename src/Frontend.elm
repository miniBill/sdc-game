module Frontend exposing (app)

import Base64
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Bytes exposing (Bytes)
import Codec
import Dict exposing (Dict)
import Element exposing (Attribute, Element, alignBottom, alignRight, alignTop, behindContent, centerX, centerY, column, el, fill, height, image, inFront, link, none, padding, paddingEach, px, row, spacing, text, width, wrappedRow)
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
import Types exposing (FrontendModel, FrontendMsg(..), Scene, ToBackend(..), ToFrontend(..))
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

            .preview {
                visibility: hidden;
            }

            .scene:hover + div .preview {
                visibility: visible;
            }

            .pixelated {
                image-rendering: -moz-crisp-edges;
                image-rendering: -webkit-crisp-edges;
                image-rendering: pixelated;
                image-rendering: crisp-edges;
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
      , scale = 2
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

        ( Scale scale, Just _ ) ->
            ( { model | scale = scale }, Cmd.none )


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

                keys =
                    Dict.keys data

                sceneViews =
                    List.map
                        (viewScene model.scale keys model.images)
                        scenes
            in
            column [ width fill, spacing rythm, padding rythm ]
                [ fileControls
                , wrappedRow [ spacing rythm ] sceneViews
                ]


fileControls : Element Msg
fileControls =
    column [ spacing rythm ]
        [ row [ spacing rythm ] <|
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
        , row [ spacing rythm ] <|
            List.map
                (\i ->
                    Input.button [ Border.width 1, padding rythm ]
                        { onPress = Just <| Scale i
                        , label = text <| "Preview scale: " ++ String.fromInt i ++ "x"
                        }
                )
                (List.range 1 5)
        ]


style : String -> String -> Attribute msg
style k v =
    Element.htmlAttribute <| Html.Attributes.style k v


class : String -> Attribute msg
class c =
    Element.htmlAttribute <| Html.Attributes.class c


viewScene : Int -> List String -> Dict String Bytes -> Tree -> Element Msg
viewScene scale keys images (Node name scene children) =
    let
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

        imageUrl =
            Dict.get scene.image images
                |> Maybe.map
                    (\image ->
                        image
                            |> Base64.fromBytes
                            |> Maybe.withDefault ""
                            |> (++) "data:image/png;base64,"
                    )

        rendering =
            Element.onRight <|
                case imageUrl of
                    Nothing ->
                        none

                    Just _ ->
                        el [ Element.moveUp 56 ] <| render scale scene imageUrl
    in
    column [ spacing rythm, alignTop ]
        [ el
            [ width <| Element.minimum 510 fill
            , alignTop
            , rendering
            ]
            (column
                [ Element.htmlAttribute <| Html.Attributes.id name
                , class "scene"
                , Border.width 1
                , width fill
                , behindContent <| pixelatedImage imageUrl
                , Background.color <| Element.rgba 0.2 0.2 0.2 0.2
                ]
                elems
            )
        , row [ spacing rythm ]
            (List.map (viewScene scale keys images) children)
        ]


pixelatedImage : Maybe String -> Element msg
pixelatedImage maybeUrl =
    case maybeUrl of
        Nothing ->
            none

        Just url ->
            el
                [ class "pixelated"
                , style "background-image" <| "url(\"" ++ url ++ "\")"
                , style "background-repeat" "no-repeat"
                , style "background-size" "contain"
                , style "background-position" "right"
                , width fill
                , height fill
                ]
                none


render : Int -> Scene -> Maybe String -> Element msg
render scale scene imageUrl =
    let
        width =
            240

        height =
            160

        ( leftLabel, rightLabel ) =
            case scene.next of
                [ ( "", _ ) ] ->
                    ( "", "A/B: Next" )

                [ ( label, _ ) ] ->
                    ( "", "A/B: " ++ label )

                [ ( l, _ ), ( r, _ ) ] ->
                    ( "B: " ++ l, "A: " ++ r )

                _ ->
                    ( "", "" )

        showText attrs text =
            if String.isEmpty text then
                none

            else
                text
                    |> String.toList
                    |> List.intersperse ' '
                    |> (\s -> ' ' :: s ++ [ ' ' ])
                    |> List.map (Char.toCode >> String.fromInt)
                    |> List.map
                        (\n ->
                            image [ Element.height <| px <| 13 * scale, class "pixelated" ]
                                { src = "font/" ++ n ++ ".png"
                                , description = n
                                }
                        )
                    |> row attrs
    in
    el
        [ paddingEach { left = rythm, top = 0, bottom = 0, right = 0 }
        , class "preview"
        ]
    <|
        image
            [ Element.width <| px <| scale * width
            , Element.height <| px <| scale * height
            , inFront <| showText [ alignBottom ] leftLabel
            , inFront <| showText [ alignBottom, alignRight ] rightLabel
            , inFront <| showText [ centerX ] scene.text
            , class "pixelated"
            ]
            { src = Maybe.withDefault "" imageUrl
            , description = "background"
            }


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
