module Main exposing (main)

import Bare
import Base64
import Browser
import Codec
import Codec.Bare
import Dict
import Element exposing (Attribute, Element, alignLeft, alignRight, alignTop, column, el, fill, height, image, newTabLink, none, padding, px, row, spacing, text, width, wrappedRow)
import Element.Border as Border
import Element.Input as Input
import File exposing (File)
import File.Select
import Html
import Html.Attributes
import Html.Events
import Json exposing (Data, Scene)
import List.Extra as List
import Task


type alias Model =
    Data


type Msg
    = FileSelect
    | FileSelected File
    | ReadFile String
    | Replace Data


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = Element.layout [] << view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Dict.empty, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileSelect ->
            ( model, File.Select.file [ "application/json" ] FileSelected )

        Replace newModel ->
            ( clean newModel, Cmd.none )

        FileSelected file ->
            ( model, Task.perform ReadFile <| File.toString file )

        ReadFile str ->
            case Codec.decodeString Json.dataCodec str of
                Err err ->
                    let
                        _ =
                            Debug.log "Err" err
                    in
                    ( model, Cmd.none )

                Ok newModel ->
                    ( newModel, Cmd.none )


clean : Data -> Data
clean =
    Dict.filter (\k v -> not (String.isEmpty k) || not (v == emptyScene))
        >> Dict.map (always cleanNext)


cleanNext : Scene -> Scene
cleanNext scene =
    { scene
        | next =
            scene.next
                |> List.filter
                    (\( k, v ) -> not (String.isEmpty k) || not (String.isEmpty v))
    }


view : Model -> Element Msg
view model =
    let
        fileControls =
            row [ spacing rythm ]
                [ Input.button [ Border.width 1, padding rythm ]
                    { onPress = Just FileSelect
                    , label = text "Upload JSON"
                    }
                , newTabLink [ Element.htmlAttribute <| Html.Attributes.download "data.json" ]
                    { label = text "Download as JSON"
                    , url = "data:application/json;base64," ++ jsonVersion
                    }
                , newTabLink [ Element.htmlAttribute <| Html.Attributes.download "data.bare" ]
                    { label = text "Download as BARE"
                    , url = "data:application/binary;base64," ++ bareVersion
                    }
                ]

        scenes =
            model
                |> dfsSort "main"
                |> (\l -> l ++ [ ( "", emptyScene ) ])
                |> List.map
                    (\( k, v ) ->
                        Element.map
                            (\( k_, v_ ) ->
                                model
                                    |> Dict.remove k
                                    |> Dict.insert k_ v_
                                    |> Dict.map
                                        (\_ scene ->
                                            { scene
                                                | next =
                                                    if k == "" then
                                                        scene.next

                                                    else
                                                        scene.next
                                                            |> List.map
                                                                (\( kn, vn ) ->
                                                                    if vn == k then
                                                                        ( kn, k_ )

                                                                    else
                                                                        ( kn, vn )
                                                                )
                                            }
                                        )
                                    |> Replace
                            )
                            (viewScene model k v)
                    )

        bareVersion =
            model
                |> Json.toBare
                |> Codec.Bare.encodeToValue Bare.dataCodec
                |> Base64.fromBytes
                |> Maybe.withDefault ""

        jsonVersion =
            model
                |> Codec.encodeToString 0 Json.dataCodec
                |> Base64.fromString
                |> Maybe.withDefault ""
    in
    column [ width fill, spacing rythm, padding rythm ] <| fileControls :: scenes


dfsSort : String -> Data -> List ( String, Scene )
dfsSort root scenes =
    case Dict.get root scenes of
        Nothing ->
            []

        Just scene ->
            let
                ( visible, nonvisible ) =
                    List.foldl
                        (\( _, v ) ( res, queue ) ->
                            let
                                found =
                                    dfsSort v queue

                                newQueue =
                                    List.foldl (\( k, _ ) -> Dict.remove k) queue found
                            in
                            ( res ++ found, newQueue )
                        )
                        ( [ ( root, scene ) ], Dict.remove root scenes )
                        scene.next
            in
            if root == "main" then
                visible ++ Dict.toList nonvisible

            else
                visible


rythm : number
rythm =
    10


viewScene : Model -> String -> Scene -> Element ( String, Scene )
viewScene model name scene =
    let
        toOption selected key =
            Html.option
                [ Html.Attributes.value key
                , Html.Attributes.selected <| key == selected
                ]
                [ Html.text key ]

        viewNext i ( k, v ) =
            [ Input.text [ alignTop, width <| Element.minimum 240 fill ]
                { label = Input.labelAbove [] <| text "Label"
                , text = k
                , onChange =
                    \newValue ->
                        if i < 0 then
                            { scene | next = scene.next ++ [ ( newValue, v ) ] }

                        else
                            { scene
                                | next = List.setAt i ( newValue, v ) scene.next
                            }
                , placeholder = Nothing
                }
            , column [ alignTop, spacing (rythm - 4) ]
                [ text "Go to"
                , el [] <|
                    Element.html <|
                        Html.select
                            [ Html.Events.onInput
                                (\newValue ->
                                    if i < 0 then
                                        { scene | next = scene.next ++ [ ( k, newValue ) ] }

                                    else
                                        { scene
                                            | next = List.setAt i ( k, newValue ) scene.next
                                        }
                                )
                            ]
                        <|
                            List.map (toOption v) (Dict.keys model ++ [ "" ])
                ]
            ]
                |> List.map (Element.map (Tuple.pair name))

        elems =
            if String.isEmpty name && scene == emptyScene then
                [ [ input [] "Name" name <| \newName -> ( newName, scene ) ] ]

            else
                fixed
                    :: List.indexedMap viewNext scene.next
                    ++ (if List.isEmpty scene.next || List.any (not << String.isEmpty << Tuple.first) scene.next then
                            [ viewNext -1 ( "", "" ) ]

                        else
                            []
                       )

        maybeImage =
            if String.isEmpty scene.image then
                none

            else
                image []
                    { src = "art/" ++ scene.image ++ ".png"
                    , description = "Image for the scene " ++ name
                    }

        fixed =
            [ input [] "Name" name <| \newName -> ( newName, scene )
            , column [ spacing rythm ]
                [ input [] "Image" scene.image <| \newImage -> ( name, { scene | image = newImage } )
                , maybeImage
                ]
            , multiline [ width <| px 400 ] "Text" scene.text <| \newText -> ( name, { scene | text = newText } )
            ]
    in
    elems
        |> List.indexedMap
            (\i ->
                row
                    [ spacing rythm
                    , padding rythm
                    , Border.width 1
                    , width fill
                    , height fill
                    , if i == 0 then
                        alignLeft

                      else
                        alignRight
                    ]
            )
        |> wrappedRow [ Border.width 1, width fill ]


input : List (Attribute Never) -> String -> String -> (String -> ( String, Scene )) -> Element ( String, Scene )
input attrs label value setter =
    Input.text ([ alignTop, width <| Element.minimum 240 fill ] ++ List.map (Element.mapAttribute never) attrs)
        { label = Input.labelAbove [] <| text label
        , text = value
        , onChange = setter
        , placeholder = Nothing
        }


multiline : List (Attribute Never) -> String -> String -> (String -> ( String, Scene )) -> Element ( String, Scene )
multiline attrs label value setter =
    Input.multiline ([ alignTop, width <| Element.minimum 240 fill, height fill ] ++ List.map (Element.mapAttribute never) attrs)
        { label = Input.labelAbove [] <| text label
        , text = value
        , onChange = setter
        , placeholder = Nothing
        , spellcheck = True
        }


emptyScene : Scene
emptyScene =
    { text = ""
    , image = ""
    , next = []
    }
