module Main exposing (main)

import Bare
import Base64
import Browser
import Codec
import Codec.Bare
import Dict
import Element exposing (Element, alignTop, column, fill, image, newTabLink, none, padding, row, spacing, text, width, wrappedRow)
import Element.Border as Border
import Element.Input as Input
import File exposing (File)
import File.Select
import Html.Attributes
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
view dict =
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
            dict
                |> Dict.toList
                |> List.sortBy
                    (\( name, _ ) ->
                        if name == "main" then
                            ( 0, name )

                        else
                            ( 1, name )
                    )
                |> (\l -> l ++ [ ( "", emptyScene ) ])
                |> List.map
                    (\( k, v ) ->
                        Element.map
                            (\( k_, v_ ) ->
                                dict
                                    |> Dict.remove k
                                    |> Dict.insert k_ v_
                            )
                            (viewScene k v)
                    )
                |> wrappedRow [ spacing rythm ]
                |> Element.map Replace

        bareVersion =
            dict
                |> Json.toBare
                |> Codec.Bare.encodeToValue Bare.dataCodec
                |> Base64.fromBytes
                |> Maybe.withDefault ""

        jsonVersion =
            dict
                |> Codec.encodeToString 0 Json.dataCodec
                |> Base64.fromString
                |> Maybe.withDefault ""
    in
    column [ width fill, spacing rythm, padding rythm ] [ fileControls, scenes ]


rythm : number
rythm =
    10


viewScene : String -> Scene -> Element ( String, Scene )
viewScene name scene =
    let
        viewNext i ( k, v ) =
            [ Input.text [ width fill ]
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
            , Input.text [ width fill ]
                { label = Input.labelAbove [] <| text "Go to"
                , text = v
                , onChange =
                    \newValue ->
                        if i < 0 then
                            { scene | next = scene.next ++ [ ( k, newValue ) ] }

                        else
                            { scene
                                | next = List.setAt i ( k, newValue ) scene.next
                            }
                , placeholder = Nothing
                }
            ]
                |> column
                    [ Border.width 1
                    , padding rythm
                    , spacing rythm
                    , width fill
                    ]
                |> Element.map (Tuple.pair name)

        rows =
            if String.isEmpty name && scene == emptyScene then
                [ input "Name" name <| \newName -> ( newName, scene ) ]

            else
                List.concat
                    [ [ input "Name" name <| \newName -> ( newName, scene )
                      , input "Image" scene.image <| \newImage -> ( name, { scene | image = newImage } )
                      , maybeImage
                      , input "Text" scene.text <| \newText -> ( name, { scene | text = newText } )
                      ]
                    , List.indexedMap viewNext scene.next
                    , if List.isEmpty scene.next || List.any (not << String.isEmpty << Tuple.first) scene.next then
                        [ viewNext -1 ( "", "" ) ]

                      else
                        []
                    ]

        maybeImage =
            if String.isEmpty scene.image then
                none

            else
                image []
                    { src = "art/" ++ scene.image ++ ".png"
                    , description = "Image for the scene " ++ name
                    }
    in
    column
        [ Border.width 2
        , padding rythm
        , spacing rythm
        , alignTop
        , width <| Element.minimum 300 fill
        ]
        rows


input : String -> String -> (String -> ( String, Scene )) -> Element ( String, Scene )
input label value setter =
    Input.text [ width fill ]
        { label = Input.labelAbove [] <| text label
        , text = value
        , onChange = setter
        , placeholder = Nothing
        }


emptyScene : Scene
emptyScene =
    { text = ""
    , image = ""
    , next = []
    }
