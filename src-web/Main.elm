module Main exposing (main)

import Browser
import Codec
import Dict
import Element exposing (Attribute, Element, alignLeft, alignRight, alignTop, column, el, fill, height, image, link, newTabLink, none, padding, px, row, spacing, text, width, wrappedRow)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Download
import File.Select
import Generator
import Html
import Html.Attributes
import Html.Events
import List.Extra as List
import Model exposing (Data, Scene, dfsSort)
import Task


type alias Model =
    Data


type Msg
    = FileSelect
    | FileSelected File
    | ReadFile String
    | Replace String ( String, Scene )
    | GenerateC
    | DownloadJson


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

        Replace oldKey ( newKey, newValue ) ->
            ( model
                |> Dict.remove oldKey
                |> Dict.insert newKey newValue
                |> (if oldKey == "" then
                        identity

                    else
                        Dict.map
                            (\_ scene ->
                                { scene
                                    | next =
                                        List.updateIf
                                            (Tuple.second >> (==) oldKey)
                                            (\( label, _ ) -> ( label, newKey ))
                                            scene.next
                                }
                            )
                   )
                |> clean
            , Cmd.none
            )

        FileSelected file ->
            ( model, Task.perform ReadFile <| File.toString file )

        ReadFile str ->
            case Codec.decodeString Model.dataCodec str of
                Err err ->
                    let
                        _ =
                            Debug.log "Err" err
                    in
                    ( model, Cmd.none )

                Ok newModel ->
                    ( newModel, Cmd.none )

        GenerateC ->
            ( model
            , File.Download.string "logic.c" "text/x-c" <|
                Generator.generate model
            )

        DownloadJson ->
            ( model
            , File.Download.string "data.json" "application/json" <|
                Codec.encodeToString 0 Model.dataCodec model
            )


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
        scenes =
            model
                |> dfsSort "main"
                |> (\l -> l ++ [ ( "", emptyScene ) ])
                |> List.map (\( k, v ) -> viewScene model k v)
    in
    column [ width fill, spacing rythm, padding rythm ]
        (fileControls model :: scenes)


fileControls : Model -> Element Msg
fileControls model =
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


rythm : number
rythm =
    10


viewScene : Model -> String -> Scene -> Element Msg
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
                , link [ Font.color <| Element.rgb 0 0 1 ]
                    { label = text "Scroll to"
                    , url = "#" ++ v
                    }
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
        |> List.map
            (wrappedRow
                [ spacing rythm
                , padding rythm
                , Border.width 1
                , width fill
                , height fill
                ]
            )
        |> wrappedRow
            [ Element.htmlAttribute <| Html.Attributes.id name
            , Border.width 1
            , width fill
            ]
        |> Element.map (Replace name)


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
