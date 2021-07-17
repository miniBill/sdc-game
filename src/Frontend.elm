module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Codec
import Dict
import Element exposing (Attribute, Element, alignTop, centerX, centerY, column, el, fill, height, image, link, none, padding, px, row, spacing, text, width, wrappedRow)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File
import File.Download
import File.Select
import Generator
import Html
import Html.Attributes
import Html.Events
import Lamdera exposing (Key, Url)
import List.Extra as List
import Model exposing (dfsSort, emptyScene, replaceScene)
import Task
import Types exposing (Data, FrontendMsg(..), Model, Scene, ToBackend(..), ToFrontend(..))
import Url


app :
    { init : Url -> Key -> ( Model, Cmd FrontendMsg )
    , view : Model -> Browser.Document FrontendMsg
    , update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
    , updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
    , subscriptions : Model -> Sub FrontendMsg
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
                , body = [ Element.layout [] <| view model ]
                }
        , update = update
        , subscriptions = subscriptions
        , updateFromBackend = updateFromBackend
        }


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        TFReplace oldKey ( newKey, newValue ) ->
            ( { model | data = Maybe.map (replaceScene oldKey newKey newValue) model.data }, Cmd.none )

        TFData data ->
            ( { model | data = Just data }, Cmd.none )


init : Url -> Key -> ( Model, Cmd FrontendMsg )
init _ key =
    ( { key = key
      , data = Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub FrontendMsg
subscriptions _ =
    Sub.none


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case ( msg, model.data ) of
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

        ( Replace _ _, _ ) ->
            ( model, Cmd.none )

        ( FileSelected file, _ ) ->
            ( model, Task.perform ReadFile <| File.toString file )

        ( ReadFile str, _ ) ->
            case Codec.decodeString Model.dataCodec str of
                Err err ->
                    let
                        _ =
                            Debug.log "Err" err
                    in
                    ( model, Cmd.none )

                Ok newData ->
                    ( { model | data = Just newData }, Lamdera.sendToBackend <| TBData newData )

        ( GenerateC, Just data ) ->
            ( model
            , File.Download.string "logic.c" "text/x-c" <|
                Generator.generate data
            )

        ( GenerateC, Nothing ) ->
            ( model, Cmd.none )

        ( DownloadJson, Just data ) ->
            ( model
            , File.Download.string "data.json" "application/json" <|
                Codec.encodeToString 0 Model.dataCodec data
            )

        ( DownloadJson, Nothing ) ->
            ( model, Cmd.none )


view : Model -> Element FrontendMsg
view model =
    case model.data of
        Nothing ->
            el [ Font.size 40, centerX, centerY, Font.center ] <| text "Loading..."

        Just data ->
            let
                scenes =
                    data
                        |> dfsSort "main"
                        |> (\l -> l ++ [ ( "", emptyScene ) ])
                        |> List.map (\( k, v ) -> viewScene data k v)
            in
            column [ width fill, spacing rythm, padding rythm ]
                (fileControls :: scenes)


fileControls : Element FrontendMsg
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


rythm : number
rythm =
    10


viewScene : Data -> String -> Scene -> Element FrontendMsg
viewScene data name scene =
    let
        toOption selected key =
            Html.option
                [ Html.Attributes.value key
                , Html.Attributes.selected <| key == selected
                ]
                [ Html.text key ]

        viewNext i ( k, v ) =
            [ Input.multiline [ alignTop, width <| Element.minimum 240 fill ]
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
                , spellcheck = True
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
                            List.map (toOption v) ("" :: Dict.keys data ++ [ "end" ])
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
                image [ width <| px 300 ]
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
