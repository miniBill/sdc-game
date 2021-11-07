module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Codec
import Codecs
import Dict
import Editors
import Element exposing (Element, alignRight, alignTop, centerX, centerY, column, el, fill, height, image, inFront, padding, paddingEach, paragraph, px, row, scrollbarY, scrollbars, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File
import File.Download
import File.Select
import Hex
import Html
import Lamdera exposing (Key, Url)
import Markdown.Parser
import Markdown.Renderer
import Model exposing (City, Id)
import Random
import Task
import Theme
import Types exposing (FrontendModel, FrontendMsg(..), ToBackend(..), ToFrontend(..))
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
                    , Element.layout [ Font.size fontSize, height fill, width fill ] <|
                        view model
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
        TFUpdateCity id city ->
            ( { model | data = Maybe.map (Dict.update id <| always city) model.data }, Cmd.none )

        TFData data ->
            ( { model
                | data = Just data
                , selectedCity =
                    let
                        _ =
                            Debug.todo
                    in
                    data
                        |> Dict.keys
                        |> List.head
                        |> Maybe.withDefault ""
              }
            , Cmd.none
            )


init : Url -> Key -> ( Model, Cmd Msg )
init _ key =
    ( { key = key
      , data = Nothing
      , lastError = ""
      , selectedCity = ""
      }
    , Cmd.none
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

        ( UpdateCity id city, Just data ) ->
            ( { model | data = Just <| Dict.update id (always city) data }
            , Lamdera.sendToBackend <| TBUpdateCity id city
            )

        ( ReadFile str, _ ) ->
            case Codec.decodeString Codecs.dataCodec str of
                Err _ ->
                    let
                        errString =
                            --Debug.toString err
                            "Error reading file"
                    in
                    ( { model | lastError = errString }, Cmd.none )

                Ok newData ->
                    ( { model | data = Just newData }, Lamdera.sendToBackend <| TBData newData )

        ( DownloadJson, Just data ) ->
            ( model
            , File.Download.string "sdc-game.json" "application/json" <|
                Codec.encodeToString 0 Codecs.dataCodec data
            )

        ( AddCity, Just _ ) ->
            ( model
            , Random.int 0 Random.maxInt
                |> Random.map Hex.toString
                |> Random.generate (\newId -> UpdateCity newId (Just Editors.cityDefault))
            )

        ( SelectCity id, Just _ ) ->
            ( { model | selectedCity = id }, Cmd.none )


view : Model -> Element Msg
view model =
    case model.data of
        Nothing ->
            el [ Font.size 40, centerX, centerY, Font.center ] <| text "Loading..."

        Just data ->
            let
                citiesViews =
                    data
                        |> Dict.toList
                        |> List.map (\( id, city ) -> viewCity id city)
                        |> column
                            [ paddingEach
                                { left = Theme.rythm
                                , top = 2 * Theme.rythm + 1
                                , right = 480 + Theme.rythm
                                , bottom = Theme.rythm
                                }
                            , Theme.spacing
                            , scrollbars
                            , height fill
                            , width fill
                            , alignTop
                            ]
            in
            el
                [ width fill
                , height fill
                , Theme.spacing
                , inFront <|
                    row [ width fill ]
                        [ controls
                        , gameView data model.selectedCity
                        ]
                ]
                citiesViews


gameView : Model.Data -> Model.Id -> Element msg
gameView data selectedCity =
    el [ alignTop, alignRight, Theme.padding ] <|
        case Dict.get selectedCity data of
            Nothing ->
                text "Select a city to show a preview"

            Just city ->
                let
                    scale =
                        6

                    shadow =
                        Element.rgba 1 1 1 0.7

                    shadowBox attrs =
                        el
                            ([ Element.padding scale
                             , Border.rounded scale
                             , Background.color shadow
                             ]
                                ++ attrs
                            )

                    viewMarked input =
                        input
                            |> String.split "  "
                            |> List.filterMap (Markdown.Parser.parse >> Result.toMaybe)
                            |> List.filterMap
                                (\g ->
                                    g
                                        |> Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer
                                        |> Result.toMaybe
                                        |> Maybe.map
                                            (\ls ->
                                                paragraph [ width fill ] <|
                                                    List.map Element.html ls
                                            )
                                )
                            |> column [ spacing scale, width <| px <| scale * (80 - 28) ]
                in
                column
                    [ Background.image city.image
                    , width <| px <| scale * 80
                    , height <| px <| scale * 45
                    , Border.width 1
                    , Font.size <| scale * 3
                    ]
                    [ el [ padding scale, centerX ] <|
                        shadowBox
                            [ paddingEach
                                { left = scale
                                , top = scale
                                , right = scale
                                , bottom = scale * 5 // 2
                                }
                            , Font.size <| scale * 6
                            ]
                            (text city.name)
                    , row [ padding scale, spacing scale, width fill, height fill ]
                        [ shadowBox [ alignTop ] <| viewMarked city.text
                        , case city.people |> List.head of
                            Nothing ->
                                Element.none

                            Just person ->
                                shadowBox
                                    [ centerY
                                    , alignRight
                                    , width <| px <| scale * 22
                                    ]
                                    (column [ spacing scale ]
                                        [ el [ centerX ] <| text person.name
                                        , image
                                            [ width <| px <| scale * 20
                                            ]
                                            { description = "Person avatar"
                                            , src = person.image
                                            }
                                        ]
                                    )
                        ]
                    ]


viewCity : Id -> City -> Element Msg
viewCity id city =
    column [ width fill ]
        [ row [ width fill, Theme.spacing ]
            [ Input.button
                [ Border.widthEach { left = 1, top = 1, right = 1, bottom = 0 }
                , Theme.padding
                , alignRight
                ]
                { onPress = Just <| SelectCity id
                , label = text "Select"
                }
            , Input.button
                [ Border.widthEach { left = 1, top = 1, right = 1, bottom = 0 }
                , Theme.padding
                , alignRight
                , Background.color <| Element.rgb 1 0.6 0.6
                ]
                { onPress = Just <| UpdateCity id Nothing
                , label = text "Delete"
                }
            ]
        , Element.map (\newCity -> UpdateCity id <| Just newCity) <| Editors.cityEditor 0 city
        ]


controls : Element Msg
controls =
    let
        btn msg label =
            Input.button [ Border.width 1, Theme.padding ]
                { onPress = Just msg
                , label = text label
                }
    in
    row
        [ Theme.spacing
        , alignTop
        , Border.roundEach
            { topLeft = 0
            , topRight = 0
            , bottomLeft = 0
            , bottomRight = Theme.rythm
            }
        , Theme.padding
        , Background.color <| Element.rgb 1 1 1
        ]
        [ btn FileSelect "Upload JSON"
        , btn DownloadJson "Save as JSON"
        , btn AddCity "Add City"
        ]
