module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Codec
import Dict
import Element exposing (Element, alignRight, alignTop, behindContent, centerX, centerY, column, el, fill, height, image, padding, row, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File
import File.Download
import File.Select
import Html
import Lamdera exposing (Key, Url)
import List.Extra as List
import Model exposing (City)
import Task
import Theme exposing (input, multiline, rythm)
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
        TFUpdateCity id city ->
            ( { model | data = Maybe.map (Dict.update id <| always city) model.data }, Cmd.none )

        TFData data ->
            ( { model | data = Just data }, Cmd.none )


init : Url -> Key -> ( Model, Cmd Msg )
init _ key =
    ( { key = key
      , data = Nothing
      , lastError = ""
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
            case Codec.decodeString Model.dataCodec str of
                Err err ->
                    let
                        errString =
                            Debug.toString err
                    in
                    ( { model | lastError = errString }, Cmd.none )

                Ok newData ->
                    ( { model | data = Just newData }, Lamdera.sendToBackend <| TBData newData )

        ( DownloadJson, Just data ) ->
            ( model
            , File.Download.string "data.json" "application/json" <|
                Codec.encodeToString 0 Model.dataCodec data
            )


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
                        |> List.map (\( id, city ) -> Element.map (UpdateCity id) <| viewCity city)
                        |> wrappedRow [ spacing rythm ]
            in
            column [ width fill, spacing rythm, padding rythm ]
                [ fileControls
                , citiesViews
                ]


fileControls : Element Msg
fileControls =
    let
        btn msg label =
            Input.button [ Border.width 1, Theme.padding ]
                { onPress = Just msg
                , label = text label
                }
    in
    column [ spacing rythm ]
        [ row [ spacing rythm ] <|
            [ btn FileSelect "Upload JSON"
            , btn DownloadJson "Save as JSON"
            ]
        ]


viewCity : City -> Element (Maybe City)
viewCity city =
    column
        [ Border.width 1
        , width fill
        , spacing rythm
        , padding rythm
        , behindContent <|
            image [ width fill, height fill ]
                { src = city.image
                , description = ""
                }
        , Background.color <| Element.rgba 0.2 0.2 0.2 0.2
        ]
        [ row [ spacing rythm, width fill ]
            [ input [ width fill ]
                { label = "Name"
                , text = city.name
                , onChange = \newName -> Just { city | name = newName }
                }
            , Input.button [ alignRight, Border.width 1 ]
                { onPress = Just Nothing
                , label = text "X"
                }
            ]
        , input [ width fill ]
            { label = "Image"
            , text = city.image
            , onChange = \newImage -> Just { city | image = newImage }
            }
        , multiline
            [ alignTop
            , height fill
            , width fill
            ]
            { label = "Text"
            , text = city.text
            , onChange = \newText -> Just { city | text = newText }
            }
        ]
