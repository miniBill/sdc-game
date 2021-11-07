module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Codec
import Codecs
import Dict
import Editors
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, row, text, width, wrappedRow)
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
            case Codec.decodeString Codecs.dataCodec str of
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
                Codec.encodeToString 0 Codecs.dataCodec data
            )

        ( AddCity, Just _ ) ->
            ( model
            , Random.int 0 Random.maxInt
                |> Random.map Hex.toString
                |> Random.generate (\newId -> UpdateCity newId (Just Editors.cityDefault))
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
                        |> List.map
                            (\( id, city ) ->
                                Element.map (UpdateCity id) <|
                                    Element.column [ Theme.spacing, width fill ]
                                        [ Input.button
                                            [ Border.width 1
                                            , Theme.padding
                                            , alignRight
                                            , Border.color <| Element.rgb 0 0 0
                                            , Background.color <| Element.rgb 1 0.6 0.6
                                            ]
                                            { onPress = Just Nothing
                                            , label = text "Delete"
                                            }
                                        , Element.map Just <| Editors.cityEditor city
                                        ]
                            )
                        |> wrappedRow [ Theme.spacing ]
            in
            column [ width fill, Theme.spacing, Theme.padding ]
                [ controls
                , citiesViews
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
    column [ Theme.spacing ]
        [ row [ Theme.spacing ] <|
            [ btn FileSelect "Upload JSON"
            , btn DownloadJson "Save as JSON"
            , btn AddCity "Add City"
            ]
        ]
