module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Codec
import Codecs
import Dict
import Editors
import Element exposing (Element, centerX, centerY, el, fill, height, text, width)
import Element.Font as Font
import File
import File.Download
import File.Select
import Frontend.Editor exposing (viewEditor)
import Frontend.Game exposing (viewGame)
import Hex
import Html
import Lamdera exposing (Key, Url)
import Random
import Task
import Theme
import Types exposing (FrontendModel, FrontendMsg(..), Page(..), Preview(..), ToBackend(..), ToFrontend(..))
import Url
import Url.Parser


app :
    { init : Url -> Key -> ( FrontendModel, Cmd FrontendMsg )
    , view : FrontendModel -> Browser.Document FrontendMsg
    , update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , subscriptions : FrontendModel -> Sub FrontendMsg
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
                , body =
                    [ css
                    , Element.layout [ Theme.fontSizes.normal, height fill, width fill ] <|
                        view model
                    ]
                }
        , update = update
        , subscriptions = subscriptions
        , updateFromBackend = updateFromBackend
        }


css : Html.Html FrontendMsg
css =
    let
        content =
            """
            select {
                font-size: """ ++ String.fromInt Theme.fontSize ++ """px;
            }
            """
    in
    Html.node "style" [] [ Html.text content ]


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        TFUpdateCity id city ->
            ( { model | data = Maybe.map (Dict.update id <| always city) model.data }, Cmd.none )

        TFData data ->
            ( { model | data = Just data }
            , Cmd.none
            )


urlToPage : Url -> Page
urlToPage url =
    let
        parser =
            Url.Parser.oneOf
                [ Url.Parser.map (Editor { preview = PreviewNone }) <| Url.Parser.s "editor"
                , Url.Parser.map (Game {}) Url.Parser.top
                ]
    in
    Url.Parser.parse parser url
        |> Maybe.withDefault (Game {})


init : Url -> Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , data = Nothing
      , lastError = ""
      , page = urlToPage url
      }
    , Cmd.none
    )


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions _ =
    Sub.none


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case ( msg, model.data ) of
        ( _, Nothing ) ->
            ( model, Cmd.none )

        ( UrlClicked urlRequest, _ ) ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        ( UrlChanged url, _ ) ->
            ( { model | page = urlToPage url }, Cmd.none )

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

        ( Preview preview, Just _ ) ->
            case model.page of
                Editor e ->
                    ( { model
                        | page = Editor { e | preview = preview }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


view : FrontendModel -> Element FrontendMsg
view model =
    case model.data of
        Nothing ->
            el
                [ Theme.fontSizes.huge
                , centerX
                , centerY
                , Font.center
                ]
                (text "Loading...")

        Just data ->
            case model.page of
                Game _ ->
                    viewGame model

                Editor e ->
                    viewEditor e data
