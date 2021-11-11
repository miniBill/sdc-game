module Frontend.Game exposing (view)

import Dict
import Element exposing (Element, alignRight, alignTop, centerX, centerY, el, fill, height, image, inFront, padding, paddingEach, paragraph, px, spacing, text, textColumn, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Frontend.Common
import Html.Attributes
import Markdown.Parser
import Markdown.Renderer
import Model exposing (Choice, Data, Dialog, Id, Next(..), Person)
import Pins
import Theme exposing (column, row)
import Types exposing (FrontendMsg(..), GameModel(..))


view : GameModel -> Element FrontendMsg
view model =
    case model of
        LoadingData ->
            Frontend.Common.loading

        DataEmpty ->
            text "branch 'DataEmpty' not implemented"

        ViewingMap data { currentPerson } ->
            let
                scale =
                    0.65

                originalWidth =
                    1830

                scaledWidth =
                    round <| scale * originalWidth

                normalAttrs =
                    [ width (px scaledWidth)
                    , centerX
                    , centerY
                    , Font.size <| round <| 22 * scale
                    ]

                attrs =
                    normalAttrs ++ inFronts

                inFronts =
                    data
                        |> Dict.toList
                        |> List.concatMap
                            (\( personId, person ) ->
                                viewPinOnMap scale
                                    (personId == currentPerson)
                                    personId
                                    person
                            )
                        |> List.map inFront
            in
            Element.image attrs
                { src = "/art/europe.jpg"
                , description = "A map of Europe"
                }

        ViewingPerson data submodel ->
            let
                scale =
                    10
            in
            viewPerson scale data submodel

        Talking data submodel ->
            let
                scale =
                    10
            in
            viewTalking scale data submodel


viewPinOnMap : Float -> Bool -> Id -> Person -> List (Element FrontendMsg)
viewPinOnMap scale selected id person =
    let
        city =
            person.city

        ( x, y ) =
            Pins.northEastToXY
                city.coordinates.north
                city.coordinates.east

        btn attrs child =
            Input.button
                attrs
                { onPress = Just <| ViewPerson id
                , label = el [ padding <| Theme.rythm // 2 ] child
                }
    in
    [ btn
        [ Element.moveDown <| scale * y + Theme.rythm * -1.5
        , Element.moveRight <| scale * x + Theme.rythm * -1.5
        , Border.width 1
        , Background.color Theme.colors.delete
        , Border.rounded Theme.rythm
        , width <| px Theme.rythm
        , height <| px Theme.rythm
        ]
        Element.none
    , btn
        [ Element.moveDown <| scale * y + Theme.rythm * -2.8
        , Element.moveRight <| scale * x + Theme.rythm * 0
        , Border.width <|
            if selected then
                2

            else
                1
        , Background.color <|
            if selected then
                Theme.colors.addNew

            else
                Theme.colors.semitransparent
        , if selected then
            Font.bold

          else
            Font.regular
        , Border.rounded Theme.rythm
        ]
        (text city.name)
    ]


viewPerson : Int -> Data -> { currentPerson : Id } -> Element FrontendMsg
viewPerson scale data { currentPerson } =
    case Dict.get currentPerson data of
        Nothing ->
            text "TODO - MISSING PERSON"

        Just person ->
            withPerson scale person <|
                Theme.button [ centerX, centerY ]
                    { onPress = Just <| TalkTo currentPerson person.dialog
                    , label = text <| "Talk to " ++ person.name
                    }


viewTalking : Int -> Data -> { currentDialog : Dialog, currentPerson : Id } -> Element FrontendMsg
viewTalking scale data { currentDialog, currentPerson } =
    case Dict.get currentPerson data of
        Nothing ->
            text "TODO - MISSING PERSON"

        Just person ->
            withPerson scale person <|
                viewDialog scale currentPerson currentDialog


viewDialog : Int -> Id -> Dialog -> Element FrontendMsg
viewDialog scale currentPerson { text, choices } =
    Theme.column []
        [ viewMarked scale text
        , choices
            |> List.map (viewChoice currentPerson)
            |> Theme.row []
        ]


viewChoice : Id -> Choice -> Element FrontendMsg
viewChoice currentPerson { text, next } =
    Theme.button [ width fill ]
        { label =
            paragraph [ width fill ]
                [ Element.text text ]
        , onPress =
            Just <|
                case next of
                    NextDialog n ->
                        TalkTo currentPerson n

                    NextViewMap ->
                        ViewMap currentPerson
        }


withPerson : Int -> Person -> Element FrontendMsg -> Element FrontendMsg
withPerson scale person inner =
    let
        city =
            person.city

        shadowBox attrs =
            el
                ([ Element.padding scale
                 , Border.rounded scale
                 , Background.color Theme.colors.semitransparent
                 ]
                    ++ attrs
                )
    in
    column
        [ Element.behindContent <|
            Element.el
                [ width <| px <| scale * 80
                , height <| px <| scale * 45
                , Element.htmlAttribute <| Html.Attributes.style "background-image" <| "url('" ++ city.image ++ "')"
                , Element.htmlAttribute <| Html.Attributes.style "background-position" "center"
                , Element.htmlAttribute <| Html.Attributes.style "background-size" "cover"
                ]
                Element.none
        , width <| px <| scale * 80
        , height <| px <| scale * 45
        , Border.width 1
        , Font.size <| scale * 3
        ]
        [ el [ centerX ] <|
            shadowBox
                [ paddingEach
                    { left = scale
                    , top = scale
                    , right = scale
                    , bottom = scale * 5 // 2
                    }
                , Font.size <| scale * 3
                ]
                (textColumn
                    [ spacing scale
                    , Font.center
                    ]
                    [ paragraph [ Font.bold ] [ text city.name ]
                    , paragraph [] [ text city.text ]
                    ]
                )
        , row [ spacing <| 2 * scale, width fill, height fill ]
            [ shadowBox [ alignTop, height fill, width fill ] inner
            , shadowBox
                [ height fill
                , alignRight
                , width <| px <| scale * 26
                , centerX
                ]
                (column
                    [ centerX
                    , centerY
                    , spacing scale
                    ]
                    [ el [ centerX ] <| text person.name
                    , image
                        [ width <| px <| scale * 20
                        , centerX
                        ]
                        { description = "Person avatar"
                        , src = person.image
                        }
                    ]
                )
            ]
        ]


viewMarked : Int -> String -> Element msg
viewMarked scale input =
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
                                List.map (Element.el [] << Element.html) ls
                        )
            )
        |> textColumn [ spacing scale ]
