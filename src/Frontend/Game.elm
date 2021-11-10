module Frontend.Game exposing (viewGame)

import Dict
import Element exposing (Element, alignRight, alignTop, centerX, centerY, el, fill, height, image, inFront, padding, paddingEach, paragraph, px, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Frontend.Common
import Markdown.Parser
import Markdown.Renderer
import Model exposing (Data, Id, Person)
import Pins
import Theme exposing (column, row)
import Types exposing (FrontendMsg(..), GameModel(..))


viewGame : GameModel -> Element FrontendMsg
viewGame model =
    case model of
        LoadingData ->
            Frontend.Common.loading

        DataEmpty ->
            text "branch 'DataEmpty' not implemented"

        ViewingMap data { currentPerson } ->
            let
                scale =
                    0.5

                originalWidth =
                    1830

                scaledWidth =
                    round <| scale * originalWidth

                normalAttrs =
                    [ width (px scaledWidth)
                    , centerX
                    , centerY
                    ]

                attrs =
                    normalAttrs ++ inFronts

                inFronts =
                    data
                        |> Dict.toList
                        |> List.concatMap
                            (\( personId, person ) ->
                                viewPerson scale
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
            viewPersonPreview data submodel

        Talking _ _ ->
            text "branch 'Talking _ _' not implemented"


viewPerson : Float -> Bool -> Id -> Person -> List (Element FrontendMsg)
viewPerson scale selected id person =
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
                , label = el [ Theme.padding ] child
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


viewPersonPreview : Data -> { currentPerson : Id } -> Element msg
viewPersonPreview data { currentPerson } =
    case Dict.get currentPerson data of
        Nothing ->
            text "TODO - MISSING PERSON"

        Just person ->
            let
                scale =
                    10

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
                                                List.map (Element.el [] << Element.html) ls
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
                    [ shadowBox [ alignTop, height fill ] <| viewMarked city.text
                    , shadowBox
                        [ height fill
                        , alignRight
                        , width <| px <| scale * 22
                        ]
                        (column [ centerY, spacing scale ]
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
