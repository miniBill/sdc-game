module Frontend.Game exposing (viewGame)

import Dict
import Element exposing (Element, alignRight, alignTop, centerX, centerY, el, fill, height, image, inFront, padding, paddingEach, paragraph, px, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Markdown.Parser
import Markdown.Renderer
import Model exposing (Person)
import Pins
import Theme exposing (column, row)
import Types exposing (FrontendModel)


viewGame : FrontendModel -> Element msg
viewGame model =
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
            model.data
                |> Maybe.withDefault Dict.empty
                |> Dict.toList
                |> List.concatMap
                    (\( _, person ) ->
                        viewPerson scale person
                    )
                |> List.map inFront
    in
    Element.image attrs { src = "/art/europe.jpg", description = "A map of Europe" }


viewPerson : Float -> Person -> List (Element msg)
viewPerson scale person =
    let
        city =
            person.city

        ( x, y ) =
            Pins.northEastToXY
                city.coordinates.north
                city.coordinates.east
    in
    [ el
        [ Element.moveDown <| scale * y - Theme.rythm / 2
        , Element.moveRight <| scale * x - Theme.rythm / 2
        , Border.width 1
        , Background.color Theme.colors.delete
        , Border.rounded Theme.rythm
        , width <| px Theme.rythm
        , height <| px Theme.rythm
        ]
        Element.none
    , el
        [ Element.moveDown <| scale * y - Theme.rythm * 1.8
        , Element.moveRight <| scale * x + Theme.rythm
        , Border.width 1
        , Background.color Theme.colors.semitransparent
        , Border.rounded Theme.rythm
        , Theme.padding
        ]
        (text city.name)
    ]


viewPersonPreview : Int -> Person -> Element msg
viewPersonPreview scale person =
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
