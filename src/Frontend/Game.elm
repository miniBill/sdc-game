module Frontend.Game exposing (viewGame)

import Dict
import Element exposing (Element, el, height, inFront, px, text, width)
import Element.Background as Background
import Element.Border as Border
import Pins
import Theme
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

        attrs =
            width (px scaledWidth) :: inFronts

        inFronts =
            model.data
                |> Maybe.withDefault Dict.empty
                |> Dict.toList
                |> List.concatMap
                    (\( _, city ) ->
                        let
                            ( x, y ) =
                                Pins.northEastToXY
                                    city.coordinates.north
                                    city.coordinates.east
                        in
                        [ inFront <|
                            el
                                [ Element.moveDown <| scale * y - Theme.rythm / 2
                                , Element.moveRight <| scale * x - Theme.rythm / 2
                                , Border.width 1
                                , Background.color Theme.colors.delete
                                , Border.rounded Theme.rythm
                                , width <| px Theme.rythm
                                , height <| px Theme.rythm
                                ]
                                Element.none
                        , inFront <|
                            el
                                [ Element.moveDown <| scale * y - Theme.rythm * 1.8
                                , Element.moveRight <| scale * x + Theme.rythm
                                , Border.width 1
                                , Background.color Theme.colors.semitransparent
                                , Border.rounded Theme.rythm
                                , Theme.padding
                                ]
                                (text city.name)
                        ]
                    )
    in
    Element.image attrs { src = "/art/europe.jpg", description = "A map of Europe" }
