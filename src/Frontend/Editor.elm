module Frontend.Editor exposing (viewEditor)

import Dict
import Editors
import Element exposing (Element, alignRight, alignTop, centerX, centerY, column, el, fill, height, image, inFront, padding, paddingEach, paragraph, px, row, scrollbars, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Markdown.Parser
import Markdown.Renderer
import Model exposing (City, Data, Id)
import Theme
import Types exposing (FrontendMsg(..), Preview(..))


viewCity : Id -> City -> Element FrontendMsg
viewCity id city =
    column [ width fill ]
        [ row
            [ Theme.spacing
            , alignRight
            , paddingEach
                { left = 0
                , right = Theme.rythm
                , top = 0
                , bottom = 0
                }
            ]
            [ Theme.tabButton []
                { onPress = Just <| Preview <| PreviewSmall id
                , label = text "Small preview"
                }
            , Theme.tabButton []
                { onPress = Just <| Preview <| PreviewBig id
                , label = text "Big preview"
                }
            , Theme.tabButton [ Background.color Theme.colors.delete ]
                { onPress = Just <| UpdateCity id Nothing
                , label = text "Delete"
                }
            ]
        , Element.map (\newCity -> UpdateCity id <| Just newCity) <|
            Tuple.first <|
                Editors.cityEditor 0 city
        ]


controls : Element FrontendMsg
controls =
    let
        btn msg label =
            Theme.button [ Background.color Theme.colors.white ]
                { onPress = Just msg
                , label = text label
                }
    in
    Theme.row
        [ alignTop
        , Border.roundEach
            { topLeft = 0
            , topRight = 0
            , bottomLeft = 0
            , bottomRight = Theme.rythm
            }
        , Background.color Theme.colors.semitransparent
        ]
        [ btn FileSelect "Upload JSON"
        , btn DownloadJson "Save as JSON"
        , Theme.button [ Background.color Theme.colors.addNew ]
            { onPress = Just AddCity
            , label = text "Add City"
            }
        ]


viewCityPreview : Int -> City -> Element msg
viewCityPreview scale city =
    let
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
            , case city.people |> List.head of
                Nothing ->
                    Element.none

                Just person ->
                    shadowBox
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


viewPreview : Model.Data -> Preview -> Element FrontendMsg
viewPreview data preview =
    let
        selected =
            case preview of
                PreviewNone ->
                    Nothing

                PreviewSmall id ->
                    Just ( id, False )

                PreviewBig id ->
                    Just ( id, True )
    in
    case selected of
        Nothing ->
            Element.none

        Just ( selectedCity, fullscreen ) ->
            case Dict.get selectedCity data of
                Nothing ->
                    Element.none

                Just city ->
                    let
                        scale =
                            if fullscreen then
                                18

                            else
                                6
                    in
                    el
                        [ alignTop
                        , alignRight
                        , Theme.padding
                        ]
                        (Theme.column
                            [ Border.rounded Theme.rythm
                            , Border.width 1
                            , Background.color Theme.colors.semitransparent
                            ]
                            [ row [ width fill ]
                                [ el [ centerX, Theme.fontSizes.big ] <| text "Preview"
                                , Theme.button [ Background.color Theme.colors.delete ]
                                    { label = text "X"
                                    , onPress = Just <| Preview PreviewNone
                                    }
                                ]
                            , viewCityPreview scale city
                            ]
                        )


viewEditor : { a | preview : Preview } -> Data -> Element FrontendMsg
viewEditor { preview } data =
    let
        citiesViews =
            data
                |> Dict.toList
                |> List.map (\( id, city ) -> viewCity id city)
                |> column
                    [ paddingEach
                        { left = Theme.rythm
                        , top = 2 * Theme.rythm + 1
                        , right = Theme.rythm
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
        , inFront controls
        , inFront <| viewPreview data preview
        ]
        citiesViews
