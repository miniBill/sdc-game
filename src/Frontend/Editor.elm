module Frontend.Editor exposing (view)

import Dict
import Editors
import Element.WithContext as Element exposing (alignRight, alignTop, centerY, el, fill, height, image, inFront, padding, paddingEach, paddingXY, px, row, scrollbars, text, width)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Frontend.Common
import Html.Attributes
import Model exposing (Data, Id, Person)
import Theme exposing (Element)
import Types exposing (EditorModel, EditorMsg(..))


view : Maybe Data -> EditorModel -> Element EditorMsg
view maybeData editorModel =
    case maybeData of
        Nothing ->
            Frontend.Common.loading

        Just data ->
            let
                personView =
                    editorModel.currentPerson
                        |> Maybe.andThen
                            (\id ->
                                Maybe.map (Tuple.pair id) <|
                                    Dict.get id data
                            )
                        |> Maybe.map (\( id, person ) -> viewPerson id person)
                        |> Maybe.withDefault Element.none

                scrollableView =
                    Theme.column
                        [ scrollbars
                        , height fill
                        , width fill
                        , alignTop
                        ]
                        [ el [ Element.transparent True ] <|
                            controls data editorModel
                        , personView
                        ]
            in
            el
                [ width fill
                , height fill
                , Theme.spacing
                , inFront <|
                    el
                        [ Theme.padding
                        , Background.color Theme.colors.semitransparent
                        , Border.roundEach
                            { topLeft = 0
                            , topRight = 0
                            , bottomLeft = 0
                            , bottomRight = Theme.rythm
                            }
                        ]
                        (controls data editorModel)
                ]
                scrollableView


controls : Data -> EditorModel -> Element EditorMsg
controls data model =
    let
        btn msg color label =
            Theme.button
                [ alignTop
                , Background.color color
                ]
                { onPress = Just msg
                , label = text label
                }

        common =
            [ btn FileSelect Theme.colors.white "Upload JSON"
            , btn DownloadJson Theme.colors.white "Save as JSON"
            , btn AddPerson Theme.colors.addNew "Add Person"
            ]

        people =
            data
                |> Dict.toList
                |> List.sortBy (\( _, { name } ) -> name)
                |> List.map
                    (\( id, person ) ->
                        Theme.button
                            [ alignTop
                            , padding 0
                            , Background.color <|
                                if Just id == model.currentPerson then
                                    Theme.colors.selectedTab

                                else
                                    Theme.colors.tab
                            ]
                            { onPress = Just <| EditPerson id
                            , label =
                                row
                                    [ paddingXY Theme.rythm 0
                                    , Theme.spacing
                                    ]
                                    [ if String.isEmpty person.image then
                                        Element.none

                                      else
                                        image
                                            [ height <| px 30
                                            , centerY
                                            ]
                                            { -- If the image has trouble loading, we really don't want to show an alt text
                                              description = ""
                                            , src = person.image
                                            }
                                    , el [ paddingXY 0 Theme.rythm ] <|
                                        text <|
                                            if String.isEmpty person.name then
                                                "<New>"

                                            else
                                                person.name
                                    ]
                            }
                    )

        error =
            if String.isEmpty model.lastError then
                []

            else
                [ text model.lastError ]

        delete =
            case model.currentPerson of
                Nothing ->
                    []

                Just id ->
                    if Dict.member id data then
                        let
                            gradient color =
                                Background.gradient
                                    { angle = 0
                                    , steps =
                                        [ Theme.getColor 0
                                        , color
                                        , color
                                        ]
                                    }
                        in
                        [ el
                            [ Theme.spacing
                            , alignRight
                            , paddingEach
                                { left = 0
                                , right = Theme.rythm
                                , top = 0
                                , bottom = 0
                                }
                            , Element.moveDown <| 1 + Theme.rythm
                            ]
                            (Theme.tabButton
                                [ gradient Theme.colors.delete
                                , Element.htmlAttribute <| Html.Attributes.style "z-index" "1"
                                ]
                                { onPress = Just <| UpdatePerson id Nothing
                                , label = text "Delete"
                                }
                            )
                        ]

                    else
                        []
    in
    Element.wrappedRow
        [ Theme.spacing
        , alignTop
        ]
        (common ++ people ++ error ++ delete)


viewPerson : Id -> Person -> Element EditorMsg
viewPerson id person =
    el [ width fill ] <|
        Element.map (\newPerson -> UpdatePerson id <| Just newPerson) <|
            Tuple.first <|
                Editors.personEditor 0 person
