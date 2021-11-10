module Frontend.Editor exposing (viewEditor)

import Dict
import Editors
import Element exposing (Element, alignRight, alignTop, column, el, fill, height, inFront, moveDown, moveUp, paddingEach, row, scrollbars, text, width)
import Element.Background as Background
import Element.Border as Border
import Html.Attributes
import Model exposing (Data, Id, Person)
import Theme
import Types exposing (FrontendModel, FrontendMsg(..))


viewEditor : FrontendModel -> Data -> Element FrontendMsg
viewEditor model data =
    let
        peopleViews =
            data
                |> Dict.toList
                |> List.map (\( id, person ) -> viewPerson id person)
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
        , inFront <| controls model
        ]
        peopleViews


controls : FrontendModel -> Element FrontendMsg
controls model =
    let
        btn msg label =
            Theme.button [ alignTop, Background.color Theme.colors.white ]
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
        , Theme.button [ alignTop, Background.color Theme.colors.addNew ]
            { onPress = Just AddPerson
            , label = text "Add Person"
            }
        , text model.lastError
        ]


viewPerson : Id -> Person -> Element FrontendMsg
viewPerson id person =
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
    column [ width fill, moveDown 1 ]
        [ row
            [ Theme.spacing
            , alignRight
            , paddingEach
                { left = 0
                , right = Theme.rythm
                , top = 0
                , bottom = 0
                }
            , Element.htmlAttribute <| Html.Attributes.style "z-index" "1"
            ]
            [ Theme.tabButton [ gradient Theme.colors.delete ]
                { onPress = Just <| UpdatePerson id Nothing
                , label = text "Delete"
                }
            ]
        , el [ width fill, moveUp 1 ] <|
            Element.map (\newPerson -> UpdatePerson id <| Just newPerson) <|
                Tuple.first <|
                    Editors.personEditor 0 person
        ]
