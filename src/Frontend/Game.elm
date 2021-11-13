module Frontend.Game exposing (view)

import Angle
import Dict
import Element.WithUnits as Element exposing (Element, Orientation(..), centerX, centerY, column, el, fill, fillPortion, height, image, inFront, padding, paragraph, px, row, spacing, text, textColumn, width)
import Element.WithUnits.Background as Background
import Element.WithUnits.Border as Border
import Element.WithUnits.Font as Font
import Element.WithUnits.Input as Input
import Frontend.Common
import Html.Attributes
import Length exposing (Length)
import Markdown.Parser
import Markdown.Renderer
import Model exposing (Choice, Data, Dialog, Id, Next(..), Person)
import Pins exposing (mapSize)
import Quantity
import Theme
import Types exposing (FrontendMsg(..), GameModel(..))


rythm : Length
rythm =
    Length.millimeters 40


baseFontSize : Length
baseFontSize =
    Length.millimeters 100


borderWidth : Length
borderWidth =
    Length.millimeters 5


view : GameModel -> Element FrontendMsg
view model =
    el
        [ width fill
        , height fill
        , Font.size baseFontSize
        ]
        (case model of
            LoadingData ->
                Element.element Frontend.Common.loading

            DataEmpty ->
                text "branch 'DataEmpty' not implemented"

            ViewingMap data { currentPerson } ->
                let
                    normalAttrs =
                        [ width <| px mapSize.width
                        , height <| px mapSize.height
                        , centerX
                        , centerY
                        , Font.size (Quantity.multiplyBy 0.3 baseFontSize)
                        ]

                    attrs =
                        normalAttrs ++ inFronts

                    inFronts =
                        data
                            |> Dict.toList
                            |> List.concatMap
                                (\( personId, person ) ->
                                    viewPinOnMap
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
                viewPerson data submodel

            Talking data submodel ->
                viewTalking data submodel
        )


viewPinOnMap : Bool -> Id -> Person -> List (Element FrontendMsg)
viewPinOnMap selected id person =
    let
        city =
            person.city

        ( x, y ) =
            Pins.northEastToXY
                (Angle.degrees city.coordinates.north)
                (Angle.degrees city.coordinates.east)

        btn attrs child =
            Input.button
                attrs
                { onPress = Just <| ViewPerson id
                , label = el [ padding (Quantity.multiplyBy 0.5 rythm) ] child
                }
    in
    [ btn
        [ Element.moveDown <| Quantity.plus y <| Quantity.multiplyBy -1.5 rythm
        , Element.moveRight <| Quantity.plus x <| Quantity.multiplyBy -1.5 rythm
        , Border.width borderWidth
        , Background.color Theme.colors.delete
        , Border.rounded rythm
        , width <| px rythm
        , height <| px rythm
        ]
        Element.none
    , btn
        [ Element.moveDown <| Quantity.plus y <| Quantity.multiplyBy -2.8 rythm
        , Element.moveRight <| Quantity.plus x <| Quantity.multiplyBy 0.0 rythm
        , Border.width <|
            Quantity.multiplyBy
                (if selected then
                    1

                 else
                    0.5
                )
                borderWidth
        , Background.color <|
            if selected then
                Theme.colors.addNew

            else
                Theme.colors.semitransparent
        , if selected then
            Font.bold

          else
            Font.regular
        , Border.rounded rythm
        ]
        (text city.name)
    ]


viewPerson : Data -> { currentPerson : Id } -> Element FrontendMsg
viewPerson data { currentPerson } =
    case Dict.get currentPerson data of
        Nothing ->
            text "TODO - MISSING PERSON"

        Just person ->
            withPerson person <|
                Input.button [ centerX, centerY ]
                    { onPress = Just <| TalkTo currentPerson person.dialog
                    , label = text <| "Talk to " ++ person.name
                    }


viewTalking : Data -> { currentDialog : Dialog, currentPerson : Id } -> Element FrontendMsg
viewTalking data { currentDialog, currentPerson } =
    case Dict.get currentPerson data of
        Nothing ->
            text "TODO - MISSING PERSON"

        Just person ->
            withPerson person <|
                viewDialog currentPerson currentDialog


viewDialog : Id -> Dialog -> Element FrontendMsg
viewDialog currentPerson { text, choices } =
    column []
        [ viewMarked text
        , choices
            |> List.map (viewChoice currentPerson)
            |> row []
        ]


viewChoice : Id -> Choice -> Element FrontendMsg
viewChoice currentPerson { text, next } =
    Input.button [ width fill ]
        { label =
            paragraph
                [ Border.rounded rythm
                , padding rythm
                , Border.width borderWidth
                , width fill
                ]
                [ Element.text text ]
        , onPress =
            Just <|
                case next of
                    NextDialog n ->
                        TalkTo currentPerson n

                    NextViewMap ->
                        ViewMap currentPerson
        }


withPerson : Person -> Element FrontendMsg -> Element FrontendMsg
withPerson person inner =
    let
        city =
            person.city

        semiBox attrs =
            el
                ([ padding rythm
                 , Border.rounded rythm
                 , Background.color Theme.colors.semitransparent
                 ]
                    ++ attrs
                )
    in
    column
        [ Element.behindContent <|
            el
                [ width fill
                , height fill
                , Element.htmlAttribute <| Html.Attributes.style "background-image" <| "url('" ++ city.image ++ "')"
                , Element.htmlAttribute <| Html.Attributes.style "background-position" "center"
                , Element.htmlAttribute <| Html.Attributes.style "background-size" "cover"
                ]
                Element.none
        , width fill
        , height fill
        , padding rythm
        , spacing rythm
        ]
        [ semiBox [ centerX ]
            (column
                [ spacing rythm
                , Font.center
                ]
                [ el [ centerX, Font.bold ] <| text city.name
                , el [ centerX ] <| text city.text
                ]
            )
        , Element.withOrientation
            (\orientation ->
                case orientation of
                    Landscape ->
                        row
                            [ spacing rythm
                            , width fill
                            , height fill
                            ]
                            [ semiBox
                                [ height fill
                                , width <| fillPortion 5
                                ]
                                inner
                            , semiBox
                                [ height fill
                                , width <| fillPortion 3
                                ]
                                (column [ centerX, centerY, spacing rythm ]
                                    [ el [ centerX ] <| text person.name
                                    , image [ width fill ]
                                        { description = "Person avatar"
                                        , src = person.image
                                        }
                                    ]
                                )
                            ]

                    Portrait ->
                        column
                            [ spacing rythm
                            , width fill
                            , height fill
                            ]
                            [ semiBox [ width fill ]
                                (column [ centerX, centerY, spacing rythm ]
                                    [ el [ centerX ] <| text person.name
                                    , image
                                        [ width <| px <| Quantity.multiplyBy 20 rythm
                                        , centerX
                                        ]
                                        { description = "Person avatar"
                                        , src = person.image
                                        }
                                    ]
                                )
                            , semiBox
                                [ height fill
                                , width fill
                                ]
                                inner
                            ]
            )
        ]


viewMarked : String -> Element msg
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
                                List.map (el [] << Element.html) ls
                        )
            )
        |> textColumn [ spacing rythm ]
