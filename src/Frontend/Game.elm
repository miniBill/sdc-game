module Frontend.Game exposing (view)

import Angle
import Dict
import Element.WithUnits as Element exposing (Attribute, Element, Orientation(..), alignRight, alignTop, centerX, centerY, column, el, fill, height, image, inFront, padding, px, row, shrink, spacing, text, width)
import Element.WithUnits.Background as Background
import Element.WithUnits.Border as Border
import Element.WithUnits.Font as Font
import Element.WithUnits.Input as Input
import Frontend.Common exposing (viewMarked)
import Html.Attributes
import Length exposing (Length)
import Model exposing (Choice, City, Data, Dialog, Id, Next(..), Person)
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
    Length.millimeters 3


view : GameModel -> Element FrontendMsg
view model =
    el
        [ width fill
        , height fill
        , Font.size baseFontSize
        , inFront <|
            el
                [ Font.size (Quantity.multiplyBy 0.5 rythm)
                , alignRight
                , alignTop
                ]
                (Element.withSize
                    (\size ->
                        text <|
                            String.join "x" <|
                                List.map
                                    (Quantity.unwrap >> String.fromFloat)
                                    [ size.width
                                    , size.height
                                    ]
                    )
                )
        ]
        (case model of
            LoadingData ->
                Element.element Frontend.Common.loading

            DataEmpty ->
                text "branch 'DataEmpty' not implemented"

            ViewingMap data submodel ->
                viewMap data submodel

            ViewingPerson data submodel ->
                viewPerson data submodel

            Talking data submodel ->
                viewTalking data submodel
        )


viewMap : Data -> { currentPerson : Id } -> Element FrontendMsg
viewMap data { currentPerson } =
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


viewPinOnMap : Bool -> Id -> Person -> List (Element FrontendMsg)
viewPinOnMap selected id person =
    let
        city =
            person.city

        radius =
            Quantity.multiplyBy 0.3 rythm

        ( x, y ) =
            Pins.northEastToXY
                (Angle.degrees city.coordinates.north)
                (Angle.degrees city.coordinates.east)

        btn attrs child =
            Input.button
                attrs
                { onPress = Just <| ViewPerson id
                , label = el [ padding (Quantity.multiplyBy 0.25 rythm) ] child
                }
    in
    [ btn
        [ Element.moveDown <| Quantity.plus y <| Quantity.negate radius
        , Element.moveRight <| Quantity.plus x <| Quantity.negate radius
        , Border.width borderWidth
        , Background.color Theme.colors.delete
        , Border.rounded radius
        , width <| px <| Quantity.multiplyBy 2 radius
        , height <| px <| Quantity.multiplyBy 2 radius
        ]
        Element.none
    , btn
        [ Element.moveDown <| Quantity.plus y <| Quantity.multiplyBy -0.75 rythm
        , Element.moveRight <| Quantity.plus x <| Quantity.multiplyBy 2 radius
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
            Element.withOrientation
                (\orientation ->
                    orientationContainer
                        [ padding rythm
                        , spacing rythm
                        , width fill
                        , height fill
                        , cityBackground person.city
                        ]
                        [ viewCityDescription
                            (if orientation == Portrait then
                                [ width fill ]

                             else
                                [ width fill
                                , height fill
                                ]
                            )
                            person.city
                        , semiBox
                            [ height fill
                            , if orientation == Portrait then
                                width fill

                              else
                                width shrink
                            ]
                            (column
                                [ centerX
                                , centerY
                                , spacing rythm
                                , width fill
                                ]
                                [ image
                                    [ if orientation == Portrait then
                                        width <| px <| Quantity.multiplyBy 30 rythm

                                      else
                                        width fill
                                    , centerX
                                    ]
                                    { description = "Person avatar"
                                    , src = person.image
                                    }
                                , Input.button [ centerX, centerY ]
                                    { onPress = Just <| TalkTo currentPerson person.dialog
                                    , label =
                                        semiBox
                                            [ Border.width borderWidth
                                            , width fill
                                            ]
                                            (text <| "Talk to " ++ person.name)
                                    }
                                ]
                            )
                        ]
                )


orientationContainer : List (Attribute msg) -> List (Element msg) -> Element msg
orientationContainer attrs children =
    Element.withOrientation
        (\orientation ->
            case orientation of
                Portrait ->
                    column attrs children

                Landscape ->
                    row attrs children
        )


viewTalking : Data -> { currentDialog : Dialog, currentPerson : Id } -> Element FrontendMsg
viewTalking data { currentDialog, currentPerson } =
    case Dict.get currentPerson data of
        Nothing ->
            text "TODO - MISSING PERSON"

        Just person ->
            viewDialog currentPerson person currentDialog


avatarSize : Element.Length
avatarSize =
    px <| Quantity.multiplyBy 6 rythm


viewDialog : Id -> Person -> Dialog -> Element FrontendMsg
viewDialog currentPerson person { text, choices } =
    column
        [ spacing rythm
        , padding rythm
        , height fill
        , width fill
        , cityBackground person.city
        ]
        [ semiBox [ width fill ] <|
            row
                [ height fill
                , spacing rythm
                ]
                [ avatar person
                , viewMarked [ width fill ] text
                ]
        , choices
            |> List.map (viewChoice currentPerson)
            |> row []
        ]


avatar : { a | image : String, name : String } -> Element msg
avatar person =
    el
        [ width avatarSize
        , height avatarSize
        , Border.width borderWidth
        , Border.rounded rythm
        , Background.image person.image
        , alignTop
        ]
        Element.none


viewChoice : Id -> Choice -> Element FrontendMsg
viewChoice currentPerson { text, next } =
    Input.button [ width fill ]
        { label =
            semiBox
                [ Border.width borderWidth
                , width fill
                ]
                (row [ spacing rythm ]
                    [ avatar { image = "/art/duck.jpg", name = "DUCK" }
                    , viewMarked [ width fill ] text
                    ]
                )
        , onPress =
            Just <|
                case next of
                    NextDialog n ->
                        TalkTo currentPerson n

                    NextViewMap ->
                        ViewMap currentPerson
        }


cityBackground : City -> Attribute FrontendMsg
cityBackground city =
    Element.behindContent <|
        el
            [ width fill
            , height fill
            , Element.htmlAttribute <| Html.Attributes.style "background-image" <| "url('" ++ city.image ++ "')"
            , Element.htmlAttribute <| Html.Attributes.style "background-position" "center"
            , Element.htmlAttribute <| Html.Attributes.style "background-size" "cover"
            ]
            Element.none


viewCityDescription : List (Attribute msg) -> City -> Element msg
viewCityDescription attrs city =
    semiBox attrs
        (column
            [ spacing rythm
            , Font.center
            ]
            [ el [ centerX, Font.bold ] <| text city.name
            , viewMarked [ centerX ] city.text
            ]
        )


semiBox : List (Attribute msg) -> Element msg -> Element msg
semiBox attrs =
    el
        ([ padding rythm
         , Border.rounded rythm
         , Background.color Theme.colors.semitransparent
         ]
            ++ attrs
        )
