module Frontend.Game exposing (view)

import Angle
import Dict
import Element.WithUnits as Element exposing (Attribute, Element, Orientation(..), alignTop, centerX, centerY, column, el, fill, height, image, inFront, padding, px, row, shrink, spacing, text, width, wrappedRow)
import Element.WithUnits.Background as Background
import Element.WithUnits.Border as Border
import Element.WithUnits.Font as Font
import Element.WithUnits.Input as Input
import Frontend.Common exposing (viewMarked)
import Html.Attributes
import Length exposing (Length)
import Model exposing (Choice, City, Data, Dialog, Id, Next(..), Person, Quiz)
import Pins exposing (mapSize)
import Quantity
import Theme
import Types exposing (FrontendMsg(..), GameModel(..), GameMsg(..), OuterGameModel(..))


rythm : Length
rythm =
    Length.millimeters 40


baseFontSize : Length
baseFontSize =
    Length.millimeters 100


borderWidth : Length
borderWidth =
    Length.millimeters 3


view : OuterGameModel -> Element GameMsg
view model =
    case model of
        LoadingData ->
            Element.element Frontend.Common.loading

        DataEmpty ->
            text "branch 'DataEmpty' not implemented"

        LoadedData data sharedModel submodel ->
            case Dict.get sharedModel.currentPerson data of
                Nothing ->
                    text "TODO - MISSING PERSON"

                Just person ->
                    case submodel of
                        ViewingMap ->
                            viewMap data sharedModel

                        ViewingPerson ->
                            viewPerson person

                        Talking talkingModel ->
                            viewTalking person talkingModel

                        Quizzing quiz ->
                            viewQuizzing person quiz


viewMap : Data -> { currentPerson : Id } -> Element GameMsg
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


viewPinOnMap : Bool -> Id -> Person -> List (Element GameMsg)
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


viewPerson : Person -> Element GameMsg
viewPerson person =
    let
        leftBox orientation =
            viewCityDescription
                (if orientation == Portrait then
                    [ width fill ]

                 else
                    [ width fill
                    , height fill
                    ]
                )
                person.city

        rightBox orientation =
            semiBox
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
                    [ avatar
                        [ width fill
                        , Element.htmlAttribute <| Html.Attributes.style "height" "100%"
                        ]
                        person
                    , Input.button [ centerX, centerY ]
                        { onPress = Just <| ViewDialog person.dialog
                        , label =
                            semiBox
                                [ Border.width borderWidth
                                , width fill
                                ]
                                (text <| "Talk to " ++ person.name)
                        }
                    ]
                )
    in
    Element.withOrientation
        (\orientation ->
            orientationContainer
                [ padding rythm
                , spacing rythm
                , width fill
                , height fill
                , cityBackground person.city
                , Font.size baseFontSize
                ]
                [ leftBox orientation
                , rightBox orientation
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


avatarSize : Element.Length
avatarSize =
    px <| Quantity.multiplyBy 6 rythm


viewTalking : Person -> { currentDialog : Dialog } -> Element GameMsg
viewTalking person { currentDialog } =
    column
        [ spacing rythm
        , padding rythm
        , height fill
        , width fill
        , cityBackground person.city
        , Font.size baseFontSize
        ]
        [ viewDialogLine person currentDialog.text
        , currentDialog.choices
            |> List.map viewChoice
            |> wrappedRow [ spacing rythm ]
        ]


viewQuizzing : Person -> Quiz -> Element GameMsg
viewQuizzing person { question, correctAnswer, wrongAnswers } =
    column
        [ spacing rythm
        , padding rythm
        , height fill
        , width fill
        , cityBackground person.city
        , Font.size baseFontSize
        ]
        [ semiBox [ width fill ] <|
            row
                [ height fill
                , spacing rythm
                ]
                [ avatar
                    [ width avatarSize
                    , height avatarSize
                    ]
                    person
                , viewMarked [ width fill ] question
                ]
        , (correctAnswer :: wrongAnswers)
            |> List.sort
            |> List.map (viewQuizAnswer correctAnswer)
            |> wrappedRow [ spacing rythm ]
        ]


duckPerson : { image : String, name : String }
duckPerson =
    { image = "/art/duck.jpg"
    , name = "DUCK"
    }


viewQuizAnswer : String -> String -> Element GameMsg
viewQuizAnswer correctAnswer answer =
    Input.button [ width fill ]
        { label = viewDialogLine duckPerson answer
        , onPress =
            Just <|
                if answer == correctAnswer then
                    GaveCorrectAnswer

                else
                    GaveWrongAnswer
        }


avatar : List (Attribute msg) -> { a | image : String, name : String } -> Element msg
avatar attrs person =
    el
        ([ Border.width borderWidth
         , Border.rounded rythm
         , Background.image person.image
         , alignTop
         ]
            ++ attrs
        )
        (image (attrs ++ [ Element.transparent True ])
            { src = person.image
            , description = person.name ++ "'s avatar"
            }
        )


viewChoice : Choice -> Element GameMsg
viewChoice { text, next } =
    Input.button [ width fill ]
        { label = viewDialogLine duckPerson text
        , onPress =
            Just <|
                case next of
                    NextDialog n ->
                        ViewDialog n

                    NextViewMap ->
                        ViewMap

                    NextQuiz ->
                        PickQuiz
        }


viewDialogLine : { a | image : String, name : String } -> String -> Element msg
viewDialogLine personIsh text =
    semiBox
        [ Border.width borderWidth
        , width fill
        ]
        (row [ spacing rythm ]
            [ avatar
                [ width avatarSize
                , height avatarSize
                ]
                personIsh
            , viewMarked [ width fill ] text
            ]
        )


cityBackground : City -> Attribute msg
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
