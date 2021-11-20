module Frontend.Game exposing (view)

import Angle
import Dict
import Element.WithUnits as Element exposing (Attribute, Element, Orientation(..), alignTop, centerX, centerY, column, el, fill, height, image, inFront, padding, paragraph, px, row, shrink, spacing, text, width, wrappedRow)
import Element.WithUnits.Background as Background
import Element.WithUnits.Border as Border
import Element.WithUnits.Font as Font
import Element.WithUnits.Input as Input
import Frontend.Common
import Html
import Html.Attributes
import Length exposing (Length)
import Markdown.Block exposing (ListItem(..), Task(..))
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import Model exposing (Choice, City, Data, Dialog, Id, Next(..), Person, Quiz)
import Pins exposing (mapSize)
import Quantity
import Set
import Theme
import Types exposing (GameModel(..), GameMsg(..), OuterGameModel(..), SharedGameModel)


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


viewMap : Data -> SharedGameModel -> Element GameMsg
viewMap data { currentPerson, tickets } =
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
                |> List.filter (\( personId, _ ) -> Set.member personId tickets)
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
            |> (\( h, t ) -> h :: t)
            |> List.map viewChoice
            |> wrappedRow [ width fill, spacing rythm ]
        ]


viewQuizzing : Person -> Quiz -> Element GameMsg
viewQuizzing person ({ question, correctAnswer, wrongAnswers } as quiz) =
    column
        [ spacing rythm
        , padding rythm
        , height fill
        , width fill
        , cityBackground person.city
        , Font.size baseFontSize
        ]
        [ semiBox [ width fill ] <|
            el
                [ Font.center
                , Font.bold
                , width fill
                , Font.size <| Quantity.multiplyBy 2 baseFontSize
                ]
                (text "QUIZ TIME!")
        , semiBox [ width fill ] <|
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
            |> List.map (viewQuizAnswer quiz)
            |> wrappedRow [ width fill, spacing rythm ]
        ]


duckPerson : { image : String, name : String }
duckPerson =
    { image = "/art/duck.jpg"
    , name = "DUCK"
    }


viewQuizAnswer : Quiz -> String -> Element GameMsg
viewQuizAnswer quiz answer =
    let
        next =
            if answer == quiz.correctAnswer then
                { text = quiz.messageIfCorrect
                , choices =
                    ( { text = "Thank you!"
                      , next = NextGiveTicket
                      }
                    , []
                    )
                }

            else
                { text = quiz.messageIfWrong
                , choices =
                    ( { text = "Let me try again!"
                      , next = NextQuiz quiz
                      }
                    , []
                    )
                }
    in
    Input.button [ width fill ]
        { label = viewDialogLine duckPerson answer
        , onPress = Just <| ViewDialog next
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

                    NextRandomQuiz ->
                        PickQuiz

                    NextQuiz quiz ->
                        ViewQuiz quiz

                    NextGiveTicket ->
                        GiveTicketAndViewMap
        }


viewDialogLine : { a | image : String, name : String } -> String -> Element msg
viewDialogLine personIsh text =
    semiBox
        [ Border.width borderWidth
        , width fill
        ]
        (row [ spacing rythm, width fill ]
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



-- Markdown renderer


elmUiRenderer : Markdown.Renderer.Renderer (Element msg)
elmUiRenderer =
    { heading = heading
    , paragraph = paragraph [ spacing rythm ]
    , thematicBreak = Element.none
    , text = Element.text
    , strong = \content -> Element.row [ Font.bold ] content
    , emphasis = \content -> Element.row [ Font.italic ] content
    , strikethrough = \content -> Element.row [ Font.strike ] content
    , codeSpan = code
    , link =
        \{ destination } body ->
            Element.newTabLink
                [ Element.htmlAttribute (Html.Attributes.style "display" "inline-flex") ]
                { url = destination
                , label =
                    Element.paragraph
                        [ Font.color (Element.rgb255 0 0 255)
                        ]
                        body
                }
    , hardLineBreak = Html.br [] [] |> Element.html
    , image =
        \image ->
            Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }
    , blockQuote =
        \children ->
            Element.column
                [ Border.widthEach
                    { top = Quantity.zero
                    , right = Quantity.zero
                    , bottom = Quantity.zero
                    , left = rythm
                    }
                , padding rythm
                , Border.color (Element.rgb255 145 145 145)
                , Background.color (Element.rgb255 245 245 245)
                ]
                children
    , unorderedList =
        \items ->
            column [ spacing rythm ]
                (items
                    |> List.map
                        (\(ListItem task children) ->
                            row [ spacing rythm ]
                                [ row
                                    [ alignTop ]
                                    ((case task of
                                        IncompleteTask ->
                                            Input.defaultCheckbox False

                                        CompletedTask ->
                                            Input.defaultCheckbox True

                                        NoTask ->
                                            text "•"
                                     )
                                        :: text " "
                                        :: children
                                    )
                                ]
                        )
                )
    , orderedList =
        \startingIndex items ->
            column [ spacing rythm ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            row [ spacing (Quantity.multiplyBy 0.5 rythm) ]
                                [ row [ alignTop ]
                                    (text (String.fromInt (index + startingIndex) ++ " ") :: itemBlocks)
                                ]
                        )
                )
    , codeBlock = codeBlock
    , html = Markdown.Html.oneOf []
    , table = column []
    , tableHeader = column []
    , tableBody = column []
    , tableRow = row []
    , tableHeaderCell = \_ children -> paragraph [] children
    , tableCell = \_ children -> paragraph [] children
    }


code : String -> Element msg
code snippet =
    Element.el
        [ Background.color (Element.rgba 0 0 0 0.04)
        , Border.rounded rythm
        , padding rythm
        , Font.family [ Font.monospace ]
        ]
        (Element.text snippet)


codeBlock : { body : String, language : Maybe String } -> Element msg
codeBlock details =
    Element.el
        [ Background.color (Element.rgba 0 0 0 0.03)
        , Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
        , Element.padding rythm
        , Font.family [ Font.monospace ]
        ]
        (Element.text details.body)


heading : { level : Markdown.Block.HeadingLevel, rawText : String, children : List (Element msg) } -> Element msg
heading { level, rawText, children } =
    Element.paragraph
        [ Font.size
            (case level of
                Markdown.Block.H1 ->
                    Quantity.multiplyBy 1.8 baseFontSize

                Markdown.Block.H2 ->
                    Quantity.multiplyBy 1.2 baseFontSize

                _ ->
                    baseFontSize
            )
        , Font.bold
        , Font.family [ Font.typeface "Montserrat" ]
        , Element.htmlAttribute
            (Html.Attributes.attribute "name" (rawTextToId rawText))
        , Element.htmlAttribute
            (Html.Attributes.id (rawTextToId rawText))
        ]
        children


rawTextToId : String -> String
rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower


viewMarked : List (Attribute msg) -> String -> Element msg
viewMarked attrs input =
    input
        |> String.replace "  " "\n\n"
        |> Markdown.Parser.parse
        |> Result.mapError (\_ -> "Parsing error")
        |> Result.andThen (Markdown.Renderer.render elmUiRenderer)
        |> Result.map (column (spacing rythm :: attrs))
        |> Result.withDefault (text input)
