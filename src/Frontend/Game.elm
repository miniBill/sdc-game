module Frontend.Game exposing (view)

import Dict
import Element.WithContext as Element exposing (Orientation(..), alignLeft, alignRight, alignTop, centerX, centerY, column, el, fill, height, image, inFront, paragraph, px, row, shrink, spacing, text, width, wrappedRow)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Frontend.Common
import Html
import Html.Attributes
import MapPixels exposing (MapLength, MapPixel)
import Markdown.Block exposing (ListItem(..), Task(..))
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import Model exposing (Choice, City, Data, Id, Next(..), Person, Quiz)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity, Rate)
import Set
import Svg as S
import Svg.Attributes as SA
import Theme exposing (Attribute, Element)
import Types exposing (ChatHistory, GameModel(..), GameMsg(..), MapModel, OuterGameModel(..), SharedGameModel, Size, TalkingModel)


view : OuterGameModel -> Element GameMsg
view model =
    case model of
        LoadingData ->
            Frontend.Common.loading

        DataEmpty ->
            text "branch 'DataEmpty' not implemented"

        LoadedData data sharedModel submodel ->
            case Dict.get sharedModel.currentPerson data of
                Nothing ->
                    text "TODO - MISSING PERSON"

                Just person ->
                    case submodel of
                        ViewingMap mapModel ->
                            viewMap data sharedModel mapModel

                        ViewingPerson ->
                            viewPerson person

                        Talking talkingModel ->
                            viewTalking person talkingModel

                        Quizzing quiz ->
                            viewQuizzing person quiz


mapSize :
    { width : MapLength
    , height : MapLength
    }
mapSize =
    { width = MapPixels.pixels 9933
    , height = MapPixels.pixels 7016
    }


scale : Size -> Quantity Float (Rate Pixels MapPixel)
scale size =
    Quantity.max
        (Quantity.per mapSize.width size.width)
        (Quantity.per mapSize.height size.height)


viewMap : Data -> SharedGameModel -> MapModel -> Element GameMsg
viewMap data sharedGameModel _ =
    Element.with .screenSize <| \screen ->
    let
        s =
            scale screen

        w =
            mapSize.width
                |> Quantity.at s

        h =
            mapSize.height
                |> Quantity.at s

        dx =
            screen.width
                |> Quantity.minus w
                |> Quantity.multiplyBy 0

        dy =
            screen.height
                |> Quantity.minus h
                |> Quantity.divideBy 0

        style k v =
            Element.htmlAttribute <| Html.Attributes.style k v

        normalAttrs =
            [ width fill
            , height fill
            , style "background-image" "url(/art/lotr-europe.jpg)"
            , style "background-position"
                (pixelsToString dx ++ " " ++ pixelsToString dy)
            , style "background-size" (pixelsToString w)
            , style "background-repeat" "no-repeat"
            ]

        attrs =
            normalAttrs ++ inFronts

        inFronts =
            data
                |> Dict.toList
                |> List.filter
                    (\( personId, _ ) ->
                        (let
                            _ =
                                Debug.todo
                         in
                         always (not <| String.isEmpty personId)
                        )
                            (Set.member personId sharedGameModel.tickets)
                    )
                |> List.concatMap
                    (\( personId, person ) ->
                        viewPinOnMap
                            sharedGameModel
                            personId
                            person
                    )
                |> List.map inFront

        mapPixelToString q =
            String.fromFloat <| MapPixels.inPixels q

        viewBox =
            [ Quantity.zero, Quantity.zero, mapSize.width, mapSize.height ]
                |> List.map mapPixelToString
                |> String.join " "

        children =
            [ S.image
                [ SA.xlinkHref "/art/lotr-europe.jpg"
                , SA.height <| mapPixelToString mapSize.width
                , SA.height <| mapPixelToString mapSize.height
                ]
                []
            ]
    in
    children
        |> S.svg
            [ SA.viewBox viewBox
            , SA.width <| pixelsToString w
            , SA.height <| pixelsToString h
            ]
        |> Element.html


pixelsToString : Quantity Float Pixels -> String
pixelsToString pixels =
    String.fromFloat (Pixels.inPixels pixels) ++ "px"


viewPinOnMap : SharedGameModel -> Id -> Person -> List (Element GameMsg)
viewPinOnMap sharedGameModel id { city } =
    let
        selected =
            id == sharedGameModel.currentPerson

        radius =
            Theme.rythm * 0.3

        x =
            city.coordinates.x

        y =
            city.coordinates.y

        btn attrs child =
            Input.button
                attrs
                { onPress = Just <| ViewPerson id
                , label = el [] child
                }

        w =
            8 * Theme.rythm
    in
    [ btn
        [ Element.moveDown <| y - radius
        , Element.moveRight <| x - radius
        , Border.width Theme.borderWidth
        , Background.color Theme.colors.delete --<| Element.rgb255 255 0 0
        , Border.rounded <| round radius
        , width <| px <| round <| 2 * radius
        , height <| px <| round <| 2 * radius
        ]
        Element.none
    , el
        [ Element.transparent True
        , Element.moveDown <| y - Theme.rythm / 2
        , Element.moveRight <|
            if city.showNameOnTheRightInTheMap then
                x + 2 * radius

            else
                x - (w + 2 * radius)
        , width <| px w
        , Element.htmlAttribute <| Html.Attributes.style "pointer-events" "none"
        ]
      <|
        btn
            [ if city.showNameOnTheRightInTheMap then
                alignLeft

              else
                alignRight
            , if selected then
                Font.bold

              else
                Font.regular
            , Element.htmlAttribute <|
                Html.Attributes.style "text-shadow" <|
                    String.join "," <|
                        List.map
                            (\( dx, dy, b ) ->
                                String.join " "
                                    [ pixelsToString dx
                                    , pixelsToString dy
                                    , pixelsToString b
                                    , "#ffffff"
                                    ]
                            )
                            [ ( Pixels.pixels -2, Pixels.pixels -2, Pixels.pixels 0.1 )
                            , ( Pixels.pixels 2, Pixels.pixels -2, Pixels.pixels 0.1 )
                            , ( Pixels.pixels -2, Pixels.pixels 2, Pixels.pixels 0.1 )
                            , ( Pixels.pixels 2, Pixels.pixels 2, Pixels.pixels 0.1 )
                            ]
            , Element.htmlAttribute <| Html.Attributes.style "pointer-events" "initial"
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
                    , Theme.spacing
                    , width fill
                    ]
                    [ avatar
                        [ width fill
                        , Element.htmlAttribute <| Html.Attributes.style "height" "100%"
                        ]
                        person
                    , Input.button [ centerX, centerY ]
                        { onPress = Just <| ViewDialog person.dialog []
                        , label =
                            semiBox
                                [ Border.width Theme.borderWidth
                                , width fill
                                ]
                                (text <| "Talk to " ++ person.name)
                        }
                    ]
                )
    in
    Element.with (.screenSize >> getOrientation)
        (\orientation ->
            orientationContainer
                [ Theme.padding
                , Theme.spacing
                , width fill
                , height fill
                , cityBackground person.city
                ]
                [ leftBox orientation
                , rightBox orientation
                ]
        )


getOrientation : Size -> Orientation
getOrientation screen =
    if screen.width |> Quantity.lessThan screen.height then
        Portrait

    else
        Landscape


orientationContainer : List (Attribute msg) -> List (Element msg) -> Element msg
orientationContainer attrs children =
    Element.with (.screenSize >> getOrientation)
        (\orientation ->
            case orientation of
                Portrait ->
                    column attrs children

                Landscape ->
                    row attrs children
        )


avatarSize : Element.Length
avatarSize =
    px <| 6 * Theme.rythm


viewTalking : Person -> TalkingModel -> Element GameMsg
viewTalking person { chatHistory, currentDialog } =
    let
        history =
            List.map
                (\( p, t ) ->
                    viewDialogLine True
                        (Maybe.withDefault duckPerson p)
                        t
                )
                (List.reverse chatHistory)

        current =
            [ viewDialogLine False person currentDialog.text
            , currentDialog.choices
                |> (\( h, t ) -> h :: t)
                |> List.map
                    (viewChoice
                        (( Just
                            { name = person.name
                            , image = person.image
                            }
                         , currentDialog.text
                         )
                            :: chatHistory
                        )
                    )
                |> wrappedRow [ width fill, Theme.spacing ]
            ]
    in
    column
        [ Theme.spacing
        , Theme.padding
        , height fill
        , width fill
        , cityBackground person.city
        ]
        (history ++ current)


viewQuizzing : Person -> Quiz -> Element GameMsg
viewQuizzing person ({ question, correctAnswer, wrongAnswers } as quiz) =
    column
        [ Theme.spacing
        , Theme.padding
        , height fill
        , width fill
        , cityBackground person.city
        ]
        [ semiBox [ width fill ] <|
            el
                [ Font.center
                , Font.bold
                , width fill
                , Theme.fontSizes.big
                ]
                (text "QUIZ TIME!")
        , semiBox [ width fill ] <|
            row
                [ height fill
                , Theme.spacing
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
            |> wrappedRow [ width fill, Theme.spacing ]
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
        { label = viewDialogLine False duckPerson answer
        , onPress = Just <| ViewDialog next []
        }


avatar : List (Attribute msg) -> { a | image : String, name : String } -> Element msg
avatar attrs person =
    el
        ([ Border.width Theme.borderWidth
         , Border.rounded Theme.rythm
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


viewChoice : ChatHistory -> Choice -> Element GameMsg
viewChoice chatHistory { text, next } =
    Input.button [ width fill ]
        { label = viewDialogLine False duckPerson text
        , onPress =
            Just <|
                case next of
                    NextDialog n ->
                        ViewDialog n (( Nothing, text ) :: chatHistory)

                    NextViewMap ->
                        ViewMap

                    NextRandomQuiz ->
                        PickQuiz

                    NextQuiz quiz ->
                        ViewQuiz quiz

                    NextGiveTicket ->
                        GiveTicketAndViewMap
        }


viewDialogLine : Bool -> { a | image : String, name : String } -> String -> Element msg
viewDialogLine historical personIsh text =
    semiBox
        [ Border.width Theme.borderWidth
        , width fill
        , if historical then
            Background.color (Element.rgba 0.6 0.6 0.6 0.6)

          else
            Background.color (Element.rgba 1 1 1 0.8)
        ]
        (row [ Theme.spacing, width fill ]
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
            [ Theme.spacing
            , Font.center
            ]
            [ el [ centerX, Font.bold ] <| text city.name
            , viewMarked [ centerX ] city.text
            ]
        )


semiBox : List (Attribute msg) -> Element msg -> Element msg
semiBox attrs =
    el
        ([ Theme.padding
         , Border.rounded Theme.rythm
         , Background.color Theme.colors.semitransparent
         ]
            ++ attrs
        )



-- Markdown renderer


elmUiRenderer : Markdown.Renderer.Renderer (Element msg)
elmUiRenderer =
    { heading = heading
    , paragraph = paragraph [ Theme.spacing ]
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
                    { top = 0
                    , right = 0
                    , bottom = 0
                    , left = Theme.rythm
                    }
                , Theme.padding
                , Border.color (Element.rgb255 145 145 145)
                , Background.color (Element.rgb255 245 245 245)
                ]
                children
    , unorderedList =
        \items ->
            column [ Theme.spacing ]
                (items
                    |> List.map
                        (\(ListItem task children) ->
                            row [ Theme.spacing ]
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
            column [ Theme.spacing ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            row [ spacing (Theme.rythm // 2) ]
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
        , Border.rounded Theme.rythm
        , Theme.padding
        , Font.family [ Font.monospace ]
        ]
        (Element.text snippet)


codeBlock : { body : String, language : Maybe String } -> Element msg
codeBlock details =
    Element.el
        [ Background.color (Element.rgba 0 0 0 0.03)
        , Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
        , Theme.padding
        , Font.family [ Font.monospace ]
        ]
        (Element.text details.body)


heading : { level : Markdown.Block.HeadingLevel, rawText : String, children : List (Element msg) } -> Element msg
heading { level, rawText, children } =
    Element.paragraph
        [ Font.size
            (case level of
                Markdown.Block.H1 ->
                    18 * Theme.fontSize // 10

                Markdown.Block.H2 ->
                    12 * Theme.fontSize // 10

                _ ->
                    Theme.fontSize
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
        |> Result.map (column (Theme.spacing :: attrs))
        |> Result.withDefault (text input)
