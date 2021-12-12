module Frontend.Game exposing (view)

import Dict
import Element.WithContext as Element exposing (Orientation(..), alignBottom, alignTop, centerX, centerY, column, el, fill, height, image, paragraph, px, row, text, width, wrappedRow)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Frontend.Common
import Frontend.GameTheme as Theme exposing (Attribute, Element)
import Html
import Html.Attributes
import MapPixels
import Markdown.Block exposing (ListItem(..), Task(..))
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import Model exposing (ChatHistory, Choice, City, Data, GameModel(..), Id, MapModel, MenuModel, Next(..), Person, Quiz, SharedGameModel, TalkingModel, mapSize)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Set
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Types exposing (GameMsg(..), OuterGameModel(..), Size)


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
                    el
                        [ width fill
                        , height fill
                        , Theme.fontSize 1
                        ]
                        (case submodel of
                            ViewingMap mapModel ->
                                viewMap data sharedModel mapModel

                            ViewingPerson ->
                                viewPerson person

                            ViewingTalking talkingModel ->
                                viewTalking sharedModel person talkingModel

                            Quizzing quiz ->
                                viewQuizzing person quiz

                            ViewingMenu previous ->
                                viewMenu previous
                        )


viewMenu : MenuModel -> Element GameMsg
viewMenu { previous, background } =
    let
        btn msg label =
            Input.button
                [ width fill ]
                { onPress = Just msg
                , label =
                    semiBox
                        [ width fill
                        , Theme.borderWidth
                        , Theme.borderRounded
                        , Theme.padding
                        ]
                        (text label)
                }
    in
    el (mainContainerAttrs { image = background }) <|
        column [ width fill, height fill, Theme.spacing ]
            [ let
                _ =
                    Debug.todo
              in
              btn Reset "RESET"
            , menuRow (BackTo previous) "Back"
            ]


viewMap : Data -> SharedGameModel -> MapModel -> Element GameMsg
viewMap data sharedGameModel _ =
    Element.with .screenSize <| \screen ->
    let
        s =
            Quantity.max
                (Quantity.per mapSize.width screen.width)
                (Quantity.per mapSize.height screen.height)

        w =
            mapSize.width
                |> Quantity.at s

        h =
            mapSize.height
                |> Quantity.at s

        pins =
            data
                |> Dict.toList
                |> List.filter
                    (\( personId, _ ) ->
                        Set.member personId sharedGameModel.tickets
                    )
                |> List.sortBy (\( personId, _ ) -> boolToInt <| personId == sharedGameModel.currentPerson)
                |> List.concatMap
                    (\( personId, person ) ->
                        viewPinOnMap
                            sharedGameModel
                            personId
                            person
                    )

        mapPixelToString q =
            String.fromFloat <| MapPixels.inPixels q

        viewBox =
            [ Quantity.zero, Quantity.zero, mapSize.width, mapSize.height ]
                |> List.map mapPixelToString
                |> String.join " "

        children =
            S.image
                [ SA.xlinkHref "/art/europe.jpg"
                , SA.width <| mapPixelToString mapSize.width
                , SA.height <| mapPixelToString mapSize.height
                ]
                []
                :: S.rect
                    [ SA.width <| mapPixelToString mapSize.width
                    , SA.height <| mapPixelToString mapSize.height
                    , SA.fillOpacity "0.15"
                    ]
                    []
                :: pins
    in
    children
        |> S.svg
            [ SA.viewBox viewBox
            , SA.width <| pixelsToString w
            , SA.height <| pixelsToString h
            ]
        |> Element.html


boolToInt : Bool -> number
boolToInt b =
    if b then
        1

    else
        0


pixelsToString : Quantity Float Pixels -> String
pixelsToString pixels =
    String.fromFloat (Pixels.inPixels pixels) ++ "px"


viewPinOnMap : SharedGameModel -> Id -> Person -> List (Svg GameMsg)
viewPinOnMap sharedGameModel id { city } =
    let
        selected =
            id == sharedGameModel.currentPerson

        radius =
            MapPixels.inPixels mapSize.width * 0.006

        common k =
            [ SA.cy <| String.fromFloat city.coordinates.y
            , SA.cx <| String.fromFloat city.coordinates.x
            , SE.onClick <| ViewPerson id
            , SA.cursor "pointer"
            , SA.r <| String.fromFloat <| k * radius
            ]

        duckRadius =
            radius * 2
    in
    [ S.circle (SA.fill "black" :: common 1) []
    , S.circle (SA.fill "white" :: common 0.8) []
    , S.g [] <|
        if selected then
            [ S.circle (SA.fill "red" :: common 2.3) []
            , S.image
                [ SA.x <| String.fromFloat <| city.coordinates.x - duckRadius
                , SA.y <| String.fromFloat <| city.coordinates.y - duckRadius
                , SA.width <| String.fromFloat <| duckRadius * 2
                , SA.height <| String.fromFloat <| duckRadius * 2
                , SA.xlinkHref "/art/duckon.webp"
                ]
                []
            ]

        else
            []
    ]


viewPerson : Person -> Element GameMsg
viewPerson person =
    let
        style k v =
            Element.htmlAttribute <| Html.Attributes.style k v

        avatarBox =
            Input.button [ width fill, height fill ]
                { onPress = Just <| ViewTalking person.dialog []
                , label =
                    semiBox [ width fill, height fill ]
                        (column
                            [ width fill
                            , height fill
                            , Font.center
                            , Theme.spacing
                            , Theme.padding
                            ]
                            [ el
                                [ Theme.borderRounded
                                , width fill
                                , height fill
                                , style "background-image" <| "url(\"" ++ person.image ++ "\")"
                                , style "background-size" "contain"
                                , style "background-repeat" "no-repeat"
                                , style "background-position" "center"
                                ]
                                Element.none
                            , el [ width fill, Font.center ] <|
                                (text <| "Talk to " ++ person.name)
                            ]
                        )
                }
    in
    Element.with (.screenSize >> getOrientation)
        (\orientation ->
            case orientation of
                Portrait ->
                    Element.column (mainContainerAttrs person.city)
                        [ viewCityDescription [ width fill ] person.city
                        , avatarBox
                        ]

                Landscape ->
                    Element.row (mainContainerAttrs person.city)
                        [ viewCityDescription [] person.city
                        , avatarBox
                        ]
        )


getOrientation : Size -> Orientation
getOrientation screen =
    if screen.width |> Quantity.lessThan screen.height then
        Portrait

    else
        Landscape


mainContainerAttrs : { a | image : String } -> List (Attribute msg)
mainContainerAttrs city =
    [ Theme.spacing
    , Theme.padding
    , height fill
    , width fill
    , cityBackground city
    ]


viewTalking : SharedGameModel -> Person -> TalkingModel -> Element GameMsg
viewTalking { currentPerson } person { chatHistory, currentDialog } =
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

        menu =
            menuRow
                (ViewMenu { background = person.city.image })
                (if String.isEmpty currentPerson then
                    "Menu"

                 else
                    ""
                )
    in
    column
        (mainContainerAttrs person.city)
        (history ++ current ++ [ menu ])


menuRow : GameMsg -> String -> Element GameMsg
menuRow msg label =
    Input.button
        [ alignBottom
        , width fill
        ]
        { onPress = Just msg
        , label =
            row
                [ Theme.spacing
                , width fill
                , Theme.padding
                ]
                [ avatar 1 { image = "/art/sdc.jpg", name = "" }
                , if String.isEmpty label then
                    Element.none

                  else
                    semiBox
                        [ Theme.borderWidth
                        , Theme.paddingXYWithCoeff 2 1
                        , height fill
                        , width fill
                        ]
                        (el [ centerY ] <| text label)
                ]
        }


viewQuizzing : Person -> Quiz -> Element GameMsg
viewQuizzing person ({ question, correctAnswer, wrongAnswers } as quiz) =
    column
        (mainContainerAttrs person.city)
        [ semiBox [ width fill ] <|
            el
                [ Font.center
                , Font.bold
                , width fill
                , Theme.fontSize 3
                ]
                (text "QUIZ TIME!")
        , semiBox [ width fill ] <|
            row
                [ height fill
                , Theme.spacing
                ]
                [ avatar 1 person
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
        , onPress = Just <| ViewTalking next []
        }


avatar : Float -> { a | image : String, name : String } -> Element msg
avatar scale person =
    let
        size =
            [ Theme.autoscalingI (scale * 200) (width << px)
            , Theme.autoscalingI (scale * 200) (height << px)
            ]
    in
    el
        ([ Theme.borderWidth
         , Theme.borderRounded
         , Background.image person.image
         , alignTop
         ]
            ++ size
        )
        (image (Element.transparent True :: size)
            { src = person.image
            , description =
                if String.isEmpty person.name then
                    "Menu"

                else
                    person.name ++ "'s avatar"
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
                        ViewTalking n (( Nothing, text ) :: chatHistory)

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
    row [ Theme.spacing, width fill ]
        [ avatar 1 personIsh
        , semiBox
            [ Theme.borderWidth
            , width fill
            , height fill
            , Theme.paddingXYWithCoeff 4 1
            , if historical then
                Background.color (Element.rgba 0.6 0.6 0.6 0.6)

              else
                Background.color (Element.rgba 1 1 1 0.8)
            ]
            (viewMarked [ centerY, width fill ] text)
        ]


cityBackground : { a | image : String } -> Attribute msg
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
            , Theme.padding
            , Font.center
            , width fill
            ]
            [ el [ Font.center, width fill, Font.bold ] <| text city.name
            , viewMarked [ centerX ] city.text
            ]
        )


semiBox : List (Attribute msg) -> Element msg -> Element msg
semiBox attrs =
    el
        ([ Theme.padding
         , Theme.borderRounded
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
                [ Theme.borderWidthEach
                    { top = 0
                    , right = 0
                    , bottom = 0
                    , left = 1
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
                            row [ Theme.spacing ]
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
        , Theme.borderRounded
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


heading :
    { level : Markdown.Block.HeadingLevel
    , rawText : String
    , children : List (Element msg)
    }
    -> Element msg
heading { level, rawText, children } =
    Element.paragraph
        [ case level of
            Markdown.Block.H1 ->
                Theme.fontSize 1.8

            Markdown.Block.H2 ->
                Theme.fontSize 1.2

            _ ->
                Theme.fontSize 1
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
