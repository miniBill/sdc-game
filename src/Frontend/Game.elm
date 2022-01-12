module Frontend.Game exposing (view)

import Dict
import Element.WithContext as Element exposing (Orientation(..), alignBottom, alignTop, centerX, centerY, column, el, fill, fillPortion, height, paragraph, px, row, text, width, wrappedRow)
import Element.WithContext.Background as Background
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Env
import Frontend.Common
import Frontend.GameMarkdown
import Frontend.GameTheme as Theme exposing (Attribute, Element)
import Html.Attributes
import MapPixels
import Markdown.Parser
import Markdown.Renderer
import Model exposing (A11yOptions, ChatHistory, Choice, City, Data, GameModel(..), Id, MapModel, MenuModel, Next(..), Person, Quiz, SharedGameModel, TalkingModel, mapSize)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Set
import SoundLibrary
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Types exposing (AudioModel, AudioMsg(..), GameMsg(..), GameMsgTuple, OuterGameModel(..), Size, TrackKind(..))


view : AudioModel -> OuterGameModel -> Element GameMsgTuple
view audioModel model =
    case model of
        LoadingData ->
            Frontend.Common.loading

        DataEmpty ->
            text "Loading failed :("

        LoadedData data sharedModel submodel ->
            case Dict.get sharedModel.currentPerson data of
                Nothing ->
                    text "There is a problem with the game data or save file :("

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
                                viewMenu audioModel previous
                        )


viewMenu : AudioModel -> MenuModel -> Element GameMsgTuple
viewMenu { mainVolume, musicVolume, effectsVolume } { previous, background } =
    Element.with .a11y <| \a11y ->
    let
        container attrs =
            semiBox
                ([ width fill
                 , Theme.borderWidth
                 , Theme.borderRounded
                 , Theme.padding
                 ]
                    ++ attrs
                )

        viewSegment attrs index segmentsCount { active, label, onPress } =
            Input.button
                ([ Theme.padding
                 , width fill
                 , Font.center
                 , { topLeft = index == 0
                   , topRight = index == segmentsCount - 1
                   , bottomLeft = index == 0
                   , bottomRight = index == segmentsCount - 1
                   }
                    |> Theme.borderRoundedEachWithCoeff
                 , Theme.borderWidthEach { top = True, bottom = True, right = True, left = index == 0 }
                 , if active then
                    Background.color Theme.colors.selectedTab

                   else
                    Theme.semitransparentBackground
                 ]
                    ++ attrs
                )
                { label = text label
                , onPress = Just onPress
                }

        menuRow : List (Attribute msg) -> String -> List (Segment msg) -> Element msg
        menuRow attrs label segments =
            let
                segmentsCount =
                    List.length segments
            in
            [ el [ width <| fillPortion 3 ] <| text label
            , segments
                |> List.indexedMap
                    (\index (Segment sattrs segment) ->
                        viewSegment
                            sattrs
                            index
                            segmentsCount
                            segment
                    )
                |> row [ width <| fillPortion 2 ]
            ]
                |> wrappedRow [ width fill, Theme.spacing ]
                |> container attrs

        toggle attrs label getter toMsg =
            let
                value =
                    getter a11y
            in
            menuRow attrs
                label
                [ Segment [] { active = not value, label = "No", onPress = False }
                , Segment [] { active = value, label = "Yes", onPress = True }
                ]
                |> Element.map (\v -> ( A11y <| toMsg v, Just <| AudioPlay SoundLibrary.click False Effect ))

        volumeRow label value toMsg =
            menuRow []
                (label ++ " (" ++ String.fromInt (round <| value * 100) ++ "%)")
                [ Segment []
                    { active = value == 0
                    , label = "Mute"
                    , onPress = 0
                    }
                , Segment []
                    { active = False
                    , label = "-"
                    , onPress = max 0 <| value - 0.1
                    }
                , Segment []
                    { active = False
                    , label = "+"
                    , onPress = min 1 <| value + 0.1
                    }
                , Segment []
                    { active = value == 1
                    , label = "Max"
                    , onPress = 1
                    }
                ]
                |> Element.map
                    (\volume ->
                        ( toMsg volume
                        , Just <| AudioPlay SoundLibrary.click False Effect
                        )
                    )
    in
    el (mainContainerAttrs { image = background }) <|
        column [ width fill, height fill, Theme.spacing ]
            [ toggle [ Background.color <| Element.rgb 1 1 1 ]
                "Opaque backgrounds"
                .opaqueBackgrounds
                (\newValue -> { a11y | opaqueBackgrounds = newValue })
            , toggle []
                "Allow free travel"
                .unlockEverything
                (\newValue -> { a11y | unlockEverything = newValue })
            , toggle []
                "Use Open Dyslexic"
                .openDyslexic
                (\newValue -> { a11y | openDyslexic = newValue })
            , menuRow []
                ("Scale (" ++ String.fromInt (round <| a11y.fontSize / Theme.defaultFontSize * 100) ++ "%)")
                [ Segment []
                    { active = a11y.fontSize < Theme.defaultFontSize
                    , label = "-"
                    , onPress = a11y.fontSize * 10 / 11
                    }
                , Segment []
                    { active = a11y.fontSize == Theme.defaultFontSize
                    , label = "100%"
                    , onPress = Theme.defaultFontSize
                    }
                , Segment []
                    { active = a11y.fontSize > Theme.defaultFontSize
                    , label = "+"
                    , onPress = a11y.fontSize * 11 / 10
                    }
                ]
                |> Element.map
                    (\fontSize ->
                        ( A11y { a11y | fontSize = toFloat <| round fontSize }
                        , Just <| AudioPlay SoundLibrary.click False Effect
                        )
                    )
            , volumeRow "Main Volume" mainVolume MainVolume
            , volumeRow "Music Volume" musicVolume MusicVolume
            , volumeRow "Effects Volume" effectsVolume EffectsVolume
            , menuRow []
                "Reset save"
                [ Segment [ Background.color <| Element.rgb 1 0.7 0.7 ]
                    { active = False
                    , label = "RESET"
                    , onPress =
                        ( Reset
                        , Just <| AudioPlay SoundLibrary.click False Effect
                        )
                    }
                ]
            , menuButtonAndLabel (BackTo previous) "Back"
            ]


type Segment msg
    = Segment (List (Attribute msg)) { active : Bool, label : String, onPress : msg }


viewMap : Data -> SharedGameModel -> MapModel -> Element GameMsgTuple
viewMap data sharedGameModel mapModel =
    Element.with identity <| \{ screenSize, a11y } ->
    let
        s =
            Quantity.max
                (Quantity.per mapSize.width screenSize.width)
                (Quantity.per mapSize.height screenSize.height)

        w =
            mapSize.width
                |> Quantity.at s

        h =
            mapSize.height
                |> Quantity.at s

        ( pins, ( innerRings, outerRings ) ) =
            data
                |> Dict.toList
                |> List.filter
                    (\( personId, _ ) ->
                        a11y.unlockEverything || Set.member personId sharedGameModel.tickets
                    )
                |> List.sortBy (\( personId, _ ) -> boolToInt <| personId == sharedGameModel.currentPerson)
                |> List.map
                    (\( personId, person ) ->
                        viewPinOnMap sharedGameModel a11y mapModel personId person
                    )
                |> List.unzip
                |> Tuple.mapSecond List.unzip

        duck =
            case Dict.get sharedGameModel.currentPerson data of
                Nothing ->
                    []

                Just person ->
                    viewDuckOnMap data mapModel person

        mapPixelToString q =
            String.fromFloat <| MapPixels.inPixels q

        viewBox =
            [ Quantity.zero, Quantity.zero, mapSize.width, mapSize.height ]
                |> List.map mapPixelToString
                |> String.join " "

        map =
            [ S.image
                [ Theme.imageXlinkHref "europe.webp"
                , SA.width <| mapPixelToString mapSize.width
                , SA.height <| mapPixelToString mapSize.height
                ]
                []
            , S.rect
                [ SA.width <| mapPixelToString mapSize.width
                , SA.height <| mapPixelToString mapSize.height
                , SA.fillOpacity "0.10"
                ]
                []
            ]

        children =
            map
                ++ List.concat outerRings
                ++ List.concat innerRings
                ++ List.concat pins
                ++ duck

        menu =
            el
                [ Theme.padding
                , Element.htmlAttribute <| Html.Attributes.style "position" "fixed"
                , Element.htmlAttribute <| Html.Attributes.style "bottom" "0px"
                ]
                (menuButtonAndLabel
                    (ViewMenu { background = "europe.webp" })
                    ""
                )
    in
    children
        |> S.svg
            [ SA.viewBox viewBox
            , SA.width <| pixelsToString w
            , SA.height <| pixelsToString h
            ]
        |> Element.html
        |> el [ Element.inFront menu ]


boolToInt : Bool -> number
boolToInt b =
    if b then
        1

    else
        0


pixelsToString : Quantity Float Pixels -> String
pixelsToString pixels =
    String.fromFloat (Pixels.inPixels pixels) ++ "px"


viewPinOnMap :
    SharedGameModel
    -> A11yOptions
    -> MapModel
    -> Id
    -> Person
    -> ( List (Svg GameMsgTuple), ( List (Svg GameMsgTuple), List (Svg GameMsgTuple) ) )
viewPinOnMap sharedGameModel a11y mapModel id { city } =
    let
        radius =
            MapPixels.inPixels mapSize.width * 0.006

        fill =
            if Set.member id sharedGameModel.usedTickets then
                if a11y.unlockEverything then
                    "lightgray"

                else
                    "gray"

            else
                "white"

        disabled =
            (mapModel.travellingTo /= Nothing)
                || (Set.member id sharedGameModel.usedTickets && not a11y.unlockEverything)

        handler pointer =
            if disabled then
                []

            else if pointer then
                [ SA.cursor "pointer"
                , SE.onClick ( TravellingTo 0 id, Nothing )
                ]

            else
                [ SE.onClick ( TravellingTo 0 id, Nothing )
                ]

        inner =
            1

        outer =
            2.5

        cy =
            String.fromFloat city.coordinates.y

        cx =
            String.fromFloat city.coordinates.x

        ring attrs pointer fl sz =
            S.circle
                ([ SA.fill fl
                 , SA.cy cy
                 , SA.cx cx
                 , SA.r <| String.fromFloat <| sz * radius
                 ]
                    ++ attrs
                    ++ handler pointer
                )
                []
    in
    ( [ if not disabled then
            ring
                [ [ ( "animation-duration", "2s" )
                  , ( "animation-name", "pulse" )
                  , ( "animation-iteration-count", "infinite" )
                  , ( "transform-origin", "center" )
                  , ( "stroke-width", String.fromFloat <| 0.1 * radius )
                  , ( "stroke", "black" )
                  ]
                    |> List.map (\( k, v ) -> k ++ ":" ++ v)
                    |> String.join ";"
                    |> SA.style
                , SA.stroke "black"
                ]
                True
                "transparent"
                inner

        else
            S.g [] []
      , ring [] True "black" inner
      , ring [] True fill (inner - 0.2)
      ]
    , if disabled then
        ( [], [] )

      else
        ( [ ring [] False "transparent" <| (inner + outer) / 2 ]
        , [ ring [] False "transparent" outer ]
        )
    )


viewDuckOnMap : Data -> MapModel -> Person -> List (Svg GameMsgTuple)
viewDuckOnMap data mapModel { city } =
    let
        radius =
            MapPixels.inPixels mapSize.width * 0.006

        ( x, y ) =
            case mapModel.travellingTo of
                Nothing ->
                    ( city.coordinates.x, city.coordinates.y )

                Just ( frac, otherPersonId ) ->
                    case Dict.get otherPersonId data of
                        Nothing ->
                            ( city.coordinates.x, city.coordinates.y )

                        Just otherPerson ->
                            let
                                ease f t =
                                    let
                                        k =
                                            if frac < 0.5 then
                                                4 * frac * frac * frac

                                            else
                                                1 - (-2 * frac + 2) ^ 3 / 2
                                    in
                                    f * (1 - k) + t * k
                            in
                            ( ease city.coordinates.x otherPerson.city.coordinates.x
                            , ease city.coordinates.y otherPerson.city.coordinates.y
                            )

        common k =
            [ SA.cy <| String.fromFloat y
            , SA.cx <| String.fromFloat x
            , SA.r <| String.fromFloat <| k * radius
            ]

        duckRadius =
            radius * 2
    in
    [ S.circle (SA.fill "red" :: common 2.3) []
    , S.image
        [ SA.x <| String.fromFloat <| x - duckRadius
        , SA.y <| String.fromFloat <| y - duckRadius
        , SA.width <| String.fromFloat <| duckRadius * 2
        , SA.height <| String.fromFloat <| duckRadius * 2
        , Theme.imageXlinkHref "duckon.webp"
        ]
        []
    ]


viewPerson : Person -> Element GameMsgTuple
viewPerson person =
    let
        style k v =
            Element.htmlAttribute <| Html.Attributes.style k v

        avatarBox =
            Input.button [ width fill, height fill ]
                { onPress = Just ( ViewTalking person.dialog [], Just <| AudioPlay SoundLibrary.quack3 False Effect )
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
                                [ width fill
                                , height fill
                                , Theme.htmlBackgroundImageUrl person.image
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
                    column (mainContainerAttrs person.city)
                        [ viewCityDescription [] person.city
                        , avatarBox
                        ]

                Landscape ->
                    row (mainContainerAttrs person.city)
                        [ viewCityDescription [ height fill ] person.city
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


viewTalking : SharedGameModel -> Person -> TalkingModel -> Element GameMsgTuple
viewTalking { currentPerson } person { chatHistory, currentDialog } =
    let
        history =
            List.map
                (\{ image, name, line } ->
                    viewDialogLine True { image = image, name = name } line
                )
                (List.reverse chatHistory)

        current =
            [ viewDialogLine False person currentDialog.text
            , currentDialog.choices
                |> (\( h, t ) -> h :: t)
                |> List.map
                    (viewChoice
                        ({ name = person.name
                         , image = person.image
                         , line = currentDialog.text
                         }
                            :: chatHistory
                        )
                    )
                |> wrappedRow [ width fill, Theme.spacing ]
            ]

        menu =
            menuButtonAndLabel
                (ViewMenu { background = person.city.image })
                (if String.isEmpty currentPerson then
                    initialMenuLabel

                 else
                    ""
                )
    in
    column
        (mainContainerAttrs person.city)
        (history ++ current ++ [ menu ])


initialMenuLabel : String
initialMenuLabel =
    "Menu / Accessibility"


menuButtonAndLabel : GameMsg -> String -> Element GameMsgTuple
menuButtonAndLabel msg label =
    Input.button
        [ alignBottom
        , width fill
        ]
        { onPress = Just ( msg, Nothing )
        , label =
            row
                [ Theme.spacing
                , width fill
                ]
                [ avatar 1 { image = "sdc.webp", name = "" }
                , if String.isEmpty label then
                    Element.none

                  else
                    semiBox
                        [ Theme.borderWidth
                        , Theme.paddingXYWithCoeff 2 1
                        , height fill
                        , width fill
                        , Background.color <| Element.rgb 1 1 1
                        ]
                        (el [ centerY ] <| text label)
                ]
        }


viewQuizzing : Person -> Quiz -> Element GameMsgTuple
viewQuizzing person ({ question, correctAnswer, wrongAnswers } as quiz) =
    let
        menu =
            menuButtonAndLabel (ViewMenu { background = person.city.image }) ""
    in
    column
        (mainContainerAttrs person.city)
        [ semiBox [ width fill ] <|
            paragraph
                [ Font.center
                , Font.bold
                , width fill
                , Theme.fontSize 3
                ]
                [ text "QUIZ TIME!" ]
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
        , menu
        ]


duckPerson : { image : String, name : String }
duckPerson =
    { image = "duck.webp"
    , name = "DUCK"
    }


viewQuizAnswer : Quiz -> String -> Element GameMsgTuple
viewQuizAnswer quiz answer =
    let
        next =
            if answer == quiz.correctAnswer then
                { text =
                    quiz.messageIfCorrect
                        |> withDefaultIfEmpty "That's correct!"
                , choices =
                    ( { text = "Thank you!"
                      , next = NextGiveTicket
                      }
                    , []
                    )
                }

            else
                { text =
                    quiz.messageIfWrong
                        |> withDefaultIfEmpty "Are you sure?"
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
        , onPress = Just ( ViewTalking next [], Just <| AudioPlay SoundLibrary.quack1 False Effect )
        }


withDefaultIfEmpty : String -> String -> String
withDefaultIfEmpty default arg =
    if String.isEmpty arg then
        default

    else
        arg


avatar : Float -> { a | image : String, name : String } -> Element msg
avatar scale person =
    let
        size =
            [ Theme.autoscalingI (scale * 120) (width << px)
            , Theme.autoscalingI (scale * 120) (height << px)
            ]
    in
    el
        ([ Theme.borderWidth
         , Theme.borderRounded
         , Background.image <| Env.imageToUrl person.image
         , alignTop
         ]
            ++ size
        )
        (Theme.image (Element.transparent True :: size)
            { src = person.image
            , description =
                if String.isEmpty person.name then
                    "Menu"

                else
                    person.name ++ "'s avatar"
            }
        )


viewChoice : ChatHistory -> Choice -> Element GameMsgTuple
viewChoice chatHistory { text, next } =
    Input.button [ width fill ]
        { label = viewDialogLine False duckPerson text
        , onPress =
            Just
                ( case next of
                    NextDialog n ->
                        ViewTalking n
                            ({ image = duckPerson.image
                             , name = duckPerson.name
                             , line = text
                             }
                                :: chatHistory
                            )

                    NextViewMap ->
                        ViewMap

                    NextRandomQuiz ->
                        PickQuiz

                    NextQuiz quiz ->
                        ViewQuiz quiz

                    NextGiveTicket ->
                        GiveTicketAndViewMap
                , Just <| AudioPlay SoundLibrary.quack1 False Effect
                )
        }


viewDialogLine : Bool -> { a | image : String, name : String } -> String -> Element msg
viewDialogLine historical personIsh text =
    row [ Theme.spacing, width fill ]
        [ avatar 1 personIsh
        , semiBox
            [ Theme.borderWidth
            , width fill
            , height fill
            , Theme.paddingXYWithCoeff 2 2
            , if historical then
                Theme.historicalBackground

              else
                Theme.semitransparentBackground
            ]
            (viewMarked [ centerY, width fill ] text)
        ]


cityBackground : { a | image : String } -> Attribute msg
cityBackground city =
    Element.behindContent <|
        el
            [ width fill
            , height fill
            , Theme.htmlBackgroundImageUrl city.image
            , Element.htmlAttribute <| Html.Attributes.style "background-position" "center"
            , Element.htmlAttribute <| Html.Attributes.style "background-size" "cover"
            ]
            Element.none


viewCityDescription : List (Attribute msg) -> City -> Element msg
viewCityDescription attrs city =
    semiBox (width fill :: attrs)
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
         , Theme.semitransparentBackground
         ]
            ++ attrs
        )



-- Markdown renderer


viewMarked : List (Attribute msg) -> String -> Element msg
viewMarked attrs input =
    input
        |> String.replace "  " "\n\n"
        |> Markdown.Parser.parse
        |> Result.mapError (\_ -> "Parsing error")
        |> Result.andThen (Markdown.Renderer.render Frontend.GameMarkdown.elmUiRenderer)
        |> Result.map (column (Theme.spacing :: attrs))
        |> Result.withDefault (text input)
