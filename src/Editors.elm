module Editors exposing (dataEditor, idEditor, personEditor, cityEditor, cityNameEditor, coordinatesEditor, nationEditor, dialogEditor, choiceEditor, nextEditor, quizEditor, consequenceEditor, itemEditor, transportKindEditor, conditionEditor, itemNameEditor, sharedGameModelDefault, gameModelDefault, mapModelDefault, talkingModelDefault, chatHistoryDefault, menuModelDefault, dataDefault, idDefault, personDefault, cityDefault, cityNameDefault, coordinatesDefault, nationDefault, dialogDefault, choiceDefault, nextDefault, quizDefault, consequenceDefault, itemDefault, transportKindDefault, conditionDefault, itemNameDefault)

{-|

@docs dataEditor, idEditor, personEditor, cityEditor, cityNameEditor, coordinatesEditor, nationEditor, dialogEditor, choiceEditor, nextEditor, quizEditor, consequenceEditor, itemEditor, transportKindEditor, conditionEditor, itemNameEditor, sharedGameModelDefault, gameModelDefault, mapModelDefault, talkingModelDefault, chatHistoryDefault, menuModelDefault, dataDefault, idDefault, personDefault, cityDefault, cityNameDefault, coordinatesDefault, nationDefault, dialogDefault, choiceDefault, nextDefault, quizDefault, consequenceDefault, itemDefault, transportKindDefault, conditionDefault, itemNameDefault

-}

import Dict
import Element.WithContext as Element
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Input as Input
import Frontend.Theme exposing (Element)
import Html.Attributes
import List.Extra
import Model
import Set
import Tuple


dataEditor : Int -> Model.Data -> ( Element Model.Data, Bool )
dataEditor level value =
    dictEditor idEditor idDefault personEditor personDefault level value


idEditor : Int -> Model.Id -> ( Element Model.Id, Bool )
idEditor level value =
    stringEditor level value


personEditor : Int -> Model.Person -> ( Element Model.Person, Bool )
personEditor level value =
    let
        raw =
            [ let
                ( editor, simple ) =
                    stringEditor (level + 1) value.name
              in
              ( "Name"
              , Element.map
                    (\lambdaArg0 -> { value | name = lambdaArg0 })
                    editor
              , simple
              )
            , let
                ( editor, simple ) =
                    cityEditor (level + 1) value.city
              in
              ( "City"
              , Element.map
                    (\lambdaArg0 -> { value | city = lambdaArg0 })
                    editor
              , simple
              )
            , let
                ( editor, simple ) =
                    stringEditor (level + 1) value.image
              in
              ( "Image"
              , Element.map
                    (\lambdaArg0 -> { value | image = lambdaArg0 })
                    editor
              , simple
              )
            , let
                ( editor, simple ) =
                    dialogEditor (level + 1) value.dialog
              in
              ( "Dialog"
              , Element.map
                    (\lambdaArg0 -> { value | dialog = lambdaArg0 })
                    editor
              , simple
              )
            , let
                ( editor, simple ) =
                    listEditor
                        "Quiz"
                        quizEditor
                        quizDefault
                        (level + 1)
                        value.quizzes
              in
              ( "Quizzes"
              , Element.map
                    (\lambdaArg0 -> { value | quizzes = lambdaArg0 })
                    editor
              , simple
              )
            ]

        simples =
            raw
                |> List.filterMap
                    (\( fieldName, fieldEditor, simple ) ->
                        if simple then
                            Maybe.Just
                                ( Element.el
                                    [ Element.centerY ]
                                    (Element.text fieldName)
                                , fieldEditor
                                )

                        else
                            Maybe.Nothing
                    )

        simplesTable =
            if List.length simples <= 2 then
                simples
                    |> List.map
                        (\pair ->
                            Element.row
                                [ Frontend.Theme.spacing, Element.width Element.fill ]
                                [ Tuple.first pair, Tuple.second pair ]
                        )
                    |> Element.row [ Frontend.Theme.spacing, Element.width Element.fill ]

            else
                Element.table
                    [ Frontend.Theme.spacing, Element.width Element.fill ]
                    { columns =
                        [ { header = Element.none
                          , width = Element.shrink
                          , view = \pair -> Tuple.first pair
                          }
                        , { header = Element.none
                          , width = Element.fill
                          , view = \pair -> Tuple.second pair
                          }
                        ]
                    , data = simples
                    }

        complexes =
            raw
                |> List.concatMap
                    (\( fieldName, fieldEditor, simple ) ->
                        if simple then
                            []

                        else
                            [ Element.text fieldName, fieldEditor ]
                    )
    in
    ( Element.column
        [ Element.width Element.fill
        , Background.color (Frontend.Theme.getColor level)
        , Element.width Element.fill
        , Frontend.Theme.spacing
        , Frontend.Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Frontend.Theme.rythm
        ]
        (simplesTable :: complexes)
    , Basics.False
    )


cityEditor : Int -> Model.City -> ( Element Model.City, Bool )
cityEditor level value =
    let
        raw =
            [ let
                ( editor, simple ) =
                    cityNameEditor (level + 1) value.name
              in
              ( "Name"
              , Element.map
                    (\lambdaArg0 -> { value | name = lambdaArg0 })
                    editor
              , simple
              )
            , let
                ( editor, simple ) =
                    stringEditor (level + 1) value.text
              in
              ( "Text"
              , Element.map
                    (\lambdaArg0 -> { value | text = lambdaArg0 })
                    editor
              , simple
              )
            , let
                ( editor, simple ) =
                    stringEditor (level + 1) value.image
              in
              ( "Image"
              , Element.map
                    (\lambdaArg0 -> { value | image = lambdaArg0 })
                    editor
              , simple
              )
            , let
                ( editor, simple ) =
                    coordinatesEditor (level + 1) value.coordinates
              in
              ( "Coordinates"
              , Element.map
                    (\lambdaArg0 -> { value | coordinates = lambdaArg0 })
                    editor
              , simple
              )
            , let
                ( editor, simple ) =
                    nationEditor (level + 1) value.nation
              in
              ( "Nation"
              , Element.map
                    (\lambdaArg0 -> { value | nation = lambdaArg0 })
                    editor
              , simple
              )
            ]

        simples =
            raw
                |> List.filterMap
                    (\( fieldName, fieldEditor, simple ) ->
                        if simple then
                            Maybe.Just
                                ( Element.el
                                    [ Element.centerY ]
                                    (Element.text fieldName)
                                , fieldEditor
                                )

                        else
                            Maybe.Nothing
                    )

        simplesTable =
            if List.length simples <= 2 then
                simples
                    |> List.map
                        (\pair ->
                            Element.row
                                [ Frontend.Theme.spacing, Element.width Element.fill ]
                                [ Tuple.first pair, Tuple.second pair ]
                        )
                    |> Element.row [ Frontend.Theme.spacing, Element.width Element.fill ]

            else
                Element.table
                    [ Frontend.Theme.spacing, Element.width Element.fill ]
                    { columns =
                        [ { header = Element.none
                          , width = Element.shrink
                          , view = \pair -> Tuple.first pair
                          }
                        , { header = Element.none
                          , width = Element.fill
                          , view = \pair -> Tuple.second pair
                          }
                        ]
                    , data = simples
                    }

        complexes =
            raw
                |> List.concatMap
                    (\( fieldName, fieldEditor, simple ) ->
                        if simple then
                            []

                        else
                            [ Element.text fieldName, fieldEditor ]
                    )
    in
    ( Element.column
        [ Element.width Element.fill
        , Background.color (Frontend.Theme.getColor level)
        , Element.width Element.fill
        , Frontend.Theme.spacing
        , Frontend.Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Frontend.Theme.rythm
        ]
        (simplesTable :: complexes)
    , Basics.False
    )


cityNameEditor : Int -> Model.CityName -> ( Element Model.CityName, Bool )
cityNameEditor level value =
    stringEditor level value


coordinatesEditor : Int -> Model.Coordinates -> ( Element Model.Coordinates, Bool )
coordinatesEditor level value =
    let
        raw =
            [ let
                ( editor, simple ) =
                    floatEditor (level + 1) value.x
              in
              ( "X"
              , Element.map (\lambdaArg0 -> { value | x = lambdaArg0 }) editor
              , simple
              )
            , let
                ( editor, simple ) =
                    floatEditor (level + 1) value.y
              in
              ( "Y"
              , Element.map (\lambdaArg0 -> { value | y = lambdaArg0 }) editor
              , simple
              )
            ]

        simples =
            raw
                |> List.filterMap
                    (\( fieldName, fieldEditor, simple ) ->
                        if simple then
                            Maybe.Just
                                ( Element.el
                                    [ Element.centerY ]
                                    (Element.text fieldName)
                                , fieldEditor
                                )

                        else
                            Maybe.Nothing
                    )

        simplesTable =
            if List.length simples <= 2 then
                simples
                    |> List.map
                        (\pair ->
                            Element.row
                                [ Frontend.Theme.spacing, Element.width Element.fill ]
                                [ Tuple.first pair, Tuple.second pair ]
                        )
                    |> Element.row [ Frontend.Theme.spacing, Element.width Element.fill ]

            else
                Element.table
                    [ Frontend.Theme.spacing, Element.width Element.fill ]
                    { columns =
                        [ { header = Element.none
                          , width = Element.shrink
                          , view = \pair -> Tuple.first pair
                          }
                        , { header = Element.none
                          , width = Element.fill
                          , view = \pair -> Tuple.second pair
                          }
                        ]
                    , data = simples
                    }

        complexes =
            raw
                |> List.concatMap
                    (\( fieldName, fieldEditor, simple ) ->
                        if simple then
                            []

                        else
                            [ Element.text fieldName, fieldEditor ]
                    )
    in
    ( Element.column
        [ Element.width Element.fill
        , Background.color (Frontend.Theme.getColor level)
        , Element.width Element.fill
        , Frontend.Theme.spacing
        , Frontend.Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Frontend.Theme.rythm
        ]
        (simplesTable :: complexes)
    , Basics.False
    )


nationEditor : Int -> Model.Nation -> ( Element Model.Nation, Bool )
nationEditor level value =
    ( let
        variantRow =
            Input.radioRow
                [ Frontend.Theme.spacing ]
                { onChange = Basics.identity
                , options =
                    [ Input.option Model.Austria (Element.text "Austria")
                    , Input.option Model.Belgium (Element.text "Belgium")
                    , Input.option Model.England (Element.text "England")
                    , Input.option Model.France (Element.text "France")
                    , Input.option Model.Germany (Element.text "Germany")
                    , Input.option Model.Italy (Element.text "Italy")
                    , Input.option Model.Netherlands (Element.text "Netherlands")
                    , Input.option Model.Norway (Element.text "Norway")
                    ]
                , selected = Maybe.Just value
                , label = Input.labelHidden ""
                }
      in
      Element.el
        [ Background.color (Frontend.Theme.getColor level)
        , Element.width Element.fill
        , Frontend.Theme.spacing
        , Frontend.Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Frontend.Theme.rythm
        ]
        variantRow
    , Basics.False
    )


dialogEditor : Int -> Model.Dialog -> ( Element Model.Dialog, Bool )
dialogEditor level value =
    let
        raw =
            [ let
                ( editor, simple ) =
                    stringEditor (level + 1) value.text
              in
              ( "Text"
              , Element.map
                    (\lambdaArg0 -> { value | text = lambdaArg0 })
                    editor
              , simple
              )
            , let
                ( editor, simple ) =
                    tupleEditor
                        choiceEditor
                        choiceDefault
                        (listEditor "Choice" choiceEditor choiceDefault)
                        []
                        (level + 1)
                        value.choices
              in
              ( "Choices"
              , Element.map
                    (\lambdaArg0 -> { value | choices = lambdaArg0 })
                    editor
              , simple
              )
            ]

        simples =
            raw
                |> List.filterMap
                    (\( fieldName, fieldEditor, simple ) ->
                        if simple then
                            Maybe.Just
                                ( Element.el
                                    [ Element.centerY ]
                                    (Element.text fieldName)
                                , fieldEditor
                                )

                        else
                            Maybe.Nothing
                    )

        simplesTable =
            if List.length simples <= 2 then
                simples
                    |> List.map
                        (\pair ->
                            Element.row
                                [ Frontend.Theme.spacing, Element.width Element.fill ]
                                [ Tuple.first pair, Tuple.second pair ]
                        )
                    |> Element.row [ Frontend.Theme.spacing, Element.width Element.fill ]

            else
                Element.table
                    [ Frontend.Theme.spacing, Element.width Element.fill ]
                    { columns =
                        [ { header = Element.none
                          , width = Element.shrink
                          , view = \pair -> Tuple.first pair
                          }
                        , { header = Element.none
                          , width = Element.fill
                          , view = \pair -> Tuple.second pair
                          }
                        ]
                    , data = simples
                    }

        complexes =
            raw
                |> List.concatMap
                    (\( fieldName, fieldEditor, simple ) ->
                        if simple then
                            []

                        else
                            [ Element.text fieldName, fieldEditor ]
                    )
    in
    ( Element.column
        [ Element.width Element.fill
        , Background.color (Frontend.Theme.getColor level)
        , Element.width Element.fill
        , Frontend.Theme.spacing
        , Frontend.Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Frontend.Theme.rythm
        ]
        (simplesTable :: complexes)
    , Basics.False
    )


choiceEditor : Int -> Model.Choice -> ( Element Model.Choice, Bool )
choiceEditor level value =
    let
        raw =
            [ let
                ( editor, simple ) =
                    stringEditor (level + 1) value.text
              in
              ( "Text"
              , Element.map
                    (\lambdaArg0 -> { value | text = lambdaArg0 })
                    editor
              , simple
              )
            , let
                ( editor, simple ) =
                    nextEditor (level + 1) value.next
              in
              ( "Next"
              , Element.map
                    (\lambdaArg0 -> { value | next = lambdaArg0 })
                    editor
              , simple
              )
            ]

        simples =
            raw
                |> List.filterMap
                    (\( fieldName, fieldEditor, simple ) ->
                        if simple then
                            Maybe.Just
                                ( Element.el
                                    [ Element.centerY ]
                                    (Element.text fieldName)
                                , fieldEditor
                                )

                        else
                            Maybe.Nothing
                    )

        simplesTable =
            if List.length simples <= 2 then
                simples
                    |> List.map
                        (\pair ->
                            Element.row
                                [ Frontend.Theme.spacing, Element.width Element.fill ]
                                [ Tuple.first pair, Tuple.second pair ]
                        )
                    |> Element.row [ Frontend.Theme.spacing, Element.width Element.fill ]

            else
                Element.table
                    [ Frontend.Theme.spacing, Element.width Element.fill ]
                    { columns =
                        [ { header = Element.none
                          , width = Element.shrink
                          , view = \pair -> Tuple.first pair
                          }
                        , { header = Element.none
                          , width = Element.fill
                          , view = \pair -> Tuple.second pair
                          }
                        ]
                    , data = simples
                    }

        complexes =
            raw
                |> List.concatMap
                    (\( fieldName, fieldEditor, simple ) ->
                        if simple then
                            []

                        else
                            [ Element.text fieldName, fieldEditor ]
                    )
    in
    ( Element.column
        [ Element.width Element.fill
        , Background.color (Frontend.Theme.getColor level)
        , Element.width Element.fill
        , Frontend.Theme.spacing
        , Frontend.Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Frontend.Theme.rythm
        ]
        (simplesTable :: complexes)
    , Basics.False
    )


nextEditor : Int -> Model.Next -> ( Element Model.Next, Bool )
nextEditor level value =
    ( let
        { dialogExtracted, quizExtracted } =
            case value of
                Model.NextDialog dialog ->
                    { extractedDefault | dialogExtracted = dialog }

                Model.NextViewMap ->
                    extractedDefault

                Model.NextRandomQuiz ->
                    extractedDefault

                Model.NextQuiz quiz ->
                    { extractedDefault | quizExtracted = quiz }

                Model.NextGiveTicket ->
                    extractedDefault

        extractedDefault =
            { dialogExtracted = dialogDefault, quizExtracted = quizDefault }

        variantRow =
            Input.radioRow
                [ Frontend.Theme.spacing ]
                { onChange = Basics.identity
                , options =
                    [ Input.option
                        (Model.NextDialog dialogExtracted)
                        (Element.text "Dialog")
                    , Input.option Model.NextViewMap (Element.text "View map")
                    , Input.option
                        Model.NextRandomQuiz
                        (Element.text "Random quiz")
                    , Input.option
                        (Model.NextQuiz quizExtracted)
                        (Element.text "Quiz")
                    , Input.option
                        Model.NextGiveTicket
                        (Element.text "Give ticket")
                    ]
                , selected = Maybe.Just value
                , label = Input.labelHidden ""
                }

        inputsRow =
            case value of
                Model.NextDialog dialog ->
                    [ Element.map
                        Model.NextDialog
                        (Tuple.first (dialogEditor (level + 1) dialog))
                    ]

                Model.NextViewMap ->
                    []

                Model.NextRandomQuiz ->
                    []

                Model.NextQuiz quiz ->
                    [ Element.map
                        Model.NextQuiz
                        (Tuple.first (quizEditor (level + 1) quiz))
                    ]

                Model.NextGiveTicket ->
                    []
      in
      Element.column
        [ Background.color (Frontend.Theme.getColor level)
        , Element.width Element.fill
        , Frontend.Theme.spacing
        , Frontend.Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Frontend.Theme.rythm
        ]
        [ variantRow
        , Element.row [ Element.width Element.fill, Frontend.Theme.spacing ] inputsRow
        ]
    , Basics.False
    )


quizEditor : Int -> Model.Quiz -> ( Element Model.Quiz, Bool )
quizEditor level value =
    let
        raw =
            [ let
                ( editor, simple ) =
                    stringEditor (level + 1) value.question
              in
              ( "Question"
              , Element.map
                    (\lambdaArg0 -> { value | question = lambdaArg0 })
                    editor
              , simple
              )
            , let
                ( editor, simple ) =
                    stringEditor (level + 1) value.correctAnswer
              in
              ( "Correct answer"
              , Element.map
                    (\lambdaArg0 -> { value | correctAnswer = lambdaArg0 })
                    editor
              , simple
              )
            , let
                ( editor, simple ) =
                    stringEditor (level + 1) value.messageIfCorrect
              in
              ( "Message if correct"
              , Element.map
                    (\lambdaArg0 -> { value | messageIfCorrect = lambdaArg0 })
                    editor
              , simple
              )
            , let
                ( editor, simple ) =
                    stringEditor (level + 1) value.messageIfWrong
              in
              ( "Message if wrong"
              , Element.map
                    (\lambdaArg0 -> { value | messageIfWrong = lambdaArg0 })
                    editor
              , simple
              )
            , let
                ( editor, simple ) =
                    listEditor
                        "String"
                        stringEditor
                        ""
                        (level + 1)
                        value.wrongAnswers
              in
              ( "Wrong answers"
              , Element.map
                    (\lambdaArg0 -> { value | wrongAnswers = lambdaArg0 })
                    editor
              , simple
              )
            ]

        simples =
            raw
                |> List.filterMap
                    (\( fieldName, fieldEditor, simple ) ->
                        if simple then
                            Maybe.Just
                                ( Element.el
                                    [ Element.centerY ]
                                    (Element.text fieldName)
                                , fieldEditor
                                )

                        else
                            Maybe.Nothing
                    )

        simplesTable =
            if List.length simples <= 2 then
                simples
                    |> List.map
                        (\pair ->
                            Element.row
                                [ Frontend.Theme.spacing, Element.width Element.fill ]
                                [ Tuple.first pair, Tuple.second pair ]
                        )
                    |> Element.row [ Frontend.Theme.spacing, Element.width Element.fill ]

            else
                Element.table
                    [ Frontend.Theme.spacing, Element.width Element.fill ]
                    { columns =
                        [ { header = Element.none
                          , width = Element.shrink
                          , view = \pair -> Tuple.first pair
                          }
                        , { header = Element.none
                          , width = Element.fill
                          , view = \pair -> Tuple.second pair
                          }
                        ]
                    , data = simples
                    }

        complexes =
            raw
                |> List.concatMap
                    (\( fieldName, fieldEditor, simple ) ->
                        if simple then
                            []

                        else
                            [ Element.text fieldName, fieldEditor ]
                    )
    in
    ( Element.column
        [ Element.width Element.fill
        , Background.color (Frontend.Theme.getColor level)
        , Element.width Element.fill
        , Frontend.Theme.spacing
        , Frontend.Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Frontend.Theme.rythm
        ]
        (simplesTable :: complexes)
    , Basics.False
    )


consequenceEditor : Int -> Model.Consequence -> ( Element Model.Consequence, Bool )
consequenceEditor level value =
    ( let
        { boolExtracted, intExtracted, itemExtracted, stringExtracted } =
            case value of
                Model.ConsequenceGetMoney int ->
                    { extractedDefault | intExtracted = int }

                Model.ConsequenceLoseMoney int ->
                    { extractedDefault | intExtracted = int }

                Model.ConsequenceGetItem item ->
                    { extractedDefault | itemExtracted = item }

                Model.ConsequenceLoseItem string ->
                    { extractedDefault | stringExtracted = string }

                Model.ConsequenceSetLocalFlag string bool ->
                    { extractedDefault
                        | stringExtracted = string
                        , boolExtracted = bool
                    }

        extractedDefault =
            { boolExtracted = True
            , intExtracted = 0
            , itemExtracted = itemDefault
            , stringExtracted = ""
            }

        variantRow =
            Input.radioRow
                [ Frontend.Theme.spacing ]
                { onChange = Basics.identity
                , options =
                    [ Input.option
                        (Model.ConsequenceGetMoney intExtracted)
                        (Element.text "Get money")
                    , Input.option
                        (Model.ConsequenceLoseMoney intExtracted)
                        (Element.text "Lose money")
                    , Input.option
                        (Model.ConsequenceGetItem itemExtracted)
                        (Element.text "Get item")
                    , Input.option
                        (Model.ConsequenceLoseItem stringExtracted)
                        (Element.text "Lose item")
                    , Input.option
                        (Model.ConsequenceSetLocalFlag
                            stringExtracted
                            boolExtracted
                        )
                        (Element.text "Set local flag")
                    ]
                , selected = Maybe.Just value
                , label = Input.labelHidden ""
                }

        inputsRow =
            case value of
                Model.ConsequenceGetMoney int ->
                    [ Element.map
                        Model.ConsequenceGetMoney
                        (Tuple.first (intEditor (level + 1) int))
                    ]

                Model.ConsequenceLoseMoney int ->
                    [ Element.map
                        Model.ConsequenceLoseMoney
                        (Tuple.first (intEditor (level + 1) int))
                    ]

                Model.ConsequenceGetItem item ->
                    [ Element.map
                        Model.ConsequenceGetItem
                        (Tuple.first (itemEditor (level + 1) item))
                    ]

                Model.ConsequenceLoseItem string ->
                    [ Element.map
                        Model.ConsequenceLoseItem
                        (Tuple.first (stringEditor (level + 1) string))
                    ]

                Model.ConsequenceSetLocalFlag string bool ->
                    [ Element.map
                        (\lambdaArg0 ->
                            Model.ConsequenceSetLocalFlag lambdaArg0 bool
                        )
                        (Tuple.first (stringEditor (level + 1) string))
                    , Element.map
                        (Model.ConsequenceSetLocalFlag string)
                        (Tuple.first (boolEditor (level + 1) bool))
                    ]
      in
      Element.column
        [ Background.color (Frontend.Theme.getColor level)
        , Element.width Element.fill
        , Frontend.Theme.spacing
        , Frontend.Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Frontend.Theme.rythm
        ]
        [ variantRow
        , Element.row [ Element.width Element.fill, Frontend.Theme.spacing ] inputsRow
        ]
    , Basics.False
    )


itemEditor : Int -> Model.Item -> ( Element Model.Item, Bool )
itemEditor level value =
    ( let
        { fromCityNametoCityNamekindTransportKindconsequencesListConsequenceExtracted, nameStringimageStringExtracted } =
            case value of
                Model.GenericItem nameStringimageString ->
                    { extractedDefault
                        | nameStringimageStringExtracted = nameStringimageString
                    }

                Model.Ticket fromCityNametoCityNamekindTransportKindconsequencesListConsequence ->
                    { extractedDefault
                        | fromCityNametoCityNamekindTransportKindconsequencesListConsequenceExtracted =
                            fromCityNametoCityNamekindTransportKindconsequencesListConsequence
                    }

        extractedDefault =
            { fromCityNametoCityNamekindTransportKindconsequencesListConsequenceExtracted =
                { from = cityNameDefault
                , to = cityNameDefault
                , kind = transportKindDefault
                , consequences = []
                }
            , nameStringimageStringExtracted = { name = "", image = "" }
            }

        variantRow =
            Input.radioRow
                [ Frontend.Theme.spacing ]
                { onChange = Basics.identity
                , options =
                    [ Input.option
                        (Model.GenericItem nameStringimageStringExtracted)
                        (Element.text "Generic item")
                    , Input.option
                        (Model.Ticket
                            fromCityNametoCityNamekindTransportKindconsequencesListConsequenceExtracted
                        )
                        (Element.text "Ticket")
                    ]
                , selected = Maybe.Just value
                , label = Input.labelHidden ""
                }

        inputsRow =
            case value of
                Model.GenericItem nameStringimageString ->
                    [ Element.map
                        Model.GenericItem
                        (Tuple.first
                            (let
                                raw =
                                    [ let
                                        ( editor, simple ) =
                                            stringEditor
                                                (level + 1 + 1)
                                                nameStringimageString.name
                                      in
                                      ( "Name"
                                      , Element.map
                                            (\lambdaArg0 ->
                                                { nameStringimageString
                                                    | name = lambdaArg0
                                                }
                                            )
                                            editor
                                      , simple
                                      )
                                    , let
                                        ( editor, simple ) =
                                            stringEditor
                                                (level + 1 + 1)
                                                nameStringimageString.image
                                      in
                                      ( "Image"
                                      , Element.map
                                            (\lambdaArg0 ->
                                                { nameStringimageString
                                                    | image = lambdaArg0
                                                }
                                            )
                                            editor
                                      , simple
                                      )
                                    ]

                                simples =
                                    raw
                                        |> List.filterMap
                                            (\( fieldName, fieldEditor, simple ) ->
                                                if simple then
                                                    Maybe.Just
                                                        ( Element.el
                                                            [ Element.centerY ]
                                                            (Element.text fieldName)
                                                        , fieldEditor
                                                        )

                                                else
                                                    Maybe.Nothing
                                            )

                                simplesTable =
                                    if List.length simples <= 2 then
                                        simples
                                            |> List.map
                                                (\pair ->
                                                    Element.row
                                                        [ Frontend.Theme.spacing
                                                        , Element.width
                                                            Element.fill
                                                        ]
                                                        [ Tuple.first pair
                                                        , Tuple.second pair
                                                        ]
                                                )
                                            |> Element.row
                                                [ Frontend.Theme.spacing
                                                , Element.width Element.fill
                                                ]

                                    else
                                        Element.table
                                            [ Frontend.Theme.spacing
                                            , Element.width Element.fill
                                            ]
                                            { columns =
                                                [ { header = Element.none
                                                  , width = Element.shrink
                                                  , view =
                                                        \pair -> Tuple.first pair
                                                  }
                                                , { header = Element.none
                                                  , width = Element.fill
                                                  , view =
                                                        \pair -> Tuple.second pair
                                                  }
                                                ]
                                            , data = simples
                                            }

                                complexes =
                                    raw
                                        |> List.concatMap
                                            (\( fieldName, fieldEditor, simple ) ->
                                                if simple then
                                                    []

                                                else
                                                    [ Element.text fieldName
                                                    , fieldEditor
                                                    ]
                                            )
                             in
                             ( Element.column
                                [ Element.width Element.fill
                                , Background.color (Frontend.Theme.getColor (level + 1))
                                , Element.width Element.fill
                                , Frontend.Theme.spacing
                                , Frontend.Theme.padding
                                , Element.alignTop
                                , Border.width 1
                                , Border.rounded Frontend.Theme.rythm
                                ]
                                (simplesTable :: complexes)
                             , Basics.False
                             )
                            )
                        )
                    ]

                Model.Ticket fromCityNametoCityNamekindTransportKindconsequencesListConsequence ->
                    [ Element.map
                        Model.Ticket
                        (Tuple.first
                            (let
                                raw =
                                    [ let
                                        ( editor, simple ) =
                                            cityNameEditor
                                                (level + 1 + 1)
                                                fromCityNametoCityNamekindTransportKindconsequencesListConsequence.from
                                      in
                                      ( "From"
                                      , Element.map
                                            (\lambdaArg0 ->
                                                { fromCityNametoCityNamekindTransportKindconsequencesListConsequence
                                                    | from = lambdaArg0
                                                }
                                            )
                                            editor
                                      , simple
                                      )
                                    , let
                                        ( editor, simple ) =
                                            cityNameEditor
                                                (level + 1 + 1)
                                                fromCityNametoCityNamekindTransportKindconsequencesListConsequence.to
                                      in
                                      ( "To"
                                      , Element.map
                                            (\lambdaArg0 ->
                                                { fromCityNametoCityNamekindTransportKindconsequencesListConsequence
                                                    | to = lambdaArg0
                                                }
                                            )
                                            editor
                                      , simple
                                      )
                                    , let
                                        ( editor, simple ) =
                                            transportKindEditor
                                                (level + 1 + 1)
                                                fromCityNametoCityNamekindTransportKindconsequencesListConsequence.kind
                                      in
                                      ( "Kind"
                                      , Element.map
                                            (\lambdaArg0 ->
                                                { fromCityNametoCityNamekindTransportKindconsequencesListConsequence
                                                    | kind = lambdaArg0
                                                }
                                            )
                                            editor
                                      , simple
                                      )
                                    , let
                                        ( editor, simple ) =
                                            listEditor
                                                "Consequence"
                                                consequenceEditor
                                                consequenceDefault
                                                (level + 1 + 1)
                                                fromCityNametoCityNamekindTransportKindconsequencesListConsequence.consequences
                                      in
                                      ( "Consequences"
                                      , Element.map
                                            (\lambdaArg0 ->
                                                { fromCityNametoCityNamekindTransportKindconsequencesListConsequence
                                                    | consequences = lambdaArg0
                                                }
                                            )
                                            editor
                                      , simple
                                      )
                                    ]

                                simples =
                                    raw
                                        |> List.filterMap
                                            (\( fieldName, fieldEditor, simple ) ->
                                                if simple then
                                                    Maybe.Just
                                                        ( Element.el
                                                            [ Element.centerY ]
                                                            (Element.text fieldName)
                                                        , fieldEditor
                                                        )

                                                else
                                                    Maybe.Nothing
                                            )

                                simplesTable =
                                    if List.length simples <= 2 then
                                        simples
                                            |> List.map
                                                (\pair ->
                                                    Element.row
                                                        [ Frontend.Theme.spacing
                                                        , Element.width
                                                            Element.fill
                                                        ]
                                                        [ Tuple.first pair
                                                        , Tuple.second pair
                                                        ]
                                                )
                                            |> Element.row
                                                [ Frontend.Theme.spacing
                                                , Element.width Element.fill
                                                ]

                                    else
                                        Element.table
                                            [ Frontend.Theme.spacing
                                            , Element.width Element.fill
                                            ]
                                            { columns =
                                                [ { header = Element.none
                                                  , width = Element.shrink
                                                  , view =
                                                        \pair -> Tuple.first pair
                                                  }
                                                , { header = Element.none
                                                  , width = Element.fill
                                                  , view =
                                                        \pair -> Tuple.second pair
                                                  }
                                                ]
                                            , data = simples
                                            }

                                complexes =
                                    raw
                                        |> List.concatMap
                                            (\( fieldName, fieldEditor, simple ) ->
                                                if simple then
                                                    []

                                                else
                                                    [ Element.text fieldName
                                                    , fieldEditor
                                                    ]
                                            )
                             in
                             ( Element.column
                                [ Element.width Element.fill
                                , Background.color (Frontend.Theme.getColor (level + 1))
                                , Element.width Element.fill
                                , Frontend.Theme.spacing
                                , Frontend.Theme.padding
                                , Element.alignTop
                                , Border.width 1
                                , Border.rounded Frontend.Theme.rythm
                                ]
                                (simplesTable :: complexes)
                             , Basics.False
                             )
                            )
                        )
                    ]
      in
      Element.column
        [ Background.color (Frontend.Theme.getColor level)
        , Element.width Element.fill
        , Frontend.Theme.spacing
        , Frontend.Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Frontend.Theme.rythm
        ]
        [ variantRow
        , Element.row [ Element.width Element.fill, Frontend.Theme.spacing ] inputsRow
        ]
    , Basics.False
    )


transportKindEditor : Int -> Model.TransportKind -> ( Element Model.TransportKind, Bool )
transportKindEditor level value =
    ( let
        variantRow =
            Input.radioRow
                [ Frontend.Theme.spacing ]
                { onChange = Basics.identity
                , options =
                    [ Input.option Model.Plane (Element.text "Plane")
                    , Input.option Model.Train (Element.text "Train")
                    , Input.option Model.Coach (Element.text "Coach")
                    , Input.option Model.Bike (Element.text "Bike")
                    , Input.option Model.Boat (Element.text "Boat")
                    , Input.option Model.Ferry (Element.text "Ferry")
                    , Input.option Model.DuckWalk (Element.text "Duck walk")
                    ]
                , selected = Maybe.Just value
                , label = Input.labelHidden ""
                }
      in
      Element.el
        [ Background.color (Frontend.Theme.getColor level)
        , Element.width Element.fill
        , Frontend.Theme.spacing
        , Frontend.Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Frontend.Theme.rythm
        ]
        variantRow
    , Basics.False
    )


conditionEditor : Int -> Model.Condition -> ( Element Model.Condition, Bool )
conditionEditor level value =
    ( let
        { conditionExtracted, itemNameExtracted, listConditionExtracted, stringExtracted } =
            case value of
                Model.ConditionNot condition ->
                    { extractedDefault | conditionExtracted = condition }

                Model.ConditionAnd listCondition ->
                    { extractedDefault | listConditionExtracted = listCondition }

                Model.ConditionOr listCondition ->
                    { extractedDefault | listConditionExtracted = listCondition }

                Model.HasItem itemName ->
                    { extractedDefault | itemNameExtracted = itemName }

                Model.LocalFlag string ->
                    { extractedDefault | stringExtracted = string }

        extractedDefault =
            { conditionExtracted = conditionDefault
            , itemNameExtracted = itemNameDefault
            , listConditionExtracted = []
            , stringExtracted = ""
            }

        variantRow =
            Input.radioRow
                [ Frontend.Theme.spacing ]
                { onChange = Basics.identity
                , options =
                    [ Input.option
                        (Model.ConditionNot conditionExtracted)
                        (Element.text "Not")
                    , Input.option
                        (Model.ConditionAnd listConditionExtracted)
                        (Element.text "And")
                    , Input.option
                        (Model.ConditionOr listConditionExtracted)
                        (Element.text "Or")
                    , Input.option
                        (Model.HasItem itemNameExtracted)
                        (Element.text "Has item")
                    , Input.option
                        (Model.LocalFlag stringExtracted)
                        (Element.text "Local flag")
                    ]
                , selected = Maybe.Just value
                , label = Input.labelHidden ""
                }

        inputsRow =
            case value of
                Model.ConditionNot condition ->
                    [ Element.map
                        Model.ConditionNot
                        (Tuple.first (conditionEditor (level + 1) condition))
                    ]

                Model.ConditionAnd listCondition ->
                    [ Element.map
                        Model.ConditionAnd
                        (Tuple.first
                            (listEditor
                                "Condition"
                                conditionEditor
                                conditionDefault
                                (level + 1)
                                listCondition
                            )
                        )
                    ]

                Model.ConditionOr listCondition ->
                    [ Element.map
                        Model.ConditionOr
                        (Tuple.first
                            (listEditor
                                "Condition"
                                conditionEditor
                                conditionDefault
                                (level + 1)
                                listCondition
                            )
                        )
                    ]

                Model.HasItem itemName ->
                    [ Element.map
                        Model.HasItem
                        (Tuple.first (itemNameEditor (level + 1) itemName))
                    ]

                Model.LocalFlag string ->
                    [ Element.map
                        Model.LocalFlag
                        (Tuple.first (stringEditor (level + 1) string))
                    ]
      in
      Element.column
        [ Background.color (Frontend.Theme.getColor level)
        , Element.width Element.fill
        , Frontend.Theme.spacing
        , Frontend.Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Frontend.Theme.rythm
        ]
        [ variantRow
        , Element.row [ Element.width Element.fill, Frontend.Theme.spacing ] inputsRow
        ]
    , Basics.False
    )


itemNameEditor : Int -> Model.ItemName -> ( Element Model.ItemName, Bool )
itemNameEditor level value =
    ( let
        { fromCityNametoCityNamekindTransportKindExtracted, stringExtracted } =
            case value of
                Model.GenericItemName string ->
                    { extractedDefault | stringExtracted = string }

                Model.TicketName fromCityNametoCityNamekindTransportKind ->
                    { extractedDefault
                        | fromCityNametoCityNamekindTransportKindExtracted =
                            fromCityNametoCityNamekindTransportKind
                    }

        extractedDefault =
            { fromCityNametoCityNamekindTransportKindExtracted =
                { from = cityNameDefault
                , to = cityNameDefault
                , kind = transportKindDefault
                }
            , stringExtracted = ""
            }

        variantRow =
            Input.radioRow
                [ Frontend.Theme.spacing ]
                { onChange = Basics.identity
                , options =
                    [ Input.option
                        (Model.GenericItemName stringExtracted)
                        (Element.text "Generic item name")
                    , Input.option
                        (Model.TicketName
                            fromCityNametoCityNamekindTransportKindExtracted
                        )
                        (Element.text "Ticket name")
                    ]
                , selected = Maybe.Just value
                , label = Input.labelHidden ""
                }

        inputsRow =
            case value of
                Model.GenericItemName string ->
                    [ Element.map
                        Model.GenericItemName
                        (Tuple.first (stringEditor (level + 1) string))
                    ]

                Model.TicketName fromCityNametoCityNamekindTransportKind ->
                    [ Element.map
                        Model.TicketName
                        (Tuple.first
                            (let
                                raw =
                                    [ let
                                        ( editor, simple ) =
                                            cityNameEditor
                                                (level + 1 + 1)
                                                fromCityNametoCityNamekindTransportKind.from
                                      in
                                      ( "From"
                                      , Element.map
                                            (\lambdaArg0 ->
                                                { fromCityNametoCityNamekindTransportKind
                                                    | from = lambdaArg0
                                                }
                                            )
                                            editor
                                      , simple
                                      )
                                    , let
                                        ( editor, simple ) =
                                            cityNameEditor
                                                (level + 1 + 1)
                                                fromCityNametoCityNamekindTransportKind.to
                                      in
                                      ( "To"
                                      , Element.map
                                            (\lambdaArg0 ->
                                                { fromCityNametoCityNamekindTransportKind
                                                    | to = lambdaArg0
                                                }
                                            )
                                            editor
                                      , simple
                                      )
                                    , let
                                        ( editor, simple ) =
                                            transportKindEditor
                                                (level + 1 + 1)
                                                fromCityNametoCityNamekindTransportKind.kind
                                      in
                                      ( "Kind"
                                      , Element.map
                                            (\lambdaArg0 ->
                                                { fromCityNametoCityNamekindTransportKind
                                                    | kind = lambdaArg0
                                                }
                                            )
                                            editor
                                      , simple
                                      )
                                    ]

                                simples =
                                    raw
                                        |> List.filterMap
                                            (\( fieldName, fieldEditor, simple ) ->
                                                if simple then
                                                    Maybe.Just
                                                        ( Element.el
                                                            [ Element.centerY ]
                                                            (Element.text fieldName)
                                                        , fieldEditor
                                                        )

                                                else
                                                    Maybe.Nothing
                                            )

                                simplesTable =
                                    if List.length simples <= 2 then
                                        simples
                                            |> List.map
                                                (\pair ->
                                                    Element.row
                                                        [ Frontend.Theme.spacing
                                                        , Element.width
                                                            Element.fill
                                                        ]
                                                        [ Tuple.first pair
                                                        , Tuple.second pair
                                                        ]
                                                )
                                            |> Element.row
                                                [ Frontend.Theme.spacing
                                                , Element.width Element.fill
                                                ]

                                    else
                                        Element.table
                                            [ Frontend.Theme.spacing
                                            , Element.width Element.fill
                                            ]
                                            { columns =
                                                [ { header = Element.none
                                                  , width = Element.shrink
                                                  , view =
                                                        \pair -> Tuple.first pair
                                                  }
                                                , { header = Element.none
                                                  , width = Element.fill
                                                  , view =
                                                        \pair -> Tuple.second pair
                                                  }
                                                ]
                                            , data = simples
                                            }

                                complexes =
                                    raw
                                        |> List.concatMap
                                            (\( fieldName, fieldEditor, simple ) ->
                                                if simple then
                                                    []

                                                else
                                                    [ Element.text fieldName
                                                    , fieldEditor
                                                    ]
                                            )
                             in
                             ( Element.column
                                [ Element.width Element.fill
                                , Background.color (Frontend.Theme.getColor (level + 1))
                                , Element.width Element.fill
                                , Frontend.Theme.spacing
                                , Frontend.Theme.padding
                                , Element.alignTop
                                , Border.width 1
                                , Border.rounded Frontend.Theme.rythm
                                ]
                                (simplesTable :: complexes)
                             , Basics.False
                             )
                            )
                        )
                    ]
      in
      Element.column
        [ Background.color (Frontend.Theme.getColor level)
        , Element.width Element.fill
        , Frontend.Theme.spacing
        , Frontend.Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Frontend.Theme.rythm
        ]
        [ variantRow
        , Element.row [ Element.width Element.fill, Frontend.Theme.spacing ] inputsRow
        ]
    , Basics.False
    )


sharedGameModelDefault : Model.SharedGameModel
sharedGameModelDefault =
    { currentPerson = idDefault, tickets = Set.empty }


gameModelDefault : Model.GameModel
gameModelDefault =
    Model.ViewingPerson


mapModelDefault : Model.MapModel
mapModelDefault =
    {}


talkingModelDefault : Model.TalkingModel
talkingModelDefault =
    { chatHistory = chatHistoryDefault, currentDialog = dialogDefault }


chatHistoryDefault : Model.ChatHistory
chatHistoryDefault =
    []


menuModelDefault : Model.MenuModel
menuModelDefault =
    { previous = gameModelDefault, background = "" }


dataDefault : Model.Data
dataDefault =
    Dict.empty


idDefault : Model.Id
idDefault =
    ""


personDefault : Model.Person
personDefault =
    { name = ""
    , city = cityDefault
    , image = ""
    , dialog = dialogDefault
    , quizzes = []
    }


cityDefault : Model.City
cityDefault =
    { name = cityNameDefault
    , text = ""
    , image = ""
    , coordinates = coordinatesDefault
    , nation = nationDefault
    }


cityNameDefault : Model.CityName
cityNameDefault =
    ""


coordinatesDefault : Model.Coordinates
coordinatesDefault =
    { x = 0, y = 0 }


nationDefault : Model.Nation
nationDefault =
    Model.Austria


dialogDefault : Model.Dialog
dialogDefault =
    { text = "", choices = ( choiceDefault, [] ) }


choiceDefault : Model.Choice
choiceDefault =
    { text = "", next = nextDefault }


nextDefault : Model.Next
nextDefault =
    Model.NextViewMap


quizDefault : Model.Quiz
quizDefault =
    { question = ""
    , correctAnswer = ""
    , messageIfCorrect = ""
    , messageIfWrong = ""
    , wrongAnswers = []
    }


consequenceDefault : Model.Consequence
consequenceDefault =
    Model.ConsequenceGetMoney 0


itemDefault : Model.Item
itemDefault =
    Model.GenericItem { name = "", image = "" }


transportKindDefault : Model.TransportKind
transportKindDefault =
    Model.Plane


conditionDefault : Model.Condition
conditionDefault =
    Model.ConditionAnd []


itemNameDefault : Model.ItemName
itemNameDefault =
    Model.GenericItemName ""


intEditor : Int -> Int -> ( Element Basics.Int, Bool )
intEditor level value =
    ( Element.map
        (\lambdaArg0 -> lambdaArg0 |> String.toInt |> Maybe.withDefault value)
        (Input.text
            [ Element.width (Element.minimum 100 Element.fill)
            , Element.alignTop
            , Background.color (Frontend.Theme.getColor level)
            ]
            { onChange = Basics.identity
            , text = String.fromInt value
            , placeholder = Maybe.Nothing
            , label = Input.labelHidden ""
            }
        )
    , Basics.True
    )


floatEditor : Int -> Float -> ( Element Basics.Float, Bool )
floatEditor level value =
    ( Element.map
        (\lambdaArg0 -> lambdaArg0 |> String.toFloat |> Maybe.withDefault value)
        (Input.text
            [ Element.width (Element.minimum 100 Element.fill)
            , Element.alignTop
            , Background.color (Frontend.Theme.getColor level)
            ]
            { onChange = Basics.identity
            , text = String.fromFloat value
            , placeholder = Maybe.Nothing
            , label = Input.labelHidden ""
            }
        )
    , Basics.True
    )


tupleEditor :
    (Int -> l -> ( Element l, Bool ))
    -> l
    -> (Int -> r -> ( Element r, Bool ))
    -> r
    -> Int
    -> ( l, r )
    -> ( Element ( l, r ), Bool )
tupleEditor leftEditor _ rightEditor _ level ( left, right ) =
    let
        ( le, lb ) =
            leftEditor (level + 1) left

        ( re, rb ) =
            rightEditor (level + 1) right

        editor =
            (if lb && rb then
                Element.row

             else
                Element.column
            )
                [ Background.color (Frontend.Theme.getColor level)
                , Element.width Element.fill
                , Frontend.Theme.spacing
                , Frontend.Theme.padding
                , Element.alignTop
                , Border.width 1
                , Border.rounded Frontend.Theme.rythm
                ]
                [ Element.map (\lambdaArg0 -> ( lambdaArg0, right )) le
                , Element.map (\lambdaArg0 -> ( left, lambdaArg0 )) re
                ]
    in
    ( editor, Basics.False )


stringEditor : Int -> String -> ( Element String.String, Bool )
stringEditor level value =
    ( Input.text
        [ Element.width (Element.minimum 100 Element.fill)
        , Element.alignTop
        , Background.color (Frontend.Theme.getColor level)
        ]
        { onChange = Basics.identity
        , text = value
        , placeholder = Maybe.Nothing
        , label = Input.labelHidden ""
        }
    , Basics.True
    )


boolEditor : Int -> Bool -> ( Element Basics.Bool, Bool )
boolEditor _ value =
    ( Input.radioRow
        [ Frontend.Theme.spacing, Element.alignTop ]
        { onChange = Basics.identity
        , options =
            [ Input.option Basics.True (Element.text "True")
            , Input.option Basics.False (Element.text "False")
            ]
        , selected = Maybe.Just value
        , label = Input.labelHidden ""
        }
    , Basics.True
    )


listEditor :
    String
    -> (Int -> e -> ( Element e, Bool ))
    -> e
    -> Int
    -> List e
    -> ( Element (List e), Bool )
listEditor typeName valueEditor valueDefault level value =
    let
        rows =
            List.indexedMap
                (\i row ->
                    Element.map
                        (\lambdaArg0 ->
                            if lambdaArg0 == valueDefault then
                                List.Extra.removeAt i value

                            else
                                List.Extra.setAt i lambdaArg0 value
                        )
                        (Element.column
                            [ Element.width Element.fill ]
                            [ Element.el
                                [ Element.paddingEach
                                    { top = 0
                                    , right = Frontend.Theme.rythm
                                    , bottom = 0
                                    , left = 0
                                    }
                                , Element.alignRight
                                ]
                                (Frontend.Theme.tabButton
                                    [ Frontend.Theme.spacing
                                    , Frontend.Theme.padding
                                    , Element.alignTop
                                    , Border.width 1
                                    , Border.rounded Frontend.Theme.rythm
                                    , Background.gradient
                                        { angle = 0
                                        , steps =
                                            [ Frontend.Theme.getColor (level + 1)
                                            , Frontend.Theme.colors.delete
                                            , Frontend.Theme.colors.delete
                                            ]
                                        }
                                    , Border.widthEach
                                        { bottom = 0
                                        , left = 1
                                        , right = 1
                                        , top = 1
                                        }
                                    , Border.roundEach
                                        { topLeft = Frontend.Theme.rythm
                                        , topRight = Frontend.Theme.rythm
                                        , bottomLeft = 0
                                        , bottomRight = 0
                                        }
                                    , Element.htmlAttribute
                                        (Html.Attributes.style "z-index" "1")
                                    ]
                                    { onPress = Maybe.Just valueDefault
                                    , label = Element.text "Delete"
                                    }
                                )
                            , Element.el
                                [ Element.width Element.fill, Element.moveUp 1 ]
                                (Tuple.first (valueEditor (level + 1) row))
                            ]
                        )
                )
                value
    in
    ( Element.column
        [ Element.width Element.fill ]
        [ Element.column
            [ Background.color (Frontend.Theme.getColor level)
            , Element.width Element.fill
            , Frontend.Theme.spacing
            , Frontend.Theme.padding
            , Element.alignTop
            , Border.width 1
            , Border.rounded Frontend.Theme.rythm
            ]
            rows
        , Element.el
            [ Element.paddingEach
                { top = 0, right = Frontend.Theme.rythm, bottom = 0, left = Frontend.Theme.rythm }
            , Element.alignRight
            ]
            (Frontend.Theme.button
                [ Frontend.Theme.spacing
                , Frontend.Theme.padding
                , Element.alignTop
                , Border.width 1
                , Border.rounded Frontend.Theme.rythm
                , Background.gradient
                    { angle = 0
                    , steps =
                        [ Frontend.Theme.colors.addNew
                        , Frontend.Theme.colors.addNew
                        , Frontend.Theme.colors.addNew
                        , Frontend.Theme.getColor level
                        ]
                    }
                , Border.widthEach { bottom = 1, left = 1, right = 1, top = 0 }
                , Border.roundEach
                    { topLeft = 0
                    , topRight = 0
                    , bottomLeft = Frontend.Theme.rythm
                    , bottomRight = Frontend.Theme.rythm
                    }
                , Element.moveUp 1
                ]
                { onPress = Maybe.Just (value ++ [ valueDefault ])
                , label = Element.text ("Add new " ++ typeName)
                }
            )
        ]
    , Basics.False
    )


dictEditor :
    (Int -> comparable -> ( Element comparable, Bool ))
    -> comparable
    -> (Int -> v -> ( Element v, Bool ))
    -> v
    -> Int
    -> Dict.Dict comparable v
    -> ( Element (Dict.Dict comparable v), Bool )
dictEditor keyEditor keyDefault valueEditor valueDefault level value =
    let
        keysColumn =
            { header = Element.none
            , width = Element.shrink
            , view =
                \( key, memberValue ) ->
                    Element.map
                        (\lambdaArg0 ->
                            if
                                lambdaArg0
                                    == keyDefault
                                    && memberValue
                                    == valueDefault
                            then
                                Dict.remove key value

                            else
                                Dict.insert
                                    lambdaArg0
                                    memberValue
                                    (Dict.remove key value)
                        )
                        (Tuple.first (keyEditor (level + 1) key))
            }

        valuesColumn =
            { header = Element.none
            , width = Element.fill
            , view =
                \( key, memberValue ) ->
                    Element.map
                        (\lambdaArg0 ->
                            if key == keyDefault && lambdaArg0 == valueDefault then
                                Dict.remove key value

                            else
                                Dict.insert key lambdaArg0 value
                        )
                        (Tuple.first (valueEditor (level + 1) memberValue))
            }
    in
    ( Element.table
        [ Background.color (Frontend.Theme.getColor level)
        , Element.width Element.fill
        , Frontend.Theme.spacing
        , Frontend.Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Frontend.Theme.rythm
        ]
        { data = Dict.toList value ++ [ ( keyDefault, valueDefault ) ]
        , columns = [ keysColumn, valuesColumn ]
        }
    , Basics.False
    )
