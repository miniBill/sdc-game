module Editors exposing (personEditor, personDefault)

{-|

@docs personEditor, personDefault

-}

import Element.WithContext as Element
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Input as Input
import Frontend.EditorTheme as Theme exposing (Element)
import Html.Attributes
import List.Extra
import Model
import Tuple


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
                                [ Theme.spacing, Element.width Element.fill ]
                                [ Tuple.first pair, Tuple.second pair ]
                        )
                    |> Element.row [ Theme.spacing, Element.width Element.fill ]

            else
                Element.table
                    [ Theme.spacing, Element.width Element.fill ]
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
        , Background.color (Theme.getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Theme.borderRounded
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
                                [ Theme.spacing, Element.width Element.fill ]
                                [ Tuple.first pair, Tuple.second pair ]
                        )
                    |> Element.row [ Theme.spacing, Element.width Element.fill ]

            else
                Element.table
                    [ Theme.spacing, Element.width Element.fill ]
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
        , Background.color (Theme.getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Theme.borderRounded
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
                                [ Theme.spacing, Element.width Element.fill ]
                                [ Tuple.first pair, Tuple.second pair ]
                        )
                    |> Element.row [ Theme.spacing, Element.width Element.fill ]

            else
                Element.table
                    [ Theme.spacing, Element.width Element.fill ]
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
        , Background.color (Theme.getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Theme.borderRounded
        ]
        (simplesTable :: complexes)
    , Basics.False
    )


nationEditor : Int -> Model.Nation -> ( Element Model.Nation, Bool )
nationEditor level value =
    ( let
        variantRow =
            Input.radioRow
                [ Theme.spacing ]
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
        [ Background.color (Theme.getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Theme.borderRounded
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
                                [ Theme.spacing, Element.width Element.fill ]
                                [ Tuple.first pair, Tuple.second pair ]
                        )
                    |> Element.row [ Theme.spacing, Element.width Element.fill ]

            else
                Element.table
                    [ Theme.spacing, Element.width Element.fill ]
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
        , Background.color (Theme.getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Theme.borderRounded
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
                                [ Theme.spacing, Element.width Element.fill ]
                                [ Tuple.first pair, Tuple.second pair ]
                        )
                    |> Element.row [ Theme.spacing, Element.width Element.fill ]

            else
                Element.table
                    [ Theme.spacing, Element.width Element.fill ]
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
        , Background.color (Theme.getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Theme.borderRounded
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
                [ Theme.spacing ]
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
        [ Background.color (Theme.getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Theme.borderRounded
        ]
        [ variantRow
        , Element.row [ Element.width Element.fill, Theme.spacing ] inputsRow
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
                                [ Theme.spacing, Element.width Element.fill ]
                                [ Tuple.first pair, Tuple.second pair ]
                        )
                    |> Element.row [ Theme.spacing, Element.width Element.fill ]

            else
                Element.table
                    [ Theme.spacing, Element.width Element.fill ]
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
        , Background.color (Theme.getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Theme.borderRounded
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
                [ Theme.spacing ]
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
        [ Background.color (Theme.getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Theme.borderRounded
        ]
        [ variantRow
        , Element.row [ Element.width Element.fill, Theme.spacing ] inputsRow
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
                [ Theme.spacing ]
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
                                                        [ Theme.spacing
                                                        , Element.width
                                                            Element.fill
                                                        ]
                                                        [ Tuple.first pair
                                                        , Tuple.second pair
                                                        ]
                                                )
                                            |> Element.row
                                                [ Theme.spacing
                                                , Element.width Element.fill
                                                ]

                                    else
                                        Element.table
                                            [ Theme.spacing
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
                                , Background.color (Theme.getColor (level + 1))
                                , Element.width Element.fill
                                , Theme.spacing
                                , Theme.padding
                                , Element.alignTop
                                , Border.width 1
                                , Theme.borderRounded
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
                                                        [ Theme.spacing
                                                        , Element.width
                                                            Element.fill
                                                        ]
                                                        [ Tuple.first pair
                                                        , Tuple.second pair
                                                        ]
                                                )
                                            |> Element.row
                                                [ Theme.spacing
                                                , Element.width Element.fill
                                                ]

                                    else
                                        Element.table
                                            [ Theme.spacing
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
                                , Background.color (Theme.getColor (level + 1))
                                , Element.width Element.fill
                                , Theme.spacing
                                , Theme.padding
                                , Element.alignTop
                                , Border.width 1
                                , Theme.borderRounded
                                ]
                                (simplesTable :: complexes)
                             , Basics.False
                             )
                            )
                        )
                    ]
      in
      Element.column
        [ Background.color (Theme.getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Theme.borderRounded
        ]
        [ variantRow
        , Element.row [ Element.width Element.fill, Theme.spacing ] inputsRow
        ]
    , Basics.False
    )


transportKindEditor : Int -> Model.TransportKind -> ( Element Model.TransportKind, Bool )
transportKindEditor level value =
    ( let
        variantRow =
            Input.radioRow
                [ Theme.spacing ]
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
        [ Background.color (Theme.getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Theme.borderRounded
        ]
        variantRow
    , Basics.False
    )


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


intEditor : Int -> Int -> ( Element Basics.Int, Bool )
intEditor level value =
    ( Element.map
        (\lambdaArg0 -> lambdaArg0 |> String.toInt |> Maybe.withDefault value)
        (Input.text
            [ Element.width (Element.minimum 100 Element.fill)
            , Element.alignTop
            , Background.color (Theme.getColor level)
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
            , Background.color (Theme.getColor level)
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
                [ Background.color (Theme.getColor level)
                , Element.width Element.fill
                , Theme.spacing
                , Theme.padding
                , Element.alignTop
                , Border.width 1
                , Theme.borderRounded
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
        , Background.color (Theme.getColor level)
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
        [ Theme.spacing, Element.alignTop ]
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
                                    , right = Theme.rythm
                                    , bottom = 0
                                    , left = 0
                                    }
                                , Element.alignRight
                                ]
                                (Theme.tabButton
                                    [ Theme.spacing
                                    , Theme.padding
                                    , Element.alignTop
                                    , Border.width 1
                                    , Theme.borderRounded
                                    , Background.gradient
                                        { angle = 0
                                        , steps =
                                            [ Theme.getColor (level + 1)
                                            , Theme.colors.delete
                                            , Theme.colors.delete
                                            ]
                                        }
                                    , Border.widthEach
                                        { bottom = 0
                                        , left = 1
                                        , right = 1
                                        , top = 1
                                        }
                                    , Border.roundEach
                                        { topLeft = Theme.rythm
                                        , topRight = Theme.rythm
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
            [ Background.color (Theme.getColor level)
            , Element.width Element.fill
            , Theme.spacing
            , Theme.padding
            , Element.alignTop
            , Border.width 1
            , Theme.borderRounded
            ]
            rows
        , Element.el
            [ Element.paddingEach
                { top = 0, right = Theme.rythm, bottom = 0, left = Theme.rythm }
            , Element.alignRight
            ]
            (Theme.button
                [ Theme.spacing
                , Theme.padding
                , Element.alignTop
                , Border.width 1
                , Theme.borderRounded
                , Background.gradient
                    { angle = 0
                    , steps =
                        [ Theme.colors.addNew
                        , Theme.colors.addNew
                        , Theme.colors.addNew
                        , Theme.getColor level
                        ]
                    }
                , Border.widthEach { bottom = 1, left = 1, right = 1, top = 0 }
                , Border.roundEach
                    { topLeft = 0
                    , topRight = 0
                    , bottomLeft = Theme.rythm
                    , bottomRight = Theme.rythm
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
