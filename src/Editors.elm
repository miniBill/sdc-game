module Editors exposing (dataEditor, cityEditor, coordinatesEditor, cityNameEditor, personEditor, idEditor, dialogEditor, choiceEditor, consequenceEditor, itemEditor, transportKindEditor, conditionEditor, itemNameEditor, dataDefault, cityDefault, coordinatesDefault, cityNameDefault, personDefault, idDefault, dialogDefault, choiceDefault, consequenceDefault, itemDefault, transportKindDefault, conditionDefault, itemNameDefault)

{-|

@docs dataEditor, cityEditor, coordinatesEditor, cityNameEditor, personEditor, idEditor, dialogEditor, choiceEditor, consequenceEditor, itemEditor, transportKindEditor, conditionEditor, itemNameEditor, dataDefault, cityDefault, coordinatesDefault, cityNameDefault, personDefault, idDefault, dialogDefault, choiceDefault, consequenceDefault, itemDefault, transportKindDefault, conditionDefault, itemNameDefault

-}

import Dict
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import List.Extra
import Model
import Theme


dataEditor : Int -> Model.Data -> Element.Element Model.Data
dataEditor level value =
    dictEditor idEditor idDefault cityEditor cityDefault level value


cityEditor : Int -> Model.City -> Element.Element Model.City
cityEditor level value =
    Element.column
        [ Element.width Element.fill
        , Background.color (getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Theme.rythm
        ]
        [ Element.text "Name"
        , Element.map
            (\lambdaArg0 ->
                let
                    updating =
                        value
                in
                { updating | name = lambdaArg0 }
            )
            (cityNameEditor (level + 1) value.name)
        , Element.text "Text"
        , Element.map
            (\lambdaArg0 ->
                let
                    updating =
                        value
                in
                { updating | text = lambdaArg0 }
            )
            (stringEditor (level + 1) value.text)
        , Element.text "Image"
        , Element.map
            (\lambdaArg0 ->
                let
                    updating =
                        value
                in
                { updating | image = lambdaArg0 }
            )
            (stringEditor (level + 1) value.image)
        , Element.text "Coordinates"
        , Element.map
            (\lambdaArg0 ->
                let
                    updating =
                        value
                in
                { updating | coordinates = lambdaArg0 }
            )
            (coordinatesEditor (level + 1) value.coordinates)
        , Element.text "People"
        , Element.map
            (\lambdaArg0 ->
                let
                    updating =
                        value
                in
                { updating | people = lambdaArg0 }
            )
            (listEditor
                "Person"
                personEditor
                personDefault
                (level + 1)
                value.people
            )
        ]


coordinatesEditor : Int -> Model.Coordinates -> Element.Element Model.Coordinates
coordinatesEditor level value =
    Element.column
        [ Element.width Element.fill
        , Background.color (getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Theme.rythm
        ]
        [ Element.text "North"
        , Element.map
            (\lambdaArg0 ->
                let
                    updating =
                        value
                in
                { updating | north = lambdaArg0 }
            )
            (floatEditor (level + 1) value.north)
        , Element.text "East"
        , Element.map
            (\lambdaArg0 ->
                let
                    updating =
                        value
                in
                { updating | east = lambdaArg0 }
            )
            (floatEditor (level + 1) value.east)
        ]


cityNameEditor : Int -> Model.CityName -> Element.Element Model.CityName
cityNameEditor level value =
    stringEditor level value


personEditor : Int -> Model.Person -> Element.Element Model.Person
personEditor level value =
    Element.column
        [ Element.width Element.fill
        , Background.color (getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Theme.rythm
        ]
        [ Element.text "Name"
        , Element.map
            (\lambdaArg0 ->
                let
                    updating =
                        value
                in
                { updating | name = lambdaArg0 }
            )
            (stringEditor (level + 1) value.name)
        , Element.text "Image"
        , Element.map
            (\lambdaArg0 ->
                let
                    updating =
                        value
                in
                { updating | image = lambdaArg0 }
            )
            (stringEditor (level + 1) value.image)
        , Element.text "Dialog"
        , Element.map
            (\lambdaArg0 ->
                let
                    updating =
                        value
                in
                { updating | dialog = lambdaArg0 }
            )
            (listEditor
                "(Id, Dialog)"
                (tupleEditor idEditor idDefault dialogEditor dialogDefault)
                ( idDefault, dialogDefault )
                (level + 1)
                value.dialog
            )
        ]


idEditor : Int -> Model.Id -> Element.Element Model.Id
idEditor level value =
    stringEditor level value


dialogEditor : Int -> Model.Dialog -> Element.Element Model.Dialog
dialogEditor level value =
    Element.column
        [ Element.width Element.fill
        , Background.color (getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Theme.rythm
        ]
        [ Element.text "Text"
        , Element.map
            (\lambdaArg0 ->
                let
                    updating =
                        value
                in
                { updating | text = lambdaArg0 }
            )
            (stringEditor (level + 1) value.text)
        , Element.text "Choices"
        , Element.map
            (\lambdaArg0 ->
                let
                    updating =
                        value
                in
                { updating | choices = lambdaArg0 }
            )
            (listEditor
                "Choice"
                choiceEditor
                choiceDefault
                (level + 1)
                value.choices
            )
        ]


choiceEditor : Int -> Model.Choice -> Element.Element Model.Choice
choiceEditor level value =
    Element.column
        [ Element.width Element.fill
        , Background.color (getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Theme.rythm
        ]
        [ Element.text "Text"
        , Element.map
            (\lambdaArg0 ->
                let
                    updating =
                        value
                in
                { updating | text = lambdaArg0 }
            )
            (stringEditor (level + 1) value.text)
        , Element.text "Next"
        , Element.map
            (\lambdaArg0 ->
                let
                    updating =
                        value
                in
                { updating | next = lambdaArg0 }
            )
            (idEditor (level + 1) value.next)
        , Element.text "Consequences"
        , Element.map
            (\lambdaArg0 ->
                let
                    updating =
                        value
                in
                { updating | consequences = lambdaArg0 }
            )
            (listEditor
                "Consequence"
                consequenceEditor
                consequenceDefault
                (level + 1)
                value.consequences
            )
        , Element.text "Condition"
        , Element.map
            (\lambdaArg0 ->
                let
                    updating =
                        value
                in
                { updating | condition = lambdaArg0 }
            )
            (maybeEditor
                "Condition"
                conditionEditor
                conditionDefault
                (level + 1)
                value.condition
            )
        ]


consequenceEditor : Int -> Model.Consequence -> Element.Element Model.Consequence
consequenceEditor level value =
    let
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
                        (intEditor (level + 1) int)
                    ]

                Model.ConsequenceLoseMoney int ->
                    [ Element.map
                        Model.ConsequenceLoseMoney
                        (intEditor (level + 1) int)
                    ]

                Model.ConsequenceGetItem item ->
                    [ Element.map
                        Model.ConsequenceGetItem
                        (itemEditor (level + 1) item)
                    ]

                Model.ConsequenceLoseItem string ->
                    [ Element.map
                        Model.ConsequenceLoseItem
                        (stringEditor (level + 1) string)
                    ]

                Model.ConsequenceSetLocalFlag string bool ->
                    [ Element.map
                        (\lambdaArg0 ->
                            Model.ConsequenceSetLocalFlag lambdaArg0 bool
                        )
                        (stringEditor (level + 1) string)
                    , Element.map
                        (Model.ConsequenceSetLocalFlag string)
                        (boolEditor (level + 1) bool)
                    ]
    in
    Element.column
        [ Background.color (getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Theme.rythm
        ]
        [ variantRow
        , Element.row [ Element.width Element.fill, Theme.spacing ] inputsRow
        ]


itemEditor : Int -> Model.Item -> Element.Element Model.Item
itemEditor level value =
    let
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
                        (Element.column
                            [ Element.width Element.fill
                            , Background.color (getColor (level + 1))
                            , Element.width Element.fill
                            , Theme.spacing
                            , Theme.padding
                            , Element.alignTop
                            , Border.width 1
                            , Border.rounded Theme.rythm
                            ]
                            [ Element.text "Name"
                            , Element.map
                                (\lambdaArg0 ->
                                    let
                                        updating =
                                            nameStringimageString
                                    in
                                    { updating | name = lambdaArg0 }
                                )
                                (stringEditor
                                    (level + 1 + 1)
                                    nameStringimageString.name
                                )
                            , Element.text "Image"
                            , Element.map
                                (\lambdaArg0 ->
                                    let
                                        updating =
                                            nameStringimageString
                                    in
                                    { updating | image = lambdaArg0 }
                                )
                                (stringEditor
                                    (level + 1 + 1)
                                    nameStringimageString.image
                                )
                            ]
                        )
                    ]

                Model.Ticket fromCityNametoCityNamekindTransportKindconsequencesListConsequence ->
                    [ Element.map
                        Model.Ticket
                        (Element.column
                            [ Element.width Element.fill
                            , Background.color (getColor (level + 1))
                            , Element.width Element.fill
                            , Theme.spacing
                            , Theme.padding
                            , Element.alignTop
                            , Border.width 1
                            , Border.rounded Theme.rythm
                            ]
                            [ Element.text "From"
                            , Element.map
                                (\lambdaArg0 ->
                                    let
                                        updating =
                                            fromCityNametoCityNamekindTransportKindconsequencesListConsequence
                                    in
                                    { updating | from = lambdaArg0 }
                                )
                                (cityNameEditor
                                    (level + 1 + 1)
                                    fromCityNametoCityNamekindTransportKindconsequencesListConsequence.from
                                )
                            , Element.text "To"
                            , Element.map
                                (\lambdaArg0 ->
                                    let
                                        updating =
                                            fromCityNametoCityNamekindTransportKindconsequencesListConsequence
                                    in
                                    { updating | to = lambdaArg0 }
                                )
                                (cityNameEditor
                                    (level + 1 + 1)
                                    fromCityNametoCityNamekindTransportKindconsequencesListConsequence.to
                                )
                            , Element.text "Kind"
                            , Element.map
                                (\lambdaArg0 ->
                                    let
                                        updating =
                                            fromCityNametoCityNamekindTransportKindconsequencesListConsequence
                                    in
                                    { updating | kind = lambdaArg0 }
                                )
                                (transportKindEditor
                                    (level + 1 + 1)
                                    fromCityNametoCityNamekindTransportKindconsequencesListConsequence.kind
                                )
                            , Element.text "Consequences"
                            , Element.map
                                (\lambdaArg0 ->
                                    let
                                        updating =
                                            fromCityNametoCityNamekindTransportKindconsequencesListConsequence
                                    in
                                    { updating | consequences = lambdaArg0 }
                                )
                                (listEditor
                                    "Consequence"
                                    consequenceEditor
                                    consequenceDefault
                                    (level + 1 + 1)
                                    fromCityNametoCityNamekindTransportKindconsequencesListConsequence.consequences
                                )
                            ]
                        )
                    ]
    in
    Element.column
        [ Background.color (getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Theme.rythm
        ]
        [ variantRow
        , Element.row [ Element.width Element.fill, Theme.spacing ] inputsRow
        ]


transportKindEditor : Int -> Model.TransportKind -> Element.Element Model.TransportKind
transportKindEditor level value =
    let
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
        [ Background.color (getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Theme.rythm
        ]
        variantRow


conditionEditor : Int -> Model.Condition -> Element.Element Model.Condition
conditionEditor level value =
    let
        { conditionExtracted, itemNameExtracted, listConditionExtracted, stringExtracted } =
            case value of
                Model.ConditionNot condition ->
                    { extractedDefault | conditionExtracted = condition }

                Model.ConditionAnd listCondition ->
                    { extractedDefault
                        | listConditionExtracted = listCondition
                    }

                Model.ConditionOr listCondition ->
                    { extractedDefault
                        | listConditionExtracted = listCondition
                    }

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
                [ Theme.spacing ]
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
                        (conditionEditor (level + 1) condition)
                    ]

                Model.ConditionAnd listCondition ->
                    [ Element.map
                        Model.ConditionAnd
                        (listEditor
                            "Condition"
                            conditionEditor
                            conditionDefault
                            (level + 1)
                            listCondition
                        )
                    ]

                Model.ConditionOr listCondition ->
                    [ Element.map
                        Model.ConditionOr
                        (listEditor
                            "Condition"
                            conditionEditor
                            conditionDefault
                            (level + 1)
                            listCondition
                        )
                    ]

                Model.HasItem itemName ->
                    [ Element.map
                        Model.HasItem
                        (itemNameEditor (level + 1) itemName)
                    ]

                Model.LocalFlag string ->
                    [ Element.map
                        Model.LocalFlag
                        (stringEditor (level + 1) string)
                    ]
    in
    Element.column
        [ Background.color (getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Theme.rythm
        ]
        [ variantRow
        , Element.row [ Element.width Element.fill, Theme.spacing ] inputsRow
        ]


itemNameEditor : Int -> Model.ItemName -> Element.Element Model.ItemName
itemNameEditor level value =
    let
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
                [ Theme.spacing ]
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
                        (stringEditor (level + 1) string)
                    ]

                Model.TicketName fromCityNametoCityNamekindTransportKind ->
                    [ Element.map
                        Model.TicketName
                        (Element.column
                            [ Element.width Element.fill
                            , Background.color (getColor (level + 1))
                            , Element.width Element.fill
                            , Theme.spacing
                            , Theme.padding
                            , Element.alignTop
                            , Border.width 1
                            , Border.rounded Theme.rythm
                            ]
                            [ Element.text "From"
                            , Element.map
                                (\lambdaArg0 ->
                                    let
                                        updating =
                                            fromCityNametoCityNamekindTransportKind
                                    in
                                    { updating | from = lambdaArg0 }
                                )
                                (cityNameEditor
                                    (level + 1 + 1)
                                    fromCityNametoCityNamekindTransportKind.from
                                )
                            , Element.text "To"
                            , Element.map
                                (\lambdaArg0 ->
                                    let
                                        updating =
                                            fromCityNametoCityNamekindTransportKind
                                    in
                                    { updating | to = lambdaArg0 }
                                )
                                (cityNameEditor
                                    (level + 1 + 1)
                                    fromCityNametoCityNamekindTransportKind.to
                                )
                            , Element.text "Kind"
                            , Element.map
                                (\lambdaArg0 ->
                                    let
                                        updating =
                                            fromCityNametoCityNamekindTransportKind
                                    in
                                    { updating | kind = lambdaArg0 }
                                )
                                (transportKindEditor
                                    (level + 1 + 1)
                                    fromCityNametoCityNamekindTransportKind.kind
                                )
                            ]
                        )
                    ]
    in
    Element.column
        [ Background.color (getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Theme.rythm
        ]
        [ variantRow
        , Element.row [ Element.width Element.fill, Theme.spacing ] inputsRow
        ]


dataDefault : Model.Data
dataDefault =
    Dict.empty


cityDefault : Model.City
cityDefault =
    { name = cityNameDefault
    , text = ""
    , image = ""
    , coordinates = coordinatesDefault
    , people = []
    }


coordinatesDefault : Model.Coordinates
coordinatesDefault =
    { north = 0, east = 0 }


cityNameDefault : Model.CityName
cityNameDefault =
    ""


personDefault : Model.Person
personDefault =
    { name = "", image = "", dialog = [] }


idDefault : Model.Id
idDefault =
    ""


dialogDefault : Model.Dialog
dialogDefault =
    { text = "", choices = [] }


choiceDefault : Model.Choice
choiceDefault =
    { text = ""
    , next = idDefault
    , consequences = []
    , condition = Maybe.Nothing
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


intEditor : Int -> Int -> Element.Element Basics.Int
intEditor level value =
    Element.map
        (\lambdaArg0 -> lambdaArg0 |> String.toInt |> Maybe.withDefault value)
        (Input.text
            [ Element.width (Element.minimum 100 Element.fill)
            , Element.alignTop
            , Background.color (getColor level)
            ]
            { onChange = Basics.identity
            , text = String.fromInt value
            , placeholder = Maybe.Nothing
            , label = Input.labelHidden ""
            }
        )


floatEditor : Int -> Float -> Element.Element Basics.Float
floatEditor level value =
    Element.map
        (\lambdaArg0 -> lambdaArg0 |> String.toFloat |> Maybe.withDefault value)
        (Input.text
            [ Element.width (Element.minimum 100 Element.fill)
            , Element.alignTop
            , Background.color (getColor level)
            ]
            { onChange = Basics.identity
            , text = String.fromFloat value
            , placeholder = Maybe.Nothing
            , label = Input.labelHidden ""
            }
        )


tupleEditor :
    (Int -> l -> Element.Element l)
    -> l
    -> (Int -> r -> Element.Element r)
    -> r
    -> Int
    -> ( l, r )
    -> Element.Element ( l, r )
tupleEditor leftEditor _ rightEditor _ level ( left, right ) =
    Element.column
        [ Background.color (getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Theme.rythm
        ]
        [ Element.map
            (\lambdaArg0 -> ( lambdaArg0, right ))
            (leftEditor (level + 1) left)
        , Element.map
            (\lambdaArg0 -> ( left, lambdaArg0 ))
            (rightEditor (level + 1) right)
        ]


maybeEditor :
    String
    -> (Int -> e -> Element.Element e)
    -> e
    -> Int
    -> Maybe e
    -> Element.Element (Maybe e)
maybeEditor typeName valueEditor valueDefault level value =
    let
        extracted =
            case value of
                Nothing ->
                    valueDefault

                Just inner ->
                    inner

        variantRow =
            Input.radioRow
                [ Theme.spacing ]
                { onChange = Basics.identity
                , options =
                    [ Input.option Nothing (Element.text "Nothing")
                    , Input.option
                        (Maybe.Just extracted)
                        (Element.text typeName)
                    ]
                , selected = Maybe.Just value
                , label = Input.labelHidden ""
                }

        inputsRow =
            case value of
                Nothing ->
                    Element.none

                Just inner ->
                    Element.map Maybe.Just (valueEditor (level + 1) inner)
    in
    Element.column
        [ Background.color (getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Theme.rythm
        ]
        [ variantRow, inputsRow ]


stringEditor : Int -> String -> Element.Element String.String
stringEditor level value =
    Input.text
        [ Element.width (Element.minimum 100 Element.fill)
        , Element.alignTop
        , Background.color (getColor level)
        ]
        { onChange = Basics.identity
        , text = value
        , placeholder = Maybe.Nothing
        , label = Input.labelHidden ""
        }


boolEditor : Int -> Bool -> Element.Element Basics.Bool
boolEditor level value =
    Input.radioRow
        [ Theme.spacing, Element.alignTop ]
        { onChange = Basics.identity
        , options =
            [ Input.option True (Element.text "True")
            , Input.option False (Element.text "False")
            ]
        , selected = Maybe.Just value
        , label = Input.labelHidden ""
        }


listEditor :
    String
    -> (Int -> e -> Element.Element e)
    -> e
    -> Int
    -> List e
    -> Element.Element (List e)
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
                                    , Border.rounded Theme.rythm
                                    , Background.color Theme.colors.delete
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
                                    ]
                                    { onPress = Maybe.Just valueDefault
                                    , label = Element.text "Delete"
                                    }
                                )
                            , valueEditor (level + 1) row
                            ]
                        )
                )
                value
    in
    Element.column
        [ Element.width Element.fill ]
        [ Element.column
            [ Background.color (getColor level)
            , Element.width Element.fill
            , Theme.spacing
            , Theme.padding
            , Element.alignTop
            , Border.width 1
            , Border.rounded Theme.rythm
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
                , Border.rounded Theme.rythm
                , Background.color Theme.colors.addNew
                , Border.widthEach { bottom = 1, left = 1, right = 1, top = 0 }
                , Border.roundEach
                    { topLeft = 0
                    , topRight = 0
                    , bottomLeft = Theme.rythm
                    , bottomRight = Theme.rythm
                    }
                ]
                { onPress = Maybe.Just (value ++ [ valueDefault ])
                , label = Element.text ("Add new " ++ typeName)
                }
            )
        ]


dictEditor :
    (Int -> comparable -> Element.Element comparable)
    -> comparable
    -> (Int -> v -> Element.Element v)
    -> v
    -> Int
    -> Dict.Dict comparable v
    -> Element.Element (Dict.Dict comparable v)
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
                        (keyEditor (level + 1) key)
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
                        (valueEditor (level + 1) memberValue)
            }
    in
    Element.table
        [ Background.color (getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Theme.rythm
        ]
        { data = Dict.toList value ++ [ ( keyDefault, valueDefault ) ]
        , columns = [ keysColumn, valuesColumn ]
        }


colors : List Element.Color
colors =
    [ Element.rgb255 0xFD 0xDF 0xDF
    , Element.rgb255 0xFC 0xF7 0xDE
    , Element.rgb255 0xDE 0xFD 0xE0
    , Element.rgb255 0xDE 0xF3 0xFD
    , Element.rgb255 0xF0 0xDE 0xFD
    ]


getColor index =
    let
        reduced =
            Basics.modBy 5 index
    in
    List.drop reduced colors
        |> List.head
        |> Maybe.withDefault (Element.rgb 0.7 0.7 0.7)
