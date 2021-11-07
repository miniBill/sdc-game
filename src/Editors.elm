module Editors exposing (dataEditor, cityEditor, cityNameEditor, personEditor, idEditor, dialogEditor, choiceEditor, consequenceEditor, itemEditor, transportKindEditor, conditionEditor, itemNameEditor, dataDefault, cityDefault, cityNameDefault, personDefault, idDefault, dialogDefault, choiceDefault, consequenceDefault, itemDefault, transportKindDefault, conditionDefault, itemNameDefault)

{-|

@docs dataEditor, cityEditor, cityNameEditor, personEditor, idEditor, dialogEditor, choiceEditor, consequenceEditor, itemEditor, transportKindEditor, conditionEditor, itemNameEditor, dataDefault, cityDefault, cityNameDefault, personDefault, idDefault, dialogDefault, choiceDefault, consequenceDefault, itemDefault, transportKindDefault, conditionDefault, itemNameDefault

-}

import Debug
import Dict
import Element
import Element.Border as Border
import Element.Input as Input
import Model


dataEditor : Model.Data -> Element.Element Model.Data
dataEditor value =
    dictEditor idEditor idDefault cityEditor cityDefault value


cityEditor : Model.City -> Element.Element Model.City
cityEditor value =
    Element.table
        [ spacing ]
        { data =
            [ ( "Name"
              , Element.map
                    (\lambdaArg0 -> { value | name = lambdaArg0 })
                    (cityNameEditor value.name)
              )
            , ( "Text"
              , Element.map
                    (\lambdaArg0 -> { value | text = lambdaArg0 })
                    (stringEditor value.text)
              )
            , ( "Image"
              , Element.map
                    (\lambdaArg0 -> { value | image = lambdaArg0 })
                    (stringEditor value.image)
              )
            , ( "People"
              , Element.map
                    (\lambdaArg0 -> { value | people = lambdaArg0 })
                    (listEditor personEditor personDefault value.people)
              )
            ]
        , columns =
            [ { header = Element.none
              , width = Element.shrink
              , view = \( name, _ ) -> Element.text name
              }
            , { header = Element.none
              , width = Element.fill
              , view = \( _, view ) -> view
              }
            ]
        }


cityNameEditor : Model.CityName -> Element.Element Model.CityName
cityNameEditor value =
    stringEditor value


personEditor : Model.Person -> Element.Element Model.Person
personEditor value =
    Element.table
        [ spacing ]
        { data =
            [ ( "Name"
              , Element.map
                    (\lambdaArg0 -> { value | name = lambdaArg0 })
                    (stringEditor value.name)
              )
            , ( "Image"
              , Element.map
                    (\lambdaArg0 -> { value | image = lambdaArg0 })
                    (stringEditor value.image)
              )
            , ( "Dialog"
              , Element.map
                    (\lambdaArg0 -> { value | dialog = lambdaArg0 })
                    (listEditor
                        (tupleEditor
                            idEditor
                            idDefault
                            dialogEditor
                            dialogDefault
                        )
                        ( idDefault, dialogDefault )
                        value.dialog
                    )
              )
            ]
        , columns =
            [ { header = Element.none
              , width = Element.shrink
              , view = \( name, _ ) -> Element.text name
              }
            , { header = Element.none
              , width = Element.fill
              , view = \( _, view ) -> view
              }
            ]
        }


idEditor : Model.Id -> Element.Element Model.Id
idEditor value =
    stringEditor value


dialogEditor : Model.Dialog -> Element.Element Model.Dialog
dialogEditor value =
    Element.table
        [ spacing ]
        { data =
            [ ( "Text"
              , Element.map
                    (\lambdaArg0 -> { value | text = lambdaArg0 })
                    (stringEditor value.text)
              )
            , ( "Choices"
              , Element.map
                    (\lambdaArg0 -> { value | choices = lambdaArg0 })
                    (listEditor choiceEditor choiceDefault value.choices)
              )
            ]
        , columns =
            [ { header = Element.none
              , width = Element.shrink
              , view = \( name, _ ) -> Element.text name
              }
            , { header = Element.none
              , width = Element.fill
              , view = \( _, view ) -> view
              }
            ]
        }


choiceEditor : Model.Choice -> Element.Element Model.Choice
choiceEditor value =
    Element.table
        [ spacing ]
        { data =
            [ ( "Text"
              , Element.map
                    (\lambdaArg0 -> { value | text = lambdaArg0 })
                    (stringEditor value.text)
              )
            , ( "Next"
              , Element.map
                    (\lambdaArg0 -> { value | next = lambdaArg0 })
                    (idEditor value.next)
              )
            , ( "Consequences"
              , Element.map
                    (\lambdaArg0 -> { value | consequences = lambdaArg0 })
                    (listEditor
                        consequenceEditor
                        consequenceDefault
                        value.consequences
                    )
              )
            , ( "Condition"
              , Element.map
                    (\lambdaArg0 -> { value | condition = lambdaArg0 })
                    (maybeEditor
                        conditionEditor
                        conditionDefault
                        value.condition
                    )
              )
            ]
        , columns =
            [ { header = Element.none
              , width = Element.shrink
              , view = \( name, _ ) -> Element.text name
              }
            , { header = Element.none
              , width = Element.fill
              , view = \( _, view ) -> view
              }
            ]
        }


consequenceEditor : Model.Consequence -> Element.Element Model.Consequence
consequenceEditor value =
    let
        inputsRow =
            case value of
                Model.ConsequenceGetMoney int ->
                    [ Element.map
                        (\newValue -> Model.ConsequenceGetMoney newValue)
                        (intEditor int)
                    ]

                Model.ConsequenceLoseMoney int ->
                    [ Element.map
                        (\newValue -> Model.ConsequenceLoseMoney newValue)
                        (intEditor int)
                    ]

                Model.ConsequenceGetItem item ->
                    [ Element.map
                        (\newValue -> Model.ConsequenceGetItem newValue)
                        (itemEditor item)
                    ]

                Model.ConsequenceLoseItem string ->
                    [ Element.map
                        (\newValue -> Model.ConsequenceLoseItem newValue)
                        (stringEditor string)
                    ]

                Model.ConsequenceSetLocalFlag string bool ->
                    [ Element.map
                        (\newValue ->
                            Model.ConsequenceSetLocalFlag newValue bool
                        )
                        (stringEditor string)
                    , Element.map
                        (\newValue ->
                            Model.ConsequenceSetLocalFlag string newValue
                        )
                        (boolEditor bool)
                    ]

        variantRow =
            Input.radioRow
                [ spacing ]
                { onChange = Basics.identity
                , options =
                    [ Input.option
                        (Model.ConsequenceGetMoney intExtracted)
                        (Element.text "GetMoney")
                    , Input.option
                        (Model.ConsequenceLoseMoney intExtracted)
                        (Element.text "LoseMoney")
                    , Input.option
                        (Model.ConsequenceGetItem itemExtracted)
                        (Element.text "GetItem")
                    , Input.option
                        (Model.ConsequenceLoseItem stringExtracted)
                        (Element.text "LoseItem")
                    , Input.option
                        (Model.ConsequenceSetLocalFlag
                            stringExtracted
                            boolExtracted
                        )
                        (Element.text "SetLocalFlag")
                    ]
                , selected = Maybe.Just value
                , label = Input.labelHidden ""
                }

        extractedDefault =
            { boolExtracted = True
            , intExtracted = 0
            , itemExtracted = itemDefault
            , stringExtracted = ""
            }

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
    in
    Element.column
        [ padding, spacing, Element.alignTop, Border.width 1 ]
        [ variantRow, Element.row [ spacing ] inputsRow ]


itemEditor : Model.Item -> Element.Element Model.Item
itemEditor value =
    let
        inputsRow =
            case value of
                Model.GenericItem nameStringimageString ->
                    [ Element.map
                        (\newValue -> Model.GenericItem newValue)
                        (Element.table
                            [ spacing ]
                            { data =
                                [ ( "Name"
                                  , Element.map
                                        (\lambdaArg0 ->
                                            { value | name = lambdaArg0 }
                                        )
                                        (stringEditor nameStringimageString.name)
                                  )
                                , ( "Image"
                                  , Element.map
                                        (\lambdaArg0 ->
                                            { value | image = lambdaArg0 }
                                        )
                                        (stringEditor
                                            nameStringimageString.image
                                        )
                                  )
                                ]
                            , columns =
                                [ { header = Element.none
                                  , width = Element.shrink
                                  , view = \( name, _ ) -> Element.text name
                                  }
                                , { header = Element.none
                                  , width = Element.fill
                                  , view = \( _, view ) -> view
                                  }
                                ]
                            }
                        )
                    ]

                Model.Ticket fromCityNametoCityNamekindTransportKindconsequencesListConsequence ->
                    [ Element.map
                        (\newValue -> Model.Ticket newValue)
                        (Element.table
                            [ spacing ]
                            { data =
                                [ ( "From"
                                  , Element.map
                                        (\lambdaArg0 ->
                                            { value | from = lambdaArg0 }
                                        )
                                        (cityNameEditor
                                            fromCityNametoCityNamekindTransportKindconsequencesListConsequence.from
                                        )
                                  )
                                , ( "To"
                                  , Element.map
                                        (\lambdaArg0 ->
                                            { value | to = lambdaArg0 }
                                        )
                                        (cityNameEditor
                                            fromCityNametoCityNamekindTransportKindconsequencesListConsequence.to
                                        )
                                  )
                                , ( "Kind"
                                  , Element.map
                                        (\lambdaArg0 ->
                                            { value | kind = lambdaArg0 }
                                        )
                                        (transportKindEditor
                                            fromCityNametoCityNamekindTransportKindconsequencesListConsequence.kind
                                        )
                                  )
                                , ( "Consequences"
                                  , Element.map
                                        (\lambdaArg0 ->
                                            { value
                                                | consequences = lambdaArg0
                                            }
                                        )
                                        (listEditor
                                            consequenceEditor
                                            consequenceDefault
                                            fromCityNametoCityNamekindTransportKindconsequencesListConsequence.consequences
                                        )
                                  )
                                ]
                            , columns =
                                [ { header = Element.none
                                  , width = Element.shrink
                                  , view = \( name, _ ) -> Element.text name
                                  }
                                , { header = Element.none
                                  , width = Element.fill
                                  , view = \( _, view ) -> view
                                  }
                                ]
                            }
                        )
                    ]

        variantRow =
            Input.radioRow
                [ spacing ]
                { onChange = Basics.identity
                , options =
                    [ Input.option
                        (Model.GenericItem nameStringimageStringExtracted)
                        (Element.text "GenericItem")
                    , Input.option
                        (Model.Ticket
                            fromCityNametoCityNamekindTransportKindconsequencesListConsequenceExtracted
                        )
                        (Element.text "Ticket")
                    ]
                , selected = Maybe.Just value
                , label = Input.labelHidden ""
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
    in
    Element.column
        [ padding, spacing, Element.alignTop, Border.width 1 ]
        [ variantRow, Element.row [ spacing ] inputsRow ]


transportKindEditor : Model.TransportKind -> Element.Element Model.TransportKind
transportKindEditor value =
    let
        variantRow =
            Input.radioRow
                [ spacing ]
                { onChange = Basics.identity
                , options =
                    [ Input.option Model.Plane (Element.text "Plane")
                    , Input.option Model.Train (Element.text "Train")
                    , Input.option Model.Coach (Element.text "Coach")
                    , Input.option Model.Bike (Element.text "Bike")
                    , Input.option Model.Boat (Element.text "Boat")
                    , Input.option Model.Ferry (Element.text "Ferry")
                    , Input.option Model.DuckWalk (Element.text "DuckWalk")
                    ]
                , selected = Maybe.Just value
                , label = Input.labelHidden ""
                }
    in
    Element.el [ padding, Element.alignTop, Border.width 1 ] variantRow


conditionEditor : Model.Condition -> Element.Element Model.Condition
conditionEditor value =
    let
        inputsRow =
            case value of
                Model.ConditionNot condition ->
                    [ Element.map
                        (\newValue -> Model.ConditionNot newValue)
                        (conditionEditor condition)
                    ]

                Model.ConditionAnd listCondition ->
                    [ Element.map
                        (\newValue -> Model.ConditionAnd newValue)
                        (listEditor
                            conditionEditor
                            conditionDefault
                            listCondition
                        )
                    ]

                Model.ConditionOr listCondition ->
                    [ Element.map
                        (\newValue -> Model.ConditionOr newValue)
                        (listEditor
                            conditionEditor
                            conditionDefault
                            listCondition
                        )
                    ]

                Model.HasItem itemName ->
                    [ Element.map
                        (\newValue -> Model.HasItem newValue)
                        (itemNameEditor itemName)
                    ]

                Model.LocalFlag string ->
                    [ Element.map
                        (\newValue -> Model.LocalFlag newValue)
                        (stringEditor string)
                    ]

        variantRow =
            Input.radioRow
                [ spacing ]
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
                        (Element.text "HasItem")
                    , Input.option
                        (Model.LocalFlag stringExtracted)
                        (Element.text "LocalFlag")
                    ]
                , selected = Maybe.Just value
                , label = Input.labelHidden ""
                }

        extractedDefault =
            { conditionExtracted = conditionDefault
            , itemNameExtracted = itemNameDefault
            , listConditionExtracted = []
            , stringExtracted = ""
            }

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
    in
    Element.column
        [ padding, spacing, Element.alignTop, Border.width 1 ]
        [ variantRow, Element.row [ spacing ] inputsRow ]


itemNameEditor : Model.ItemName -> Element.Element Model.ItemName
itemNameEditor value =
    let
        inputsRow =
            case value of
                Model.GenericItemName string ->
                    [ Element.map
                        (\newValue -> Model.GenericItemName newValue)
                        (stringEditor string)
                    ]

                Model.TicketName fromCityNametoCityNamekindTransportKind ->
                    [ Element.map
                        (\newValue -> Model.TicketName newValue)
                        (Element.table
                            [ spacing ]
                            { data =
                                [ ( "From"
                                  , Element.map
                                        (\lambdaArg0 ->
                                            { value | from = lambdaArg0 }
                                        )
                                        (cityNameEditor
                                            fromCityNametoCityNamekindTransportKind.from
                                        )
                                  )
                                , ( "To"
                                  , Element.map
                                        (\lambdaArg0 ->
                                            { value | to = lambdaArg0 }
                                        )
                                        (cityNameEditor
                                            fromCityNametoCityNamekindTransportKind.to
                                        )
                                  )
                                , ( "Kind"
                                  , Element.map
                                        (\lambdaArg0 ->
                                            { value | kind = lambdaArg0 }
                                        )
                                        (transportKindEditor
                                            fromCityNametoCityNamekindTransportKind.kind
                                        )
                                  )
                                ]
                            , columns =
                                [ { header = Element.none
                                  , width = Element.shrink
                                  , view = \( name, _ ) -> Element.text name
                                  }
                                , { header = Element.none
                                  , width = Element.fill
                                  , view = \( _, view ) -> view
                                  }
                                ]
                            }
                        )
                    ]

        variantRow =
            Input.radioRow
                [ spacing ]
                { onChange = Basics.identity
                , options =
                    [ Input.option
                        (Model.GenericItemName stringExtracted)
                        (Element.text "GenericItemName")
                    , Input.option
                        (Model.TicketName
                            fromCityNametoCityNamekindTransportKindExtracted
                        )
                        (Element.text "TicketName")
                    ]
                , selected = Maybe.Just value
                , label = Input.labelHidden ""
                }

        extractedDefault =
            { fromCityNametoCityNamekindTransportKindExtracted =
                { from = cityNameDefault
                , to = cityNameDefault
                , kind = transportKindDefault
                }
            , stringExtracted = ""
            }

        { fromCityNametoCityNamekindTransportKindExtracted, stringExtracted } =
            case value of
                Model.GenericItemName string ->
                    { extractedDefault | stringExtracted = string }

                Model.TicketName fromCityNametoCityNamekindTransportKind ->
                    { extractedDefault
                        | fromCityNametoCityNamekindTransportKindExtracted =
                            fromCityNametoCityNamekindTransportKind
                    }
    in
    Element.column
        [ padding, spacing, Element.alignTop, Border.width 1 ]
        [ variantRow, Element.row [ spacing ] inputsRow ]


dataDefault : Model.Data
dataDefault =
    Dict.empty


cityDefault : Model.City
cityDefault =
    { name = cityNameDefault, text = "", image = "", people = [] }


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


rythm : Int
rythm =
    10


spacing : Element.Attribute msg
spacing =
    Element.spacing rythm


padding : Element.Attribute msg
padding =
    Element.padding rythm


stringEditor : String -> Element.Element String.String
stringEditor value =
    Input.text
        [ Element.alignTop ]
        { onChange = Basics.identity
        , text = value
        , placeholder = Maybe.Nothing
        , label = Input.labelHidden ""
        }


boolEditor value =
    Input.radioRow
        [ spacing, Element.alignTop ]
        { onChange = Basics.identity
        , options =
            [ Input.option True (Element.text "True")
            , Input.option False (Element.text "False")
            ]
        , selected = Maybe.Just value
        , label = Input.labelHidden ""
        }


listEditor : (e -> Element.Element e) -> e -> List e -> Element.Element (List e)
listEditor valueEditor valueDefault value =
    Element.column
        [ spacing, padding, Element.alignTop, Border.width 1 ]
        (Debug.todo "TODO: listEditor rows")


dictEditor :
    (k -> Element.Element k)
    -> (v -> Element.Element v)
    -> Dict.Dict k v
    -> Element.Element (Dict.Dict k v)
dictEditor keyEditor valueEditor value =
    Element.table
        [ spacing, padding, Element.alignTop, Border.width 1 ]
        { data = Dict.toList value
        , columns = Debug.todo "TODO: dictEditor columns"
        }
