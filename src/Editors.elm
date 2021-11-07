module Editors exposing (cityEditor, cityDefault)

{-|

@docs cityEditor, cityDefault

-}

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import List.Extra
import Model


cityEditor : Model.City -> Element.Element Model.City
cityEditor value =
    Element.table
        [ Element.width Element.fill
        , spacing
        , padding
        , Element.alignTop
        , Border.width 1
        ]
        { data =
            [ ( "Name"
              , Element.map
                    (\lambdaArg0 ->
                        let
                            updating =
                                value
                        in
                        { updating | name = lambdaArg0 }
                    )
                    (cityNameEditor value.name)
              )
            , ( "Text"
              , Element.map
                    (\lambdaArg0 ->
                        let
                            updating =
                                value
                        in
                        { updating | text = lambdaArg0 }
                    )
                    (stringEditor value.text)
              )
            , ( "Image"
              , Element.map
                    (\lambdaArg0 ->
                        let
                            updating =
                                value
                        in
                        { updating | image = lambdaArg0 }
                    )
                    (stringEditor value.image)
              )
            , ( "People"
              , Element.map
                    (\lambdaArg0 ->
                        let
                            updating =
                                value
                        in
                        { updating | people = lambdaArg0 }
                    )
                    (listEditor personEditor personDefault value.people)
              )
            ]
        , columns =
            [ { header = Element.none
              , width = Element.shrink
              , view =
                    \( name, _ ) ->
                        Element.el [ Element.centerY ] (Element.text name)
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
        [ Element.width Element.fill
        , spacing
        , padding
        , Element.alignTop
        , Border.width 1
        ]
        { data =
            [ ( "Name"
              , Element.map
                    (\lambdaArg0 ->
                        let
                            updating =
                                value
                        in
                        { updating | name = lambdaArg0 }
                    )
                    (stringEditor value.name)
              )
            , ( "Image"
              , Element.map
                    (\lambdaArg0 ->
                        let
                            updating =
                                value
                        in
                        { updating | image = lambdaArg0 }
                    )
                    (stringEditor value.image)
              )
            , ( "Dialog"
              , Element.map
                    (\lambdaArg0 ->
                        let
                            updating =
                                value
                        in
                        { updating | dialog = lambdaArg0 }
                    )
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
              , view =
                    \( name, _ ) ->
                        Element.el [ Element.centerY ] (Element.text name)
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
        [ Element.width Element.fill
        , spacing
        , padding
        , Element.alignTop
        , Border.width 1
        ]
        { data =
            [ ( "Text"
              , Element.map
                    (\lambdaArg0 ->
                        let
                            updating =
                                value
                        in
                        { updating | text = lambdaArg0 }
                    )
                    (stringEditor value.text)
              )
            , ( "Choices"
              , Element.map
                    (\lambdaArg0 ->
                        let
                            updating =
                                value
                        in
                        { updating | choices = lambdaArg0 }
                    )
                    (listEditor choiceEditor choiceDefault value.choices)
              )
            ]
        , columns =
            [ { header = Element.none
              , width = Element.shrink
              , view =
                    \( name, _ ) ->
                        Element.el [ Element.centerY ] (Element.text name)
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
        [ Element.width Element.fill
        , spacing
        , padding
        , Element.alignTop
        , Border.width 1
        ]
        { data =
            [ ( "Text"
              , Element.map
                    (\lambdaArg0 ->
                        let
                            updating =
                                value
                        in
                        { updating | text = lambdaArg0 }
                    )
                    (stringEditor value.text)
              )
            , ( "Next"
              , Element.map
                    (\lambdaArg0 ->
                        let
                            updating =
                                value
                        in
                        { updating | next = lambdaArg0 }
                    )
                    (idEditor value.next)
              )
            , ( "Consequences"
              , Element.map
                    (\lambdaArg0 ->
                        let
                            updating =
                                value
                        in
                        { updating | consequences = lambdaArg0 }
                    )
                    (listEditor
                        consequenceEditor
                        consequenceDefault
                        value.consequences
                    )
              )
            , ( "Condition"
              , Element.map
                    (\lambdaArg0 ->
                        let
                            updating =
                                value
                        in
                        { updating | condition = lambdaArg0 }
                    )
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
              , view =
                    \( name, _ ) ->
                        Element.el [ Element.centerY ] (Element.text name)
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
                [ spacing ]
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
                    [ Element.map Model.ConsequenceGetMoney (intEditor int) ]

                Model.ConsequenceLoseMoney int ->
                    [ Element.map Model.ConsequenceLoseMoney (intEditor int) ]

                Model.ConsequenceGetItem item ->
                    [ Element.map Model.ConsequenceGetItem (itemEditor item) ]

                Model.ConsequenceLoseItem string ->
                    [ Element.map
                        Model.ConsequenceLoseItem
                        (stringEditor string)
                    ]

                Model.ConsequenceSetLocalFlag string bool ->
                    [ Element.map
                        (\lambdaArg0 ->
                            Model.ConsequenceSetLocalFlag lambdaArg0 bool
                        )
                        (stringEditor string)
                    , Element.map
                        (Model.ConsequenceSetLocalFlag string)
                        (boolEditor bool)
                    ]
    in
    Element.column
        [ spacing, padding, Element.alignTop, Border.width 1 ]
        [ variantRow, Element.row [ spacing ] inputsRow ]


itemEditor : Model.Item -> Element.Element Model.Item
itemEditor value =
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
                [ spacing ]
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
                        (Element.table
                            [ Element.width Element.fill
                            , spacing
                            , padding
                            , Element.alignTop
                            , Border.width 1
                            ]
                            { data =
                                [ ( "Name"
                                  , Element.map
                                        (\lambdaArg0 ->
                                            let
                                                updating =
                                                    nameStringimageString
                                            in
                                            { updating | name = lambdaArg0 }
                                        )
                                        (stringEditor nameStringimageString.name)
                                  )
                                , ( "Image"
                                  , Element.map
                                        (\lambdaArg0 ->
                                            let
                                                updating =
                                                    nameStringimageString
                                            in
                                            { updating | image = lambdaArg0 }
                                        )
                                        (stringEditor
                                            nameStringimageString.image
                                        )
                                  )
                                ]
                            , columns =
                                [ { header = Element.none
                                  , width = Element.shrink
                                  , view =
                                        \( name, _ ) ->
                                            Element.el
                                                [ Element.centerY ]
                                                (Element.text name)
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
                        Model.Ticket
                        (Element.table
                            [ Element.width Element.fill
                            , spacing
                            , padding
                            , Element.alignTop
                            , Border.width 1
                            ]
                            { data =
                                [ ( "From"
                                  , Element.map
                                        (\lambdaArg0 ->
                                            let
                                                updating =
                                                    fromCityNametoCityNamekindTransportKindconsequencesListConsequence
                                            in
                                            { updating | from = lambdaArg0 }
                                        )
                                        (cityNameEditor
                                            fromCityNametoCityNamekindTransportKindconsequencesListConsequence.from
                                        )
                                  )
                                , ( "To"
                                  , Element.map
                                        (\lambdaArg0 ->
                                            let
                                                updating =
                                                    fromCityNametoCityNamekindTransportKindconsequencesListConsequence
                                            in
                                            { updating | to = lambdaArg0 }
                                        )
                                        (cityNameEditor
                                            fromCityNametoCityNamekindTransportKindconsequencesListConsequence.to
                                        )
                                  )
                                , ( "Kind"
                                  , Element.map
                                        (\lambdaArg0 ->
                                            let
                                                updating =
                                                    fromCityNametoCityNamekindTransportKindconsequencesListConsequence
                                            in
                                            { updating | kind = lambdaArg0 }
                                        )
                                        (transportKindEditor
                                            fromCityNametoCityNamekindTransportKindconsequencesListConsequence.kind
                                        )
                                  )
                                , ( "Consequences"
                                  , Element.map
                                        (\lambdaArg0 ->
                                            let
                                                updating =
                                                    fromCityNametoCityNamekindTransportKindconsequencesListConsequence
                                            in
                                            { updating
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
                                  , view =
                                        \( name, _ ) ->
                                            Element.el
                                                [ Element.centerY ]
                                                (Element.text name)
                                  }
                                , { header = Element.none
                                  , width = Element.fill
                                  , view = \( _, view ) -> view
                                  }
                                ]
                            }
                        )
                    ]
    in
    Element.column
        [ spacing, padding, Element.alignTop, Border.width 1 ]
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
                    , Input.option Model.DuckWalk (Element.text "Duck walk")
                    ]
                , selected = Maybe.Just value
                , label = Input.labelHidden ""
                }
    in
    Element.el [ padding, Element.alignTop, Border.width 1 ] variantRow


conditionEditor : Model.Condition -> Element.Element Model.Condition
conditionEditor value =
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
                    [ Element.map Model.ConditionNot (conditionEditor condition)
                    ]

                Model.ConditionAnd listCondition ->
                    [ Element.map
                        Model.ConditionAnd
                        (listEditor
                            conditionEditor
                            conditionDefault
                            listCondition
                        )
                    ]

                Model.ConditionOr listCondition ->
                    [ Element.map
                        Model.ConditionOr
                        (listEditor
                            conditionEditor
                            conditionDefault
                            listCondition
                        )
                    ]

                Model.HasItem itemName ->
                    [ Element.map Model.HasItem (itemNameEditor itemName) ]

                Model.LocalFlag string ->
                    [ Element.map Model.LocalFlag (stringEditor string) ]
    in
    Element.column
        [ spacing, padding, Element.alignTop, Border.width 1 ]
        [ variantRow, Element.row [ spacing ] inputsRow ]


itemNameEditor : Model.ItemName -> Element.Element Model.ItemName
itemNameEditor value =
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
                [ spacing ]
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
                    [ Element.map Model.GenericItemName (stringEditor string) ]

                Model.TicketName fromCityNametoCityNamekindTransportKind ->
                    [ Element.map
                        Model.TicketName
                        (Element.table
                            [ Element.width Element.fill
                            , spacing
                            , padding
                            , Element.alignTop
                            , Border.width 1
                            ]
                            { data =
                                [ ( "From"
                                  , Element.map
                                        (\lambdaArg0 ->
                                            let
                                                updating =
                                                    fromCityNametoCityNamekindTransportKind
                                            in
                                            { updating | from = lambdaArg0 }
                                        )
                                        (cityNameEditor
                                            fromCityNametoCityNamekindTransportKind.from
                                        )
                                  )
                                , ( "To"
                                  , Element.map
                                        (\lambdaArg0 ->
                                            let
                                                updating =
                                                    fromCityNametoCityNamekindTransportKind
                                            in
                                            { updating | to = lambdaArg0 }
                                        )
                                        (cityNameEditor
                                            fromCityNametoCityNamekindTransportKind.to
                                        )
                                  )
                                , ( "Kind"
                                  , Element.map
                                        (\lambdaArg0 ->
                                            let
                                                updating =
                                                    fromCityNametoCityNamekindTransportKind
                                            in
                                            { updating | kind = lambdaArg0 }
                                        )
                                        (transportKindEditor
                                            fromCityNametoCityNamekindTransportKind.kind
                                        )
                                  )
                                ]
                            , columns =
                                [ { header = Element.none
                                  , width = Element.shrink
                                  , view =
                                        \( name, _ ) ->
                                            Element.el
                                                [ Element.centerY ]
                                                (Element.text name)
                                  }
                                , { header = Element.none
                                  , width = Element.fill
                                  , view = \( _, view ) -> view
                                  }
                                ]
                            }
                        )
                    ]
    in
    Element.column
        [ spacing, padding, Element.alignTop, Border.width 1 ]
        [ variantRow, Element.row [ spacing ] inputsRow ]


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


intEditor : Int -> Element.Element Basics.Int
intEditor value =
    Element.map
        (\lambdaArg0 -> lambdaArg0 |> String.toInt |> Maybe.withDefault value)
        (Input.text
            [ Element.width (Element.minimum 100 Element.fill)
            , Element.alignTop
            ]
            { onChange = Basics.identity
            , text = String.fromInt value
            , placeholder = Maybe.Nothing
            , label = Input.labelHidden ""
            }
        )


tupleEditor :
    (l -> Element.Element l)
    -> l
    -> (r -> Element.Element r)
    -> r
    -> ( l, r )
    -> Element.Element ( l, r )
tupleEditor leftEditor _ rightEditor _ ( left, right ) =
    Element.row
        [ Element.width Element.fill
        , spacing
        , padding
        , Element.alignTop
        , Border.width 1
        ]
        [ Element.map (\lambdaArg0 -> ( lambdaArg0, right )) (leftEditor left)
        , Element.map (\lambdaArg0 -> ( left, lambdaArg0 )) (rightEditor right)
        ]


maybeEditor : (e -> Element.Element e) -> e -> Maybe e -> Element.Element (Maybe e)
maybeEditor valueEditor valueDefault value =
    let
        extracted =
            case value of
                Nothing ->
                    valueDefault

                Just inner ->
                    inner

        variantRow =
            Input.radioRow
                [ spacing ]
                { onChange = Basics.identity
                , options =
                    [ Input.option Nothing (Element.text "Nothing")
                    , Input.option (Maybe.Just extracted) (Element.text "Just")
                    ]
                , selected = Maybe.Just value
                , label = Input.labelHidden ""
                }

        inputsRow =
            case value of
                Nothing ->
                    Element.none

                Just inner ->
                    Element.map Maybe.Just (valueEditor inner)
    in
    Element.column
        [ spacing, padding, Element.alignTop, Border.width 1 ]
        [ variantRow, inputsRow ]


stringEditor : String -> Element.Element String.String
stringEditor value =
    Input.text
        [ Element.width (Element.minimum 100 Element.fill), Element.alignTop ]
        { onChange = Basics.identity
        , text = value
        , placeholder = Maybe.Nothing
        , label = Input.labelHidden ""
        }


boolEditor : Bool -> Element.Element Basics.Bool
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
                            [ Input.button
                                [ spacing
                                , padding
                                , Element.alignTop
                                , Border.width 1
                                , Border.color (Element.rgb 0 0 0)
                                , Background.color (Element.rgb 1 0.6 0.6)
                                , Element.alignRight
                                , Border.widthEach
                                    { bottom = 0, left = 1, right = 1, top = 1 }
                                ]
                                { onPress = Maybe.Just valueDefault
                                , label = Element.text "Delete"
                                }
                            , valueEditor row
                            ]
                        )
                )
                value
                ++ [ Input.button
                        [ Element.alignRight
                        , spacing
                        , padding
                        , Element.alignTop
                        , Border.width 1
                        , Border.color (Element.rgb 0 0 0)
                        , Background.color (Element.rgb 0.6 1 0.6)
                        ]
                        { onPress = Maybe.Just (value ++ [ valueDefault ])
                        , label = Element.text "Add new"
                        }
                   ]
    in
    Element.column
        [ Element.width Element.fill
        , spacing
        , padding
        , Element.alignTop
        , Border.width 1
        ]
        rows
