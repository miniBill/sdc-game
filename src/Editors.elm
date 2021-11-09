module Editors exposing (choiceDefault, choiceEditor, cityDefault, cityEditor, cityNameDefault, cityNameEditor, conditionDefault, conditionEditor, consequenceDefault, consequenceEditor, coordinatesDefault, coordinatesEditor, dataDefault, dataEditor, dialogDefault, dialogEditor, idDefault, idEditor, itemDefault, itemEditor, itemNameDefault, itemNameEditor, personDefault, personEditor, transportKindDefault, transportKindEditor)

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
import Tuple


dataEditor : Int -> Model.Data -> ( Element.Element Model.Data, Bool )
dataEditor level value =
    dictEditor idEditor idDefault cityEditor cityDefault level value


cityEditor : Int -> Model.City -> ( Element.Element Model.City, Bool )
cityEditor level value =
    let
        raw =
            [ let
                ( editor, simple ) =
                  cityNameEditor (level + 1) value.name
              in
              ( "Name"
              , Element.map
                  (\lambdaArg0 ->
                      let
                          updating =
                              value
                      in
                      { updating | name = lambdaArg0 }
                  )
                  editor
              , simple
              )
            , let
                ( editor, simple ) =
                  stringEditor (level + 1) value.text
              in
              ( "Text"
              , Element.map
                  (\lambdaArg0 ->
                      let
                          updating =
                              value
                      in
                      { updating | text = lambdaArg0 }
                  )
                  editor
              , simple
              )
            , let
                ( editor, simple ) =
                  stringEditor (level + 1) value.image
              in
              ( "Image"
              , Element.map
                  (\lambdaArg0 ->
                      let
                          updating =
                              value
                      in
                      { updating | image = lambdaArg0 }
                  )
                  editor
              , simple
              )
            , let
                ( editor, simple ) =
                  coordinatesEditor (level + 1) value.coordinates
              in
              ( "Coordinates"
              , Element.map
                  (\lambdaArg0 ->
                      let
                          updating =
                              value
                      in
                      { updating | coordinates = lambdaArg0 }
                  )
                  editor
              , simple
              )
            , let
                ( editor, simple ) =
                  listEditor
                      "Person"
                      personEditor
                      personDefault
                      (level + 1)
                      value.people
              in
              ( "People"
              , Element.map
                  (\lambdaArg0 ->
                      let
                          updating =
                              value
                      in
                      { updating | people = lambdaArg0 }
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
                    |> List.concatMap
                        (\pair -> [ Tuple.first pair, Tuple.second pair ])
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
        , Background.color (getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Theme.rythm
        ]
        (simplesTable :: complexes)
    , Basics.False
    )


coordinatesEditor :
    Int -> Model.Coordinates -> ( Element.Element Model.Coordinates, Bool )
coordinatesEditor level value =
    let
        raw =
            [ let
                ( editor, simple ) =
                  floatEditor (level + 1) value.north
              in
              ( "North"
              , Element.map
                  (\lambdaArg0 ->
                      let
                          updating =
                              value
                      in
                      { updating | north = lambdaArg0 }
                  )
                  editor
              , simple
              )
            , let
                ( editor, simple ) =
                  floatEditor (level + 1) value.east
              in
              ( "East"
              , Element.map
                  (\lambdaArg0 ->
                      let
                          updating =
                              value
                      in
                      { updating | east = lambdaArg0 }
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
                    |> List.concatMap
                        (\pair -> [ Tuple.first pair, Tuple.second pair ])
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
        , Background.color (getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Theme.rythm
        ]
        (simplesTable :: complexes)
    , Basics.False
    )


cityNameEditor :
    Int -> Model.CityName -> ( Element.Element Model.CityName, Bool )
cityNameEditor level value =
    stringEditor level value


personEditor : Int -> Model.Person -> ( Element.Element Model.Person, Bool )
personEditor level value =
    let
        raw =
            [ let
                ( editor, simple ) =
                  stringEditor (level + 1) value.name
              in
              ( "Name"
              , Element.map
                  (\lambdaArg0 ->
                      let
                          updating =
                              value
                      in
                      { updating | name = lambdaArg0 }
                  )
                  editor
              , simple
              )
            , let
                ( editor, simple ) =
                  stringEditor (level + 1) value.image
              in
              ( "Image"
              , Element.map
                  (\lambdaArg0 ->
                      let
                          updating =
                              value
                      in
                      { updating | image = lambdaArg0 }
                  )
                  editor
              , simple
              )
            , let
                ( editor, simple ) =
                  listEditor
                      "(Id, Dialog)"
                      (tupleEditor idEditor idDefault dialogEditor dialogDefault
                      )
                      ( idDefault, dialogDefault )
                      (level + 1)
                      value.dialog
              in
              ( "Dialog"
              , Element.map
                  (\lambdaArg0 ->
                      let
                          updating =
                              value
                      in
                      { updating | dialog = lambdaArg0 }
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
                    |> List.concatMap
                        (\pair -> [ Tuple.first pair, Tuple.second pair ])
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
        , Background.color (getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Theme.rythm
        ]
        (simplesTable :: complexes)
    , Basics.False
    )


idEditor : Int -> Model.Id -> ( Element.Element Model.Id, Bool )
idEditor level value =
    stringEditor level value


dialogEditor : Int -> Model.Dialog -> ( Element.Element Model.Dialog, Bool )
dialogEditor level value =
    let
        raw =
            [ let
                ( editor, simple ) =
                  stringEditor (level + 1) value.text
              in
              ( "Text"
              , Element.map
                  (\lambdaArg0 ->
                      let
                          updating =
                              value
                      in
                      { updating | text = lambdaArg0 }
                  )
                  editor
              , simple
              )
            , let
                ( editor, simple ) =
                  listEditor
                      "Choice"
                      choiceEditor
                      choiceDefault
                      (level + 1)
                      value.choices
              in
              ( "Choices"
              , Element.map
                  (\lambdaArg0 ->
                      let
                          updating =
                              value
                      in
                      { updating | choices = lambdaArg0 }
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
                    |> List.concatMap
                        (\pair -> [ Tuple.first pair, Tuple.second pair ])
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
        , Background.color (getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Theme.rythm
        ]
        (simplesTable :: complexes)
    , Basics.False
    )


choiceEditor : Int -> Model.Choice -> ( Element.Element Model.Choice, Bool )
choiceEditor level value =
    let
        raw =
            [ let
                ( editor, simple ) =
                  stringEditor (level + 1) value.text
              in
              ( "Text"
              , Element.map
                  (\lambdaArg0 ->
                      let
                          updating =
                              value
                      in
                      { updating | text = lambdaArg0 }
                  )
                  editor
              , simple
              )
            , let
                ( editor, simple ) =
                  idEditor (level + 1) value.next
              in
              ( "Next"
              , Element.map
                  (\lambdaArg0 ->
                      let
                          updating =
                              value
                      in
                      { updating | next = lambdaArg0 }
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
                      (level + 1)
                      value.consequences
              in
              ( "Consequences"
              , Element.map
                  (\lambdaArg0 ->
                      let
                          updating =
                              value
                      in
                      { updating | consequences = lambdaArg0 }
                  )
                  editor
              , simple
              )
            , let
                ( editor, simple ) =
                  maybeEditor
                      "Condition"
                      conditionEditor
                      conditionDefault
                      (level + 1)
                      value.condition
              in
              ( "Condition"
              , Element.map
                  (\lambdaArg0 ->
                      let
                          updating =
                              value
                      in
                      { updating | condition = lambdaArg0 }
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
                    |> List.concatMap
                        (\pair -> [ Tuple.first pair, Tuple.second pair ])
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
        , Background.color (getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Theme.rythm
        ]
        (simplesTable :: complexes)
    , Basics.False
    )


consequenceEditor :
    Int -> Model.Consequence -> ( Element.Element Model.Consequence, Bool )
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
    , Basics.False
    )


itemEditor : Int -> Model.Item -> ( Element.Element Model.Item, Bool )
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
                                           let
                                               updating =
                                                   nameStringimageString
                                           in
                                           { updating | name = lambdaArg0 }
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
                                           let
                                               updating =
                                                   nameStringimageString
                                           in
                                           { updating | image = lambdaArg0 }
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
                                                         (Element.text fieldName
                                                         )
                                                     , fieldEditor
                                                     )

                                             else
                                                 Maybe.Nothing
                                         )

                              simplesTable =
                                 if List.length simples <= 2 then
                                     simples
                                         |> List.concatMap
                                             (\pair ->
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
                               , Background.color (getColor (level + 1))
                               , Element.width Element.fill
                               , Theme.spacing
                               , Theme.padding
                               , Element.alignTop
                               , Border.width 1
                               , Border.rounded Theme.rythm
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
                                           let
                                               updating =
                                                   fromCityNametoCityNamekindTransportKindconsequencesListConsequence
                                           in
                                           { updating | from = lambdaArg0 }
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
                                           let
                                               updating =
                                                   fromCityNametoCityNamekindTransportKindconsequencesListConsequence
                                           in
                                           { updating | to = lambdaArg0 }
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
                                           let
                                               updating =
                                                   fromCityNametoCityNamekindTransportKindconsequencesListConsequence
                                           in
                                           { updating | kind = lambdaArg0 }
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
                                           let
                                               updating =
                                                   fromCityNametoCityNamekindTransportKindconsequencesListConsequence
                                           in
                                           { updating
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
                                                         (Element.text fieldName
                                                         )
                                                     , fieldEditor
                                                     )

                                             else
                                                 Maybe.Nothing
                                         )

                              simplesTable =
                                 if List.length simples <= 2 then
                                     simples
                                         |> List.concatMap
                                             (\pair ->
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
                               , Background.color (getColor (level + 1))
                               , Element.width Element.fill
                               , Theme.spacing
                               , Theme.padding
                               , Element.alignTop
                               , Border.width 1
                               , Border.rounded Theme.rythm
                               ]
                               (simplesTable :: complexes)
                           , Basics.False
                           )
                          )
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
    , Basics.False
    )


transportKindEditor :
    Int -> Model.TransportKind -> ( Element.Element Model.TransportKind, Bool )
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
          [ Background.color (getColor level)
          , Element.width Element.fill
          , Theme.spacing
          , Theme.padding
          , Element.alignTop
          , Border.width 1
          , Border.rounded Theme.rythm
          ]
          variantRow
    , Basics.False
    )


conditionEditor :
    Int -> Model.Condition -> ( Element.Element Model.Condition, Bool )
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
    , Basics.False
    )


itemNameEditor :
    Int -> Model.ItemName -> ( Element.Element Model.ItemName, Bool )
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
                                           let
                                               updating =
                                                   fromCityNametoCityNamekindTransportKind
                                           in
                                           { updating | from = lambdaArg0 }
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
                                           let
                                               updating =
                                                   fromCityNametoCityNamekindTransportKind
                                           in
                                           { updating | to = lambdaArg0 }
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
                                           let
                                               updating =
                                                   fromCityNametoCityNamekindTransportKind
                                           in
                                           { updating | kind = lambdaArg0 }
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
                                                         (Element.text fieldName
                                                         )
                                                     , fieldEditor
                                                     )

                                             else
                                                 Maybe.Nothing
                                         )

                              simplesTable =
                                 if List.length simples <= 2 then
                                     simples
                                         |> List.concatMap
                                             (\pair ->
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
                               , Background.color (getColor (level + 1))
                               , Element.width Element.fill
                               , Theme.spacing
                               , Theme.padding
                               , Element.alignTop
                               , Border.width 1
                               , Border.rounded Theme.rythm
                               ]
                               (simplesTable :: complexes)
                           , Basics.False
                           )
                          )
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
    , Basics.False
    )


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


intEditor : Int -> Int -> ( Element.Element Basics.Int, Bool )
intEditor level value =
    ( Element.map
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
    , Basics.True
    )


floatEditor : Int -> Float -> ( Element.Element Basics.Float, Bool )
floatEditor level value =
    ( Element.map
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
    , Basics.True
    )


tupleEditor :
    (Int -> l -> ( Element.Element l, Bool ))
    -> l
    -> (Int -> r -> ( Element.Element r, Bool ))
    -> r
    -> Int
    -> ( l, r )
    -> ( Element.Element ( l, r ), Bool )
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
                [ Background.color (getColor level)
                , Element.width Element.fill
                , Theme.spacing
                , Theme.padding
                , Element.alignTop
                , Border.width 1
                , Border.rounded Theme.rythm
                ]
                [ Element.map (\lambdaArg0 -> ( lambdaArg0, right )) le
                , Element.map (\lambdaArg0 -> ( left, lambdaArg0 )) re
                ]
    in
    ( editor, Basics.False )


maybeEditor :
    String
    -> (Int -> e -> ( Element.Element e, Bool ))
    -> e
    -> Int
    -> Maybe e
    -> ( Element.Element (Maybe e), Bool )
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
                    Element.map
                        Maybe.Just
                        (Tuple.first (valueEditor (level + 1) inner))
    in
    ( Element.column
        [ Background.color (getColor level)
        , Element.width Element.fill
        , Theme.spacing
        , Theme.padding
        , Element.alignTop
        , Border.width 1
        , Border.rounded Theme.rythm
        ]
        [ variantRow, inputsRow ]
    , Basics.False
    )


stringEditor : Int -> String -> ( Element.Element String.String, Bool )
stringEditor level value =
    ( Input.text
        [ Element.width (Element.minimum 100 Element.fill)
        , Element.alignTop
        , Background.color (getColor level)
        ]
        { onChange = Basics.identity
        , text = value
        , placeholder = Maybe.Nothing
        , label = Input.labelHidden ""
        }
    , Basics.True
    )


boolEditor : Int -> Bool -> ( Element.Element Basics.Bool, Bool )
boolEditor level value =
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
    -> (Int -> e -> ( Element.Element e, Bool ))
    -> e
    -> Int
    -> List e
    -> ( Element.Element (List e), Bool )
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
                            , Tuple.first (valueEditor (level + 1) row)
                            ]
                        )
                )
                value
    in
    ( Element.column
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
    , Basics.False
    )


dictEditor :
    (Int -> comparable -> ( Element.Element comparable, Bool ))
    -> comparable
    -> (Int -> v -> ( Element.Element v, Bool ))
    -> v
    -> Int
    -> Dict.Dict comparable v
    -> ( Element.Element (Dict.Dict comparable v), Bool )
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
                            if key == keyDefault && lambdaArg0 == valueDefault
                            then
                                Dict.remove key value

                            else
                                Dict.insert key lambdaArg0 value
                        )
                        (Tuple.first (valueEditor (level + 1) memberValue))
            }
    in
    ( Element.table
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
    , Basics.False
    )


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


