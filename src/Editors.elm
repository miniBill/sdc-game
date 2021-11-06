module Editors exposing (choiceEditor, cityEditor, cityNameEditor, conditionEditor, consequenceEditor, dataEditor, dialogEditor, idEditor, itemEditor, itemNameEditor, personEditor, transportKindEditor)

{-| 

@docs dataEditor, cityEditor, cityNameEditor, personEditor, idEditor, dialogEditor, choiceEditor, consequenceEditor, itemEditor, transportKindEditor, conditionEditor, itemNameEditor


-}


import Debug
import Dict
import Element
import Element.Input as Input
import Model


dataEditor : Model.Data -> Element.Element Model.Data
dataEditor value =
    dictEditor idEditor cityEditor value


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
                    (listEditor personEditor value.people)
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
                         (Debug.todo "branch 'Tuple _ _' not implemented")
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
                    (listEditor choiceEditor value.choices)
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
                    (listEditor consequenceEditor value.consequences)
              )
            , ( "Condition"
              , Element.map
                    (\lambdaArg0 -> { value | condition = lambdaArg0 })
                    (Debug.todo "branch 'Maybe _' not implemented"
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
    Element.column [] []


itemEditor : Model.Item -> Element.Element Model.Item
itemEditor value =
    Element.column [] []


transportKindEditor : Model.TransportKind -> Element.Element Model.TransportKind
transportKindEditor value =
    Element.column [] []


conditionEditor : Model.Condition -> Element.Element Model.Condition
conditionEditor value =
    Element.column [] []


itemNameEditor : Model.ItemName -> Element.Element Model.ItemName
itemNameEditor value =
    Element.column [] []


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
        []
        { onChange = Basics.identity
        , text = value
        , placeholder = Nothing
        , label = Input.labelHidden ""
        }


dictEditor :
    (k -> Element.Element k)
    -> (v -> Element.Element v)
    -> Dict.Dict k v
    -> Element.Element (Dict.Dict k v)
dictEditor keyEditor valueEditor value =
    Element.table
        [ spacing ]
        { data = Dict.toList value
        , columns = Debug.todo "TODO: dictEditor columns"
        }


