module Editors exposing (..)

{-| -}

import Dict
import Element
import Element.Input as Input
import List.Extra


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
                (Element.map
                    (\newValue -> List.Extra.setAt newValue value)
                    valueEditor
                )
                value
                ++ [ Element.map
                        (\newValue -> value ++ [ newValue ])
                        (valueEditor valueDefault)
                   ]
    in
    Element.column [ spacing, padding, Element.alignTop, Border.width 1 ] rows


dictEditor :
    (k -> Element.Element k)
    -> k
    -> (v -> Element.Element v)
    -> v
    -> Dict.Dict k v
    -> Element.Element (Dict.Dict k v)
dictEditor keyEditor keyDefault valueEditor valueDefault value =
    let
        keysColumn =
            { header = Element.none
            , width = Element.shrink
            , view =
                \( key, memberValue ) ->
                    Element.map
                        (\newKey ->
                            if
                                newKey
                                    == keyDefault
                                    && memberValue
                                    == valueDefault
                            then
                                Dict.remove key value

                            else
                                Dict.insert
                                    newKey
                                    memberValue
                                    (Dict.remove key value)
                        )
                        keyEditor
            }

        valuesColumn =
            { header = Element.none
            , width = Element.fill
            , view =
                \( key, _ ) ->
                    Element.map
                        (\newValue ->
                            if key == keyDefault && newValue == valueDefault then
                                Dict.remove key value

                            else
                                Dict.insert key newValue value
                        )
                        valueEditor
            }
    in
    Element.table
        [ spacing, padding, Element.alignTop, Border.width 1 ]
        { data = Dict.toList value ++ [ ( keyDefault, valueDefault ) ]
        , columns = [ keysColumn, valuesColumn ]
        }
