module Frontend.EditorTheme exposing (Attribute, Context, Element, button, colors, column, customEditor, enumEditor, floatEditor, fontSizes, image, imageXlinkHref, intEditor, listEditor, map, objectEditor, padding, rythm, spacing, stringEditor, tupleEditor)

import Element.WithContext as Element exposing (Color)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Env
import Html.Attributes
import List.Extra
import Model exposing (A11yOptions)
import Svg
import Svg.Attributes
import Types exposing (Size)


type alias Context =
    { screenSize : Size
    , a11y : A11yOptions
    }


type alias Element msg =
    Element.Element Context msg


type alias Attribute msg =
    Element.Attribute Context msg



-- Attributes


borderWidth : number
borderWidth =
    1


borderRounded : Attribute msg
borderRounded =
    Border.rounded rythm


rythm : number
rythm =
    10


padding : Attribute msg
padding =
    Element.padding rythm


spacing : Attribute msg
spacing =
    Element.spacing rythm


colors :
    { addNew : Color
    , delete : Color
    , semitransparent : Color
    , white : Color
    , selectedTab : Color
    , tab : Color
    }
colors =
    { addNew = Element.rgb255 0xDE 0xFD 0xE0
    , delete = Element.rgb255 0xFD 0xDF 0xDF
    , semitransparent = Element.rgba255 0xFF 0xFF 0xFF 0.7
    , white = Element.rgb255 0xFF 0xFF 0xFF
    , selectedTab = Element.rgb255 0xD0 0xD0 0xFF
    , tab = Element.rgb255 0xD0 0xD0 0xD0
    }


getColor : Int -> Element.Color
getColor index =
    case modBy 5 index of
        0 ->
            Element.rgb255 0xFD 0xDF 0xDF

        1 ->
            Element.rgb255 0xFC 0xF7 0xDE

        2 ->
            Element.rgb255 0xDE 0xFD 0xE0

        3 ->
            Element.rgb255 0xDE 0xF3 0xFD

        _ ->
            Element.rgb255 0xF0 0xDE 0xFD


fontSizes :
    { huge : Element.Attribute { a | a11y : A11yOptions } msg
    , bigger : Element.Attribute { a | a11y : A11yOptions } msg
    , big : Element.Attribute { a | a11y : A11yOptions } msg
    , normal : Element.Attribute { a | a11y : A11yOptions } msg
    , small : Element.Attribute { a | a11y : A11yOptions } msg
    , smaller : Element.Attribute { a | a11y : A11yOptions } msg
    , tiny : Element.Attribute { a | a11y : A11yOptions } msg
    }
fontSizes =
    let
        size n =
            Element.withAttribute .a11y
                (\a11y ->
                    Font.size <|
                        round <|
                            Element.modular (0.5 * a11y.fontSize) 1.25 n
                )
    in
    { huge = size 4
    , bigger = size 3
    , big = size 2
    , normal = size 1
    , small = size -1
    , smaller = size -2
    , tiny = size -3
    }



-- Containers


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs =
    Element.column ([ padding, spacing ] ++ attrs)



-- Inputs


button :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
button attrs =
    Input.button
        ([ Border.width borderWidth
         , borderRounded
         , padding
         ]
            ++ attrs
        )


tabButton :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
tabButton attrs =
    button
        ([ Border.widthEach { left = borderWidth, top = borderWidth, right = borderWidth, bottom = 0 }
         , Border.roundEach
            { topLeft = rythm
            , topRight = rythm
            , bottomLeft = 0
            , bottomRight = 0
            }
         ]
            ++ attrs
        )


image : List (Attribute msg) -> { description : String, src : String } -> Element msg
image attrs config =
    Element.image attrs { config | src = Env.imageToUrl config.src }


imageXlinkHref : String -> Svg.Attribute msg
imageXlinkHref src =
    Svg.Attributes.xlinkHref <| Env.imageToUrl src



-- Editors


objectEditor : List ( String, Element msg ) -> List ( String, Element msg ) -> Int -> Element msg
objectEditor rawSimples rawComplexes level =
    let
        simpleLabel =
            Element.el
                [ Element.centerY ]
                << Element.text

        simplesTable =
            if List.length rawSimples <= 2 then
                rawSimples
                    |> List.map
                        (\( fieldName, fieldEditor ) ->
                            Element.row
                                [ spacing, Element.width Element.fill ]
                                [ simpleLabel fieldName, fieldEditor ]
                        )
                    |> Element.row [ spacing, Element.width Element.fill ]

            else
                Element.table
                    [ spacing, Element.width Element.fill ]
                    { columns =
                        [ { header = Element.none
                          , width = Element.shrink
                          , view = simpleLabel << Tuple.first
                          }
                        , { header = Element.none
                          , width = Element.fill
                          , view = Tuple.second
                          }
                        ]
                    , data = rawSimples
                    }

        complexes =
            List.concatMap
                (\( fieldName, fieldEditor ) ->
                    [ Element.text fieldName, fieldEditor ]
                )
                rawComplexes
    in
    Element.column
        [ Element.width Element.fill
        , Background.color (getColor level)
        , Element.width Element.fill
        , spacing
        , padding
        , Element.alignTop
        , Border.width 1
        , borderRounded
        ]
        (simplesTable :: complexes)


listEditor :
    String
    -> (Int -> e -> Element e)
    -> e
    -> Int
    -> List e
    -> Element (List e)
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
                                    , right = rythm
                                    , bottom = 0
                                    , left = 0
                                    }
                                , Element.alignRight
                                ]
                                (tabButton
                                    [ spacing
                                    , padding
                                    , Element.alignTop
                                    , Border.width 1
                                    , borderRounded
                                    , Background.gradient
                                        { angle = 0
                                        , steps =
                                            [ getColor (level + 1)
                                            , colors.delete
                                            , colors.delete
                                            ]
                                        }
                                    , Border.widthEach
                                        { bottom = 0
                                        , left = 1
                                        , right = 1
                                        , top = 1
                                        }
                                    , Border.roundEach
                                        { topLeft = rythm
                                        , topRight = rythm
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
                                (valueEditor (level + 1) row)
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
            , spacing
            , padding
            , Element.alignTop
            , Border.width 1
            , borderRounded
            ]
            rows
        , Element.el
            [ Element.paddingEach
                { top = 0, right = rythm, bottom = 0, left = rythm }
            , Element.alignRight
            ]
            (button
                [ spacing
                , padding
                , Element.alignTop
                , Border.width 1
                , borderRounded
                , Background.gradient
                    { angle = 0
                    , steps =
                        [ colors.addNew
                        , colors.addNew
                        , colors.addNew
                        , getColor level
                        ]
                    }
                , Border.widthEach { bottom = 1, left = 1, right = 1, top = 0 }
                , Border.roundEach
                    { topLeft = 0
                    , topRight = 0
                    , bottomLeft = rythm
                    , bottomRight = rythm
                    }
                , Element.moveUp 1
                ]
                { onPress = Just (value ++ [ valueDefault ])
                , label = Element.text ("Add new " ++ typeName)
                }
            )
        ]


customEditor : List ( String, a ) -> List (Element a) -> Int -> a -> Element a
customEditor variants inputsRow level value =
    Element.column
        [ Background.color (getColor level)
        , Element.width Element.fill
        , spacing
        , padding
        , Element.alignTop
        , Border.width 1
        , borderRounded
        ]
        [ variantRow variants value
        , Element.row [ Element.width Element.fill, spacing ] inputsRow
        ]


variantRow : List ( String, a ) -> a -> Element a
variantRow variants value =
    Input.radioRow
        [ spacing ]
        { onChange = identity
        , options =
            variants
                |> List.map (\( label, v ) -> Input.option v (Element.text label))
        , selected = Maybe.Just value
        , label = Input.labelHidden ""
        }


enumEditor : List ( String, a ) -> a -> Int -> Element a
enumEditor variants value level =
    Element.el
        [ Background.color (getColor level)
        , Element.width Element.fill
        , spacing
        , padding
        , Element.alignTop
        , Border.width 1
        , borderRounded
        ]
        (variantRow variants value)


tupleEditor :
    (Int -> l -> Element l)
    -> Bool
    -> (Int -> r -> Element r)
    -> Bool
    -> Int
    -> ( l, r )
    -> Element ( l, r )
tupleEditor leftEditor leftSimple rightEditor rightSimple level ( left, right ) =
    let
        le =
            leftEditor (level + 1) left

        re =
            rightEditor (level + 1) right
    in
    (if leftSimple && rightSimple then
        Element.row

     else
        Element.column
    )
        [ Background.color (getColor level)
        , Element.width Element.fill
        , spacing
        , padding
        , Element.alignTop
        , Border.width 1
        , borderRounded
        ]
        [ Element.map (\lambdaArg0 -> ( lambdaArg0, right )) le
        , Element.map (\lambdaArg0 -> ( left, lambdaArg0 )) re
        ]


intEditor : Int -> Int -> Element Int
intEditor level value =
    Element.map
        (\lambdaArg0 -> lambdaArg0 |> String.toInt |> Maybe.withDefault value)
        (Input.text
            [ Element.width (Element.minimum 100 Element.fill)
            , Element.alignTop
            , Background.color (getColor level)
            ]
            { onChange = identity
            , text = String.fromInt value
            , placeholder = Maybe.Nothing
            , label = Input.labelHidden ""
            }
        )


floatEditor : Int -> Float -> Element Float
floatEditor level value =
    Element.map
        (\lambdaArg0 -> lambdaArg0 |> String.toFloat |> Maybe.withDefault value)
        (Input.text
            [ Element.width (Element.minimum 100 Element.fill)
            , Element.alignTop
            , Background.color (getColor level)
            ]
            { onChange = identity
            , text = String.fromFloat value
            , placeholder = Maybe.Nothing
            , label = Input.labelHidden ""
            }
        )


stringEditor : Int -> String -> Element String.String
stringEditor level value =
    Input.text
        [ Element.width (Element.minimum 100 Element.fill)
        , Element.alignTop
        , Background.color (getColor level)
        ]
        { onChange = identity
        , text = value
        , placeholder = Maybe.Nothing
        , label = Input.labelHidden ""
        }


map : (f -> v) -> Element f -> Element v
map =
    Element.map
