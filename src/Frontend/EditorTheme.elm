module Frontend.EditorTheme exposing (Attribute, Context, Editor, Element, button, colors, column, customEditor, enumEditor, floatEditor, fontSizes, image, imageXlinkHref, intEditor, listEditor, map, objectEditor, padding, rythm, spacing, stringEditor, tupleEditor)

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


borderWidths : { left : number, right : number, top : number, bottom : number }
borderWidths =
    { left = 1
    , right = 1
    , top = 1
    , bottom = 1
    }


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


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs =
    Element.row ([ padding, spacing ] ++ attrs)



-- Inputs


{-| `[ Border.width borderWidth, borderRounded, padding ]`
-}
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


image : List (Attribute msg) -> { description : String, src : String } -> Element msg
image attrs config =
    Element.image attrs { config | src = Env.imageToUrl config.src }


imageXlinkHref : String -> Svg.Attribute msg
imageXlinkHref src =
    Svg.Attributes.xlinkHref <| Env.imageToUrl src



-- Editors


type alias Editor msg =
    Int -> Element msg


roundTop : Attribute e
roundTop =
    Border.roundEach
        { topLeft = rythm
        , topRight = rythm
        , bottomLeft = 0
        , bottomRight = 0
        }


roundBottom : Attribute e
roundBottom =
    Border.roundEach
        { topLeft = 0
        , topRight = 0
        , bottomLeft = rythm
        , bottomRight = rythm
        }


chubby : (List (Attribute msg) -> child -> Element a) -> List (Attribute msg) -> child -> Editor a
chubby container attrs child level =
    container
        ([ Background.color (getColor level)
         , borderRounded
         , Border.width borderWidth
         , spacing
         , padding
         , Element.width Element.fill
         , Element.alignTop
         ]
            ++ attrs
        )
        child


objectEditor : List ( String, Editor msg ) -> List ( String, Editor msg ) -> Editor msg
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
                                [ simpleLabel fieldName, fieldEditor (level + 1) ]
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
                          , view = \( _, fieldEditor ) -> fieldEditor (level + 1)
                          }
                        ]
                    , data = rawSimples
                    }

        complexes =
            List.concatMap
                (\( fieldName, fieldEditor ) ->
                    [ Element.text fieldName, fieldEditor (level + 1) ]
                )
                rawComplexes
    in
    chubby Element.column [] (simplesTable :: complexes) level


listEditor :
    String
    -> (e -> Editor e)
    -> e
    -> List e
    -> Editor (List e)
listEditor typeName valueEditor valueDefault value level =
    let
        rows =
            List.indexedMap
                (\i rowData ->
                    Element.map
                        (\lambdaArg0 ->
                            if lambdaArg0 == valueDefault then
                                List.Extra.removeAt i value

                            else
                                List.Extra.setAt i lambdaArg0 value
                        )
                        (toRow rowData)
                )
                value

        toRow rowData =
            Element.column
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
                    (button
                        [ Border.widthEach { borderWidths | bottom = 0 }
                        , roundTop
                        , Element.alignTop
                        , Background.gradient
                            { angle = 0
                            , steps =
                                [ getColor (level + 1)
                                , colors.delete
                                , colors.delete
                                ]
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
                    (valueEditor rowData (level + 1))
                ]
    in
    Element.column
        [ Element.width Element.fill ]
        [ chubby Element.column [] rows level
        , Element.el
            [ Element.paddingXY rythm 0
            , Element.alignRight
            ]
            (button
                [ Element.alignTop
                , Background.gradient
                    { angle = 0
                    , steps =
                        [ colors.addNew
                        , colors.addNew
                        , colors.addNew
                        , getColor level
                        ]
                    }
                , Border.widthEach { borderWidths | top = 0 }
                , roundBottom
                , Element.moveUp 1
                ]
                { onPress = Just (value ++ [ valueDefault ])
                , label = Element.text ("Add new " ++ typeName)
                }
            )
        ]


customEditor : List ( String, a ) -> List (Editor a) -> a -> Editor a
customEditor variants inputsRow value level =
    chubby Element.column
        []
        [ variantRow variants value
        , Element.row [ Element.width Element.fill, spacing ] (List.map (\e -> e (level + 1)) inputsRow)
        ]
        level


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


enumEditor : List ( String, a ) -> a -> Editor a
enumEditor variants value =
    chubby Element.el [] (variantRow variants value)


tupleEditor :
    (l -> Editor l)
    -> Bool
    -> (r -> Editor r)
    -> Bool
    -> ( l, r )
    -> Editor ( l, r )
tupleEditor leftEditor leftSimple rightEditor rightSimple ( left, right ) level =
    chubby
        (if leftSimple && rightSimple then
            Element.row

         else
            Element.column
        )
        []
        [ Element.map (\newLeft -> ( newLeft, right )) (leftEditor left (level + 1))
        , Element.map (\newRight -> ( left, newRight )) (rightEditor right (level + 1))
        ]
        level


intEditor : Int -> Editor Int
intEditor =
    input String.toInt String.fromInt


floatEditor : Float -> Editor Float
floatEditor =
    input String.toFloat String.fromFloat


stringEditor : String -> Editor String
stringEditor =
    input Just identity


input : (String -> Maybe a) -> (a -> String) -> a -> Editor a
input onChange toString value =
    chubby Input.text
        [ Element.width <| Element.minimum 100 Element.fill, Border.color <| Element.rgb 0 0 0 ]
        { onChange = \newValue -> onChange newValue |> Maybe.withDefault value
        , text = toString value
        , placeholder = Nothing
        , label = Input.labelHidden ""
        }


map : (f -> v) -> Editor f -> Editor v
map f e level =
    Element.map f (e level)
