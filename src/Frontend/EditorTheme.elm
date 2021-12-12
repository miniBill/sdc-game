module Frontend.EditorTheme exposing (Attr, Attribute, Context, Element, borderWidth, button, colors, column, fontSizes, getColor, input, multiline, padding, row, rythm, select, spacing, tabButton, wrappedRow)

import Element.WithContext as Element exposing (Color)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Html
import Html.Attributes
import Html.Events
import Types exposing (A11yOptions, Size)


type alias Context =
    { screenSize : Size
    , a11y : A11yOptions
    }


type alias Element msg =
    Element.Element Context msg


type alias Attribute msg =
    Element.Attribute Context msg


type alias Attr decorative msg =
    Element.Attr Context decorative msg



-- Attributes


borderWidth : number
borderWidth =
    1


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
                            Element.modular (0.5 * toFloat a11y.fontSize) 1.25 n
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


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs =
    Element.row ([ padding, spacing ] ++ attrs)


wrappedRow : List (Attribute msg) -> List (Element msg) -> Element msg
wrappedRow attrs =
    Element.wrappedRow ([ padding, spacing ] ++ attrs)


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs =
    Element.column ([ padding, spacing ] ++ attrs)



-- Inputs


select :
    List (Attribute msg)
    ->
        { onInput : String -> msg
        , selected : String
        , options : List String
        }
    -> Element msg
select attrs { onInput, selected, options } =
    let
        toOption key =
            Html.option
                [ Html.Attributes.value key
                , Html.Attributes.selected <| key == selected
                ]
                [ Html.text key ]
    in
    Element.el attrs <|
        Element.html <|
            Html.select
                [ Html.Attributes.style "padding" <| String.fromInt rythm ++ "px"
                , Html.Events.onInput onInput
                , Html.Attributes.style "background-color" <| toCssString colors.semitransparent
                ]
                (List.map toOption options)


toCssString : Color -> String
toCssString color =
    let
        { red, green, blue, alpha } =
            Element.toRgb color

        channel c =
            String.fromInt <| floor <| 255 * c
    in
    "rgba(" ++ String.join "," [ channel red, channel green, channel blue, String.fromFloat alpha ] ++ ")"


input :
    List (Attribute Never)
    ->
        { label : String
        , text : String
        , onChange : String -> msg
        }
    -> Element msg
input attrs { label, text, onChange } =
    Input.text
        (Background.color colors.semitransparent
            :: List.map (Element.mapAttribute never) attrs
        )
        { label = Input.labelHidden label
        , text = text
        , onChange = onChange
        , placeholder = Just <| Input.placeholder [] <| Element.text label
        }


multiline :
    List (Attribute Never)
    ->
        { label : String
        , text : String
        , onChange : String -> msg
        }
    -> Element msg
multiline attrs { label, text, onChange } =
    Input.multiline
        (Background.color colors.semitransparent
            :: List.map (Element.mapAttribute never) attrs
        )
        { label = Input.labelHidden label
        , text = text
        , onChange = onChange
        , placeholder = Just <| Input.placeholder [] <| Element.text label
        , spellcheck = True
        }


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
         , Border.rounded rythm
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
