module Theme exposing (button, colors, column, fontSize, fontSizes, input, multiline, padding, row, rythm, select, spacing, tabButton)

import Element exposing (Attribute, Color, Element, el)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Html.Events



-- Attributes


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
    }
colors =
    { addNew = Element.rgb 0.6 1 0.6
    , delete = Element.rgb 1 0.6 0.6
    , semitransparent = Element.rgba 1 1 1 0.7
    , white = Element.rgb 1 1 1
    }


fontSize : number
fontSize =
    16


fontSizes :
    { huge : Element.Attr decorative msg
    , bigger : Element.Attr decorative msg
    , big : Element.Attr decorative msg
    , normal : Element.Attr decorative msg
    , small : Element.Attr decorative msg
    , smaller : Element.Attr decorative msg
    , tiny : Element.Attr decorative msg
    }
fontSizes =
    let
        size n =
            Font.size <|
                round <|
                    Element.modular fontSize 1.25 n
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
    el attrs <|
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
        ([ Border.width 1
         , Border.rounded rythm
         , padding
         ]
            ++ attrs
        )


tabButton :
    List (Element.Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
tabButton attrs =
    button
        ([ Border.widthEach { left = 1, top = 1, right = 1, bottom = 0 }
         , Border.roundEach
            { topLeft = rythm
            , topRight = rythm
            , bottomLeft = 0
            , bottomRight = 0
            }
         ]
            ++ attrs
        )
