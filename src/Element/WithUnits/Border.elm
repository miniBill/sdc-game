module Element.WithUnits.Border exposing (color, rounded, width, widthEach)

import Element.Border
import Element.WithUnits exposing (Attribute, Color)
import Element.WithUnits.Internal exposing (Attribute(..), wrap, wrapAttribute)
import Length exposing (Length)


width : Length -> Attribute msg
width =
    wrapAttribute Element.Border.width


widthEach : { top : Length, left : Length, bottom : Length, right : Length } -> Attribute msg
widthEach { top, left, bottom, right } =
    Attribute <| \size ->
    Element.Border.widthEach
        { top = wrap identity top size
        , left = wrap identity left size
        , bottom = wrap identity bottom size
        , right = wrap identity right size
        }


rounded : Length -> Attribute msg
rounded =
    wrapAttribute Element.Border.rounded


color : Color -> Attribute msg
color c =
    Attribute <| \_ -> Element.Border.color c
