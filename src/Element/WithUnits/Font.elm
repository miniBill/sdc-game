module Element.WithUnits.Font exposing (bold, center, color, family, italic, monospace, regular, shadow, size, strike, typeface)

import Element.Font
import Element.WithUnits exposing (Color)
import Element.WithUnits.Internal exposing (Attribute(..), wrap, wrapAttribute, wrapF)
import Length


type alias Font =
    Element.Font.Font


size : Length.Length -> Attribute msg
size =
    wrapAttribute Element.Font.size


regular : Attribute msg
regular =
    Attribute <| \_ -> Element.Font.regular


center : Attribute msg
center =
    Attribute <| \_ -> Element.Font.center


bold : Attribute msg
bold =
    Attribute <| \_ -> Element.Font.bold


italic : Attribute msg
italic =
    Attribute <| \_ -> Element.Font.italic


strike : Attribute msg
strike =
    Attribute <| \_ -> Element.Font.strike


family : List Font -> Attribute msg
family fonts =
    Attribute <| \_ -> Element.Font.family fonts


monospace : Font
monospace =
    Element.Font.monospace


typeface : String -> Font
typeface =
    Element.Font.typeface


color : Color -> Attribute msg
color c =
    Attribute <| \_ -> Element.Font.color c


shadow : { offset : ( Length.Length, Length.Length ), blur : Length.Length, color : Color } -> Attribute msg
shadow shade =
    Attribute <| \size_ ->
    let
        wrap u =
            wrapF identity u size_
    in
    Element.Font.shadow
        { offset = Tuple.mapBoth wrap wrap shade.offset
        , color = shade.color
        , blur = wrap shade.blur
        }
