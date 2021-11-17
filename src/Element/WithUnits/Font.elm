module Element.WithUnits.Font exposing (bold, center, family, italic, monospace, regular, size, strike)

import Element.Font
import Element.WithUnits.Internal exposing (Attribute(..), wrapAttribute)
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
