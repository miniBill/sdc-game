module Element.WithUnits.Font exposing (bold, center, italic, regular, size)

import Element.Font
import Element.WithUnits.Internal exposing (Attribute(..), wrapAttribute)
import Length


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
