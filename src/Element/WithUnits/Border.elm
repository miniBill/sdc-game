module Element.WithUnits.Border exposing (rounded, width)

import Element.Border
import Element.WithUnits exposing (Attribute)
import Element.WithUnits.Internal exposing (wrapAttribute)
import Length exposing (Length)


width : Length -> Attribute msg
width =
    wrapAttribute Element.Border.width


rounded : Length -> Attribute msg
rounded =
    wrapAttribute Element.Border.rounded
