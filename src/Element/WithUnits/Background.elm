module Element.WithUnits.Background exposing (..)

import Element exposing (Color)
import Element.Background
import Element.WithUnits.Internal exposing (Attribute(..))


color : Color -> Attribute msg
color c =
    Attribute <| \_ -> Element.Background.color c


image : String -> Attribute msg
image url =
    Attribute <| \_ -> Element.Background.image url
