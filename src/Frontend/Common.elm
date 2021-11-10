module Frontend.Common exposing (..)

import Element exposing (Element, centerX, centerY, el, text)
import Element.Font as Font
import Theme


loading : Element msg
loading =
    el
        [ Theme.fontSizes.huge
        , centerX
        , centerY
        , Font.center
        ]
        (text "Loading...")
