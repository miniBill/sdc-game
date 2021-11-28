module Frontend.Common exposing (..)

import Element.WithContext as Element exposing (Element, centerX, centerY, el, text)
import Element.WithContext.Font as Font
import Theme


loading : Element context msg
loading =
    el
        [ Theme.fontSizes.huge
        , centerX
        , centerY
        , Font.center
        ]
        (text "Loading...")
