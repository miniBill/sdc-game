module Frontend.Common exposing (..)

import Element as E
import Element.Font
import Theme


loading : E.Element msg
loading =
    E.el
        [ Theme.fontSizes.huge
        , E.centerX
        , E.centerY
        , Element.Font.center
        ]
        (E.text "Loading...")
