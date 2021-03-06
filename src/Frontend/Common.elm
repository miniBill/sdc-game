module Frontend.Common exposing (loading)

import Element.WithContext exposing (Element, centerX, centerY, el, text)
import Element.WithContext.Font as Font
import Frontend.EditorTheme as EditorTheme
import Model exposing (A11yOptions)


loading : Element { a | a11y : A11yOptions } msg
loading =
    el
        [ EditorTheme.fontSizes.huge
        , centerX
        , centerY
        , Font.center
        ]
        (text "Loading...")
