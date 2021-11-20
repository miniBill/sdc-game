module Frontend.Common exposing (..)

import Element as E
import Element.Font
import Element.WithUnits as Element exposing (Attribute, Element, column, paragraph, text)
import Element.WithUnits.Font as Font
import Html
import Html.Attributes
import Markdown.Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
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
