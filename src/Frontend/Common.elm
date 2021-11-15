module Frontend.Common exposing (..)

import Element as E
import Element.Font
import Element.WithUnits as Element exposing (Attribute, Element, column, paragraph, spacing, text)
import Element.WithUnits.Font as Font
import Length
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


elmUiRendered : Markdown.Renderer.Renderer (Element msg)
elmUiRendered =
    let
        html =
            Markdown.Html.oneOf []
    in
    { heading = \_ -> Debug.todo "heading"
    , paragraph = paragraph []
    , blockQuote = \_ -> Debug.todo "blockQuote"
    , html = html
    , text = Element.text
    , codeSpan = \_ -> Debug.todo "codeSpan"
    , strong = \_ -> Debug.todo "strong"
    , emphasis = \_ -> Debug.todo "emphasis"
    , strikethrough = \_ -> Debug.todo "strikethrough"
    , hardLineBreak = Element.todo "hardLineBreak"
    , link = \_ -> Debug.todo "link"
    , image = \_ -> Debug.todo "image"
    , unorderedList = \_ -> Debug.todo "unorderedList"
    , orderedList = \_ -> Debug.todo "orderedList"
    , codeBlock = \_ -> Debug.todo "codeBlock"
    , thematicBreak = Element.todo "thematicBreak"
    , table = \_ -> Debug.todo "table"
    , tableHeader = \_ -> Debug.todo "tableHeader"
    , tableBody = \_ -> Debug.todo "tableBody"
    , tableRow = \_ -> Debug.todo "tableRow"
    , tableCell = \_ -> Debug.todo "tableCell"
    , tableHeaderCell = \_ -> Debug.todo "tableHeaderCell"
    }


viewMarked : List (Attribute msg) -> String -> Element msg
viewMarked attrs input =
    input
        |> String.replace "  " "\n\n"
        |> Markdown.Parser.parse
        |> Result.mapError (\_ -> "Parsing error")
        |> Result.andThen (Markdown.Renderer.render elmUiRendered)
        |> Result.map (column attrs)
        |> Result.withDefault (text input)
