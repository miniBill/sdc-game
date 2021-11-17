module Frontend.Common exposing (..)

import Element as E
import Element.Font
import Element.WithUnits as Element exposing (Attribute, Element, column, paragraph, text)
import Element.WithUnits.Font as Font
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


type alias View msg =
    List (Element msg)


elmUiRendered : Markdown.Renderer.Renderer (View msg)
elmUiRendered =
    let
        heading : { level : Markdown.Block.HeadingLevel, rawText : String, children : List (View msg) } -> View msg
        heading _ =
            Debug.todo "heading renderer"

        paragraph : List (View msg) -> View msg
        paragraph children =
            [ Element.paragraph [] <| List.concat children ]

        blockQuote : List (View msg) -> View msg
        blockQuote _ =
            Debug.todo "blockQuote renderer"

        html : Markdown.Html.Renderer (List (View msg) -> View msg)
        html =
            Markdown.Html.oneOf []

        text : String -> View msg
        text s =
            [ Element.text s ]

        codeSpan : String -> View msg
        codeSpan _ =
            Debug.todo "codeSpan renderer"

        strong : List (View msg) -> View msg
        strong =
            List.map (Element.el [ Font.bold ]) << List.concat

        emphasis : List (View msg) -> View msg
        emphasis =
            List.map (Element.el [ Font.italic ]) << List.concat

        strikethrough : List (View msg) -> View msg
        strikethrough _ =
            Debug.todo "strikethrough renderer"

        hardLineBreak : View msg
        hardLineBreak =
            []

        link : { title : Maybe String, destination : String } -> List (View msg) -> View msg
        link _ _ =
            Debug.todo "link renderer"

        image : { alt : String, src : String, title : Maybe String } -> View msg
        image _ =
            Debug.todo "image renderer"

        unorderedList : List (Markdown.Block.ListItem (View msg)) -> View msg
        unorderedList _ =
            Debug.todo "unorderedList renderer"

        orderedList : Int -> List (List (View msg)) -> View msg
        orderedList _ _ =
            Debug.todo "orderedList renderer"

        codeBlock : { body : String, language : Maybe String } -> View msg
        codeBlock _ =
            Debug.todo "codeBlock renderer"

        thematicBreak : View msg
        thematicBreak =
            []

        table : List (View msg) -> View msg
        table _ =
            Debug.todo "table renderer"

        tableHeader : List (View msg) -> View msg
        tableHeader _ =
            Debug.todo "tableHeader renderer"

        tableBody : List (View msg) -> View msg
        tableBody _ =
            Debug.todo "tableBody renderer"

        tableRow : List (View msg) -> View msg
        tableRow _ =
            Debug.todo "tableRow renderer"

        tableCell : Maybe Markdown.Block.Alignment -> List (View msg) -> View msg
        tableCell _ _ =
            Debug.todo "tableCell renderer"

        tableHeaderCell : Maybe Markdown.Block.Alignment -> List (View msg) -> View msg
        tableHeaderCell _ _ =
            Debug.todo "tableHeaderCell renderer"
    in
    { heading = heading
    , paragraph = paragraph
    , blockQuote = blockQuote
    , html = html
    , text = text
    , codeSpan = codeSpan
    , strong = strong
    , emphasis = emphasis
    , strikethrough = strikethrough
    , hardLineBreak = hardLineBreak
    , link = link
    , image = image
    , unorderedList = unorderedList
    , orderedList = orderedList
    , codeBlock = codeBlock
    , thematicBreak = thematicBreak
    , table = table
    , tableHeader = tableHeader
    , tableBody = tableBody
    , tableRow = tableRow
    , tableCell = tableCell
    , tableHeaderCell = tableHeaderCell
    }


viewMarked : List (Attribute msg) -> String -> Element msg
viewMarked attrs input =
    input
        |> String.replace "  " "\n\n"
        |> Markdown.Parser.parse
        |> Result.mapError (\_ -> "Parsing error")
        |> Result.andThen (Markdown.Renderer.render elmUiRendered)
        |> Result.map (List.concat >> column attrs)
        |> Result.withDefault (text input)
