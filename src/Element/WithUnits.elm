module Element.WithUnits exposing (Attribute, Element, alignRight, alignTop, behindContent, centerX, centerY, column, el, element, fill, height, html, htmlAttribute, image, inFront, moveDown, moveLeft, moveRight, moveUp, none, padding, paddingEach, paragraph, px, row, run, spacing, text, textColumn, width)

import Element
import Element.WithUnits.Internal exposing (Attribute(..), Element(..), Length(..), wrap, wrapAttribute, wrapAttributeF, wrapAttrs, wrapContainer)
import Html
import Length
import Types exposing (Size)


type alias Element msg =
    Element.WithUnits.Internal.Element msg


type alias Attribute msg =
    Element.WithUnits.Internal.Attribute msg


type alias Length =
    Element.WithUnits.Internal.Length


run : Size -> Element msg -> Element.Element msg
run =
    Element.WithUnits.Internal.run


element : Element.Element msg -> Element msg
element e =
    Element <| \_ -> e


padding : Length.Length -> Attribute msg
padding =
    wrapAttribute Element.padding


spacing : Length.Length -> Attribute msg
spacing =
    wrapAttribute Element.spacing


none : Element msg
none =
    Element (\_ -> Element.none)


text : String -> Element msg
text =
    Element << always << Element.text


paragraph : List (Attribute msg) -> List (Element msg) -> Element msg
paragraph =
    wrapContainer Element.paragraph


textColumn : List (Attribute msg) -> List (Element msg) -> Element msg
textColumn =
    wrapContainer Element.textColumn


paddingEach :
    { top : Length.Length
    , left : Length.Length
    , bottom : Length.Length
    , right : Length.Length
    }
    -> Attribute msg
paddingEach { top, left, right, bottom } =
    Attribute <| \size ->
    Element.paddingEach
        { left = wrap identity left size
        , right = wrap identity right size
        , bottom = wrap identity bottom size
        , top = wrap identity top size
        }


px : Length.Length -> Length
px length =
    Length (wrap Element.px length)


width : Length -> Attribute msg
width (Length length) =
    Attribute (Element.width << length)


height : Length -> Attribute msg
height (Length length) =
    Attribute (Element.height << length)


fill : Length
fill =
    Length <| \_ -> Element.fill


alignRight : Attribute msg
alignRight =
    Attribute <| always Element.alignRight


alignTop : Attribute msg
alignTop =
    Attribute <| always Element.alignTop


centerX : Attribute msg
centerX =
    Attribute <| always Element.centerX


centerY : Attribute msg
centerY =
    Attribute <| always Element.centerY


inFront : Element msg -> Attribute msg
inFront (Element e) =
    Attribute (\size -> Element.inFront <| e size)


behindContent : Element msg -> Attribute msg
behindContent (Element e) =
    Attribute (\size -> Element.behindContent <| e size)


column : List (Attribute msg) -> List (Element msg) -> Element msg
column =
    wrapContainer Element.column


row : List (Attribute msg) -> List (Element msg) -> Element msg
row =
    wrapContainer Element.row


el : List (Attribute msg) -> Element msg -> Element msg
el attrs (Element child) =
    wrapAttrs Element.el attrs child


html : Html.Html msg -> Element msg
html h =
    Element <| \_ -> Element.html h


htmlAttribute : Html.Attribute msg -> Attribute msg
htmlAttribute a =
    Attribute <| \_ -> Element.htmlAttribute a


image : List (Attribute msg) -> { src : String, description : String } -> Element msg
image attrs args =
    wrapAttrs Element.image attrs (\_ -> args)


moveDown : Length.Length -> Attribute msg
moveDown =
    wrapAttributeF Element.moveDown


moveUp : Length.Length -> Attribute msg
moveUp =
    wrapAttributeF Element.moveUp


moveLeft : Length.Length -> Attribute msg
moveLeft =
    wrapAttributeF Element.moveLeft


moveRight : Length.Length -> Attribute msg
moveRight =
    wrapAttributeF Element.moveRight
