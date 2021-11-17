module Element.WithUnits exposing (Attribute, Element, Length, Orientation(..), alignRight, alignTop, behindContent, centerX, centerY, column, el, element, fill, fillPortion, height, html, htmlAttribute, image, inFront, maximum, minimum, moveDown, moveLeft, moveRight, moveUp, newTabLink, none, padding, paddingEach, paragraph, px, row, run, shrink, spacing, text, textColumn, transparent, width, withOrientation, withSize)

import Element
import Element.WithUnits.Internal exposing (Attribute(..), Element(..), Length(..), wrap, wrapAttribute, wrapAttributeF, wrapAttrs, wrapContainer)
import Html
import Length
import Quantity
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


width : Length -> Attribute msg
width (Length length) =
    Attribute (Element.width << length)


height : Length -> Attribute msg
height (Length length) =
    Attribute (Element.height << length)


px : Length.Length -> Length
px length =
    Length (wrap Element.px length)


shrink : Length
shrink =
    Length <| \_ -> Element.shrink


minimum : Length.Length -> Length -> Length
minimum limit (Length w) =
    Length (\size -> wrap (\l -> Element.minimum l (w size)) limit size)


maximum : Length.Length -> Length -> Length
maximum limit (Length w) =
    Length (\size -> wrap (\l -> Element.maximum l (w size)) limit size)


fill : Length
fill =
    Length <| \_ -> Element.fill


fillPortion : Int -> Length
fillPortion p =
    Length <| \_ -> Element.fillPortion p


transparent : Bool -> Attribute msg
transparent b =
    Attribute <| \_ -> Element.transparent b


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


newTabLink : List (Attribute msg) -> { url : String, label : Element msg } -> Element msg
newTabLink attrs args =
    wrapAttrs Element.newTabLink
        attrs
        (\size ->
            { url = args.url
            , label = run size args.label
            }
        )


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


type Orientation
    = Portrait
    | Landscape


withOrientation : (Orientation -> Element msg) -> Element msg
withOrientation f =
    Element
        (\size ->
            let
                orientation =
                    if size.width |> Quantity.lessThan size.height then
                        Portrait

                    else
                        Landscape
            in
            run size <|
                f orientation
        )


withSize : (Size -> Element msg) -> Element msg
withSize f =
    Element (\size -> run size (f size))
