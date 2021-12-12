module Frontend.GameTheme exposing (Attr, Attribute, Element, autoscaling, autoscalingI, borderRounded, borderWidth, borderWidthEach, colors, fontSize, padding, paddingWithCoeff, paddingXYWithCoeff, spacing)

import Element.WithContext as Element exposing (Color)
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Pixels
import Quantity
import Types exposing (A11yOptions, Size)



-- Types


type alias Context =
    { screenSize : Size, a11y : A11yOptions }


type alias Element msg =
    Element.Element Context msg


type alias Attribute msg =
    Element.Attribute Context msg


type alias Attr decorative msg =
    Element.Attr Context decorative msg



-- Attrs


colors : { semitransparent : Color }
colors =
    { semitransparent = Element.rgba255 0xFF 0xFF 0xFF 0.7 }


borderSize : number
borderSize =
    1


rythm : number
rythm =
    15


padding : Attribute msg
padding =
    Element.padding rythm


paddingXYWithCoeff : Float -> Float -> Attribute msg
paddingXYWithCoeff x y =
    autoscaling 1
        (\s ->
            Element.paddingXY
                (round <| x * rythm * s)
                (round <| y * rythm * s)
        )


paddingWithCoeff : Float -> Attribute msg
paddingWithCoeff k =
    Element.padding (round <| k * rythm)


spacing : Attribute msg
spacing =
    Element.spacing rythm


fontSize : Float -> Attribute msg
fontSize k =
    Element.withAttribute .a11y <| \a11y -> autoscalingI (toFloat a11y.fontSize * k) Font.size


borderWidth : Attribute msg
borderWidth =
    Border.width borderSize


borderRounded : Attribute msg
borderRounded =
    autoscalingI (rythm * 4) Border.rounded


borderWidthEach :
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }
    -> Attribute msg
borderWidthEach { top, right, bottom, left } =
    Border.widthEach
        { top = top * borderSize
        , right = right * borderSize
        , bottom = bottom * borderSize
        , left = left * borderSize
        }



-- Autoscaling
--
-- 0.001 @ 400
-- 0.00075 @ 1000


autoscaling : Float -> (Float -> Attribute msg) -> Attribute msg
autoscaling k f =
    Element.withAttribute .screenSize <| \size ->
    let
        minSize =
            Pixels.inPixels (Quantity.min size.width size.height)

        coeff =
            0.0012 / (1 + 0.3 * (minSize - 400) / 600)
    in
    f <| k * 0.001 * minSize


autoscalingI : Float -> (Int -> Attribute msg) -> Attribute msg
autoscalingI k f =
    autoscaling k (f << round)
