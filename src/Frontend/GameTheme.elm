module Frontend.GameTheme exposing (Attribute, Element, autoscalingI, borderRounded, borderRoundedEachWithCoeff, borderWidth, borderWidthEach, colors, defaultFontSize, fontSize, padding, paddingXYWithCoeff, spacing)

import Element.WithContext as Element exposing (Color)
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Types exposing (A11yOptions, Size)



-- Types


type alias Context =
    { screenSize : Size, a11y : A11yOptions }


type alias Element msg =
    Element.Element Context msg


type alias Attribute msg =
    Element.Attribute Context msg



-- Attrs


colors :
    { semitransparent : Color
    , selectedTab : Color
    }
colors =
    { semitransparent = Element.rgba255 0xFF 0xFF 0xFF 0.7
    , selectedTab = Element.rgb255 0xD0 0xD0 0xFF
    }


borderSize : number
borderSize =
    1


rythm : number
rythm =
    15


defaultFontSize : number
defaultFontSize =
    20


padding : Attribute msg
padding =
    autoscalingI rythm Element.padding


paddingXYWithCoeff : Float -> Float -> Attribute msg
paddingXYWithCoeff x y =
    autoscaling 1
        (\s ->
            Element.paddingXY
                (round <| x * rythm * s)
                (round <| y * rythm * s)
        )


spacing : Attribute msg
spacing =
    autoscalingI rythm Element.spacing


fontSize : Float -> Attribute msg
fontSize k =
    Element.withAttribute .a11y <| \a11y -> autoscalingI (a11y.fontSize * k) Font.size


borderWidth : Attribute msg
borderWidth =
    Border.width borderSize


borderRounded : Attribute msg
borderRounded =
    autoscalingI (rythm * 4) Border.rounded


borderRoundedEachWithCoeff : { topRight : Bool, topLeft : Bool, bottomRight : Bool, bottomLeft : Bool } -> Attribute msg
borderRoundedEachWithCoeff { topRight, topLeft, bottomRight, bottomLeft } =
    autoscalingI (rythm * 4) <| \s ->
    Border.roundEach
        { topRight = s * rythm * boolToInt topRight
        , topLeft = s * rythm * boolToInt topLeft
        , bottomRight = s * rythm * boolToInt bottomRight
        , bottomLeft = s * rythm * boolToInt bottomLeft
        }


borderWidthEach :
    { top : Bool
    , right : Bool
    , bottom : Bool
    , left : Bool
    }
    -> Attribute msg
borderWidthEach { top, right, bottom, left } =
    Border.widthEach
        { top = boolToInt top * borderSize
        , right = boolToInt right * borderSize
        , bottom = boolToInt bottom * borderSize
        , left = boolToInt left * borderSize
        }


boolToInt : Bool -> Int
boolToInt b =
    if b then
        1

    else
        0



-- Autoscaling


autoscaling : Float -> (Float -> Attribute msg) -> Attribute msg
autoscaling k f =
    Element.withAttribute .a11y <| \a11y ->
    f <| k * a11y.fontSize / defaultFontSize


autoscalingI : Float -> (Int -> Attribute msg) -> Attribute msg
autoscalingI k f =
    autoscaling k (f << round)
