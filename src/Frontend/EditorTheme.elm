module Frontend.EditorTheme exposing (Attribute, Context, Element, borderRounded, button, colors, column, fontSizes, getColor, image, imageXlinkHref, padding, rythm, spacing, tabButton)

import Element.WithContext as Element exposing (Color)
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Env
import Svg
import Svg.Attributes
import Types exposing (A11yOptions, Size)


type alias Context =
    { screenSize : Size
    , a11y : A11yOptions
    }


type alias Element msg =
    Element.Element Context msg


type alias Attribute msg =
    Element.Attribute Context msg



-- Attributes


borderWidth : number
borderWidth =
    1


borderRounded : Attribute msg
borderRounded =
    Border.rounded rythm


rythm : number
rythm =
    10


padding : Attribute msg
padding =
    Element.padding rythm


spacing : Attribute msg
spacing =
    Element.spacing rythm


colors :
    { addNew : Color
    , delete : Color
    , semitransparent : Color
    , white : Color
    , selectedTab : Color
    , tab : Color
    }
colors =
    { addNew = Element.rgb255 0xDE 0xFD 0xE0
    , delete = Element.rgb255 0xFD 0xDF 0xDF
    , semitransparent = Element.rgba255 0xFF 0xFF 0xFF 0.7
    , white = Element.rgb255 0xFF 0xFF 0xFF
    , selectedTab = Element.rgb255 0xD0 0xD0 0xFF
    , tab = Element.rgb255 0xD0 0xD0 0xD0
    }


getColor : Int -> Element.Color
getColor index =
    case modBy 5 index of
        0 ->
            Element.rgb255 0xFD 0xDF 0xDF

        1 ->
            Element.rgb255 0xFC 0xF7 0xDE

        2 ->
            Element.rgb255 0xDE 0xFD 0xE0

        3 ->
            Element.rgb255 0xDE 0xF3 0xFD

        _ ->
            Element.rgb255 0xF0 0xDE 0xFD


fontSizes :
    { huge : Element.Attribute { a | a11y : A11yOptions } msg
    , bigger : Element.Attribute { a | a11y : A11yOptions } msg
    , big : Element.Attribute { a | a11y : A11yOptions } msg
    , normal : Element.Attribute { a | a11y : A11yOptions } msg
    , small : Element.Attribute { a | a11y : A11yOptions } msg
    , smaller : Element.Attribute { a | a11y : A11yOptions } msg
    , tiny : Element.Attribute { a | a11y : A11yOptions } msg
    }
fontSizes =
    let
        size n =
            Element.withAttribute .a11y
                (\a11y ->
                    Font.size <|
                        round <|
                            Element.modular (0.5 * a11y.fontSize) 1.25 n
                )
    in
    { huge = size 4
    , bigger = size 3
    , big = size 2
    , normal = size 1
    , small = size -1
    , smaller = size -2
    , tiny = size -3
    }



-- Containers


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs =
    Element.column ([ padding, spacing ] ++ attrs)



-- Inputs


button :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
button attrs =
    Input.button
        ([ Border.width borderWidth
         , Border.rounded rythm
         , padding
         ]
            ++ attrs
        )


tabButton :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
tabButton attrs =
    button
        ([ Border.widthEach { left = borderWidth, top = borderWidth, right = borderWidth, bottom = 0 }
         , Border.roundEach
            { topLeft = rythm
            , topRight = rythm
            , bottomLeft = 0
            , bottomRight = 0
            }
         ]
            ++ attrs
        )


image : List (Attribute msg) -> { description : String, src : String } -> Element msg
image attrs config =
    Element.image attrs { config | src = String.replace "//" "/" <| Env.filesBaseUrl ++ config.src }


imageXlinkHref : String -> Svg.Attribute msg
imageXlinkHref src =
    Svg.Attributes.xlinkHref <| String.replace "//" "/" <| Env.filesBaseUrl ++ src
