module Element.WithUnits.Internal exposing
    ( Attribute(..)
    , Element(..)
    , Length(..)
    , run
    , wrap
    , wrapAttribute
    , wrapAttributeF
    , wrapAttrs
    , wrapContainer
    )

import Element
import Length exposing (Meters)
import Pins exposing (mapSize)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity, Rate)
import Types exposing (Size)


type Element msg
    = Element (Size -> Element.Element msg)


type Attribute msg
    = Attribute (Size -> Element.Attribute msg)


type Length
    = Length (Size -> Element.Length)


run : Size -> Element msg -> Element.Element msg
run size (Element f) =
    f size


scale : Size -> Quantity Float (Rate Pixels Meters)
scale size =
    Quantity.min
        (Quantity.per mapSize.width size.width)
        (Quantity.per mapSize.height size.height)


wrap : (Int -> r) -> Length.Length -> Size -> r
wrap original =
    wrapF (round >> original)


wrapF : (Float -> r) -> Length.Length -> Size -> r
wrapF original length size =
    original <| Pixels.inPixels <| Quantity.at (scale size) length


wrapAttribute : (Int -> Element.Attribute msg) -> Length.Length -> Attribute msg
wrapAttribute original length =
    Attribute <| wrap original length


wrapAttributeF : (Float -> Element.Attribute msg) -> Length.Length -> Attribute msg
wrapAttributeF original length =
    Attribute <| wrapF original length


wrapAttrs :
    (List (Element.Attribute msg) -> a -> Element.Element msg)
    -> List (Attribute msg)
    -> (Size -> a)
    -> Element msg
wrapAttrs wrapped attrs child =
    Element <| \size ->
    wrapped
        (List.map (\(Attribute a) -> a size) attrs)
        (child size)


wrapContainer :
    (List (Element.Attribute msg)
     -> List (Element.Element msg)
     -> Element.Element msg
    )
    -> List (Attribute msg)
    -> List (Element msg)
    -> Element msg
wrapContainer container attrs children =
    Element <| \size ->
    container
        (List.map (\(Attribute a) -> a size) attrs)
        (List.map (\(Element e) -> e size) children)
