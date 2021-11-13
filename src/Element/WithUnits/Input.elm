module Element.WithUnits.Input exposing (button)

import Element.Input
import Element.WithUnits.Internal exposing (Attribute(..), Element(..), wrapAttrs)


button :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
button attrs args =
    wrapAttrs
        Element.Input.button
        attrs
        (\size ->
            { onPress = args.onPress
            , label = Element.WithUnits.Internal.run size args.label
            }
        )
