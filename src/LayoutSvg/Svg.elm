module LayoutSvg.Svg exposing (Svg, layout, text)

import Html exposing (Html)
import TypedSvg
import TypedSvg.Attributes as Attrs
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core


type Svg msg
    = Svg (Context -> TypedSvg.Core.Svg msg)


type alias Context =
    { width : Float
    , height : Float
    , x : Float
    , y : Float
    }


type Attribute msg
    = Attribute (Context -> TypedSvg.Core.Attribute msg)


layout :
    List (Attribute msg)
    ->
        { width : Float
        , height : Float
        , child : Svg msg
        }
    -> Html msg
layout attrs data =
    let
        initialContext =
            { x = 0
            , y = 0
            , width = data.width
            , height = data.height
            }
    in
    TypedSvg.svg
        (Attrs.viewBox 0 0 data.width data.height
            :: List.map (\(Attribute f) -> f initialContext) attrs
        )
        [ (\(Svg f) -> f) data.child initialContext ]


text : {align:  content:String} -> Svg msg
text content =
    Svg
        (\context ->
            TypedSvg.text_
                [ InPx.x context.x
                , InPx.y context.y
                ]
                (TypedSvg.Core.text content)
        )
