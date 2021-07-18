module Theme exposing (colors, input, multiline, padding, rythm, select, spacing)

import Element exposing (Attribute, Color, Element, el, rgba, text)
import Element.Background as Background
import Element.Input as Input
import Html
import Html.Attributes
import Html.Events


rythm : number
rythm =
    10


padding : Attribute msg
padding =
    Element.padding rythm


spacing : Attribute msg
spacing =
    Element.spacing rythm


colors : { semitransparent : Color }
colors =
    { semitransparent =
        rgba 1 1 1 0.7
    }


select :
    List (Attribute msg)
    ->
        { onInput : String -> msg
        , selected : String
        , options : List String
        }
    -> Element msg
select attrs { onInput, selected, options } =
    let
        toOption key =
            Html.option
                [ Html.Attributes.value key
                , Html.Attributes.selected <| key == selected
                ]
                [ Html.text key ]
    in
    el attrs <|
        Element.html <|
            Html.select
                [ Html.Attributes.style "padding" <| String.fromInt rythm ++ "px"
                , Html.Events.onInput onInput
                ]
                (List.map toOption options)


input :
    List (Attribute Never)
    ->
        { label : String
        , text : String
        , onChange : String -> msg
        }
    -> Element msg
input attrs { label, text, onChange } =
    Input.text
        (Background.color colors.semitransparent
            :: List.map (Element.mapAttribute never) attrs
        )
        { label = Input.labelHidden label
        , text = text
        , onChange = onChange
        , placeholder = Just <| Input.placeholder [] <| Element.text label
        }


multiline :
    List (Attribute Never)
    ->
        { label : String
        , text : String
        , onChange : String -> msg
        }
    -> Element msg
multiline attrs { label, text, onChange } =
    Input.multiline
        (Background.color colors.semitransparent
            :: List.map (Element.mapAttribute never) attrs
        )
        { label = Input.labelHidden label
        , text = text
        , onChange = onChange
        , placeholder = Just <| Input.placeholder [] <| Element.text label
        , spellcheck = True
        }
