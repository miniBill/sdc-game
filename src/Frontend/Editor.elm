module Frontend.Editor exposing (view)

import Dict
import Editors
import Element.WithContext as Element exposing (alignTop, centerY, el, fill, height, inFront, padding, paddingXY, px, row, scrollbars, shrink, text, width)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Frontend.Common
import Frontend.EditorTheme as Theme exposing (Element)
import Json.Decode as JD exposing (Decoder)
import MapPixels
import Model exposing (Data, Id, Person, mapSize)
import Quantity
import Svg as S
import Svg.Attributes as SA
import Svg.Events
import Types exposing (EditorModel, EditorMsg(..))


view : Maybe Data -> EditorModel -> Element EditorMsg
view maybeData editorModel =
    case maybeData of
        Nothing ->
            Frontend.Common.loading

        Just data ->
            let
                personView =
                    editorModel.currentPerson
                        |> Maybe.andThen
                            (\id ->
                                Maybe.map (Tuple.pair id) <|
                                    Dict.get id data
                            )
                        |> Maybe.map (\( id, person ) -> viewPerson id person)
                        |> Maybe.withDefault Element.none

                scrollableView =
                    Theme.column
                        [ scrollbars
                        , height fill
                        , width fill
                        , alignTop
                        ]
                        [ controls GhostMode data editorModel
                        , personView
                        ]
            in
            el
                [ width fill
                , height fill
                , Theme.spacing
                , inFront <|
                    el
                        [ Element.paddingEach
                            { left = 0
                            , top = 0
                            , right = Theme.rythm
                            , bottom = 0
                            }
                        ]
                    <|
                        el
                            [ Theme.padding
                            , Background.color Theme.colors.semitransparent
                            , Border.roundEach
                                { topLeft = 0
                                , topRight = 0
                                , bottomLeft = 0
                                , bottomRight = Theme.rythm
                                }
                            ]
                            (controls RealMode data editorModel)
                ]
                scrollableView


type ControlsMode
    = GhostMode
    | RealMode


controls : ControlsMode -> Data -> EditorModel -> Element EditorMsg
controls mode data model =
    Element.wrappedRow
        [ Theme.spacing
        , alignTop
        ]
        (commonControls mode
            ++ peopleButtons mode data model
            ++ viewError model
        )


viewError : EditorModel -> List (Element msg)
viewError model =
    if String.isEmpty model.lastError then
        []

    else
        [ text model.lastError ]


commonControls : ControlsMode -> List (Element EditorMsg)
commonControls mode =
    let
        btn msg color label =
            Theme.button
                [ alignTop
                , Background.color color
                , Element.transparent <| mode == GhostMode
                ]
                { onPress = Just msg
                , label = text label
                }
    in
    [ btn FileSelect Theme.colors.white "Upload JSON"
    , btn DownloadJson Theme.colors.white "Save as JSON"
    , btn AddPerson Theme.colors.addNew "Add Person"
    ]


peopleButtons : ControlsMode -> Data -> EditorModel -> List (Element EditorMsg)
peopleButtons mode data model =
    data
        |> Dict.toList
        |> List.sortBy (\( _, { name } ) -> name)
        |> List.map
            (\( id, person ) ->
                Theme.button
                    [ alignTop
                    , padding 0
                    , Background.color <|
                        if Just id == model.currentPerson then
                            Theme.colors.selectedTab

                        else
                            Theme.colors.tab
                    , Element.transparent <| mode == GhostMode
                    ]
                    { onPress = Just <| EditPerson id
                    , label =
                        row
                            [ paddingXY Theme.rythm 0
                            , Theme.spacing
                            ]
                            [ if String.isEmpty person.image then
                                Element.none

                              else
                                Theme.image
                                    [ height <| px 30
                                    , centerY
                                    ]
                                    { -- If the image has trouble loading, we really don't want to show an alt text
                                      description = ""
                                    , src = person.image
                                    }
                            , el [ paddingXY 0 Theme.rythm ] <|
                                text <|
                                    if String.isEmpty person.name then
                                        "<New>"

                                    else
                                        person.name
                            ]
                    }
            )


viewPerson : Id -> Person -> Element EditorMsg
viewPerson id person =
    Element.map (\newPerson -> UpdatePerson id <| Just newPerson) <|
        Element.column
            [ alignTop
            , Theme.spacing
            , width fill
            , height shrink
            ]
            [ Tuple.first <| Editors.personEditor 0 person
            , mapEditor person
            ]


mapEditor : Person -> Element Person
mapEditor person =
    let
        mapPixelToString q =
            String.fromFloat <| MapPixels.inPixels q

        radius k =
            SA.r <|
                String.fromFloat <|
                    MapPixels.inPixels mapSize.width
                        * 0.006
                        * k

        cx =
            SA.cx <| String.fromFloat <| person.city.coordinates.x

        cy =
            SA.cy <| String.fromFloat <| person.city.coordinates.y
    in
    S.svg
        [ [ Quantity.zero
          , Quantity.zero
          , Model.mapSize.width
          , Model.mapSize.height
          ]
            |> List.map mapPixelToString
            |> String.join " "
            |> SA.viewBox
        , Svg.Events.on "click" <|
            JD.map
                (\coords ->
                    let
                        city =
                            person.city

                        x =
                            toFloat coords.x * MapPixels.inPixels mapSize.width / toFloat coords.w

                        y =
                            toFloat coords.y * MapPixels.inPixels mapSize.height / toFloat coords.h
                    in
                    { person | city = { city | coordinates = { x = x, y = y } } }
                )
                clickDecoder
        ]
        [ S.image
            [ Theme.imageXlinkHref "/art/europe.jpg"
            , SA.height <| mapPixelToString mapSize.width
            , SA.height <| mapPixelToString mapSize.height
            ]
            []
        , S.circle
            [ cy, cx, radius 1, SA.stroke "black", SA.fill "transparent" ]
            []
        , S.circle
            [ cy, cx, radius 1.2, SA.stroke "white", SA.fill "transparent" ]
            []
        , S.circle
            [ cy, cx, radius 0.2, SA.fill "black" ]
            []
        ]
        |> Element.html
        |> el []


clickDecoder : Decoder { x : Int, y : Int, w : Int, h : Int }
clickDecoder =
    JD.map4 (\x y w h -> { x = x, y = y, w = w, h = h })
        (JD.field "offsetX" JD.int)
        (JD.field "offsetY" JD.int)
        (JD.at [ "target", "farthestViewportElement", "clientWidth" ] JD.int)
        (JD.at [ "target", "farthestViewportElement", "clientHeight" ] JD.int)
