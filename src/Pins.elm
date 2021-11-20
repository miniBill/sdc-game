module Pins exposing (mapSize, northEastToXY)

import Angle exposing (Angle, Radians)
import Length exposing (Length, Meters)
import Quantity exposing (Product, Quantity, Rate)


northEastToXY : Angle -> Angle -> ( Length, Length )
northEastToXY lat long =
    ( Quantity.plus (Tuple.first coeffsEast) (Quantity.at (Tuple.second coeffsEast) long)
    , Quantity.plus (Tuple.first coeffsNorth) (Quantity.at (Tuple.second coeffsNorth) lat)
    )


coeffsEast : ( Length, Quantity Float (Rate Meters Radians) )
coeffsEast =
    calculateCoeffs .east .x


coeffsNorth : ( Length, Quantity Float (Rate Meters Radians) )
coeffsNorth =
    calculateCoeffs .north .y


calculateCoeffs : (Pin -> Angle) -> (Pin -> Length) -> ( Length, Quantity Float (Rate Meters Radians) )
calculateCoeffs xf yf =
    let
        xs =
            List.map xf pins

        ys =
            List.map yf pins

        xAverage =
            Quantity.sum xs
                |> Quantity.divideBy (toFloat <| List.length xs)

        yAverage =
            Quantity.sum ys
                |> Quantity.divideBy (toFloat <| List.length ys)

        xDelta =
            List.map (\x -> Quantity.difference x xAverage) xs

        yDelta =
            List.map (\y -> Quantity.difference y yAverage) ys

        beta =
            Quantity.sum (List.map2 (\xd yd -> Quantity.product xd yd) xDelta yDelta)
                |> over2 (Quantity.sum (List.map Quantity.squared xDelta))

        alpha =
            Quantity.difference yAverage (Quantity.at beta xAverage)
    in
    ( alpha, beta )


over2 : Quantity Float (Product l d) -> Quantity Float (Product l n) -> Quantity Float (Rate n d)
over2 d n =
    Quantity.unsafe (Quantity.unwrap n / Quantity.unwrap d)


pins : List Pin
pins =
    [ udine
    , london
    , paris
    , gibraltar
    , malmo
    ]


type alias Pin =
    { x : Length
    , y : Length
    , north : Angle
    , east : Angle
    }


udine : Pin
udine =
    pin 1459 1001 46.065602 13.237361


london : Pin
london =
    pin 659 675 51.508551 -0.128021


paris : Pin
paris =
    pin 807 832 48.853258 2.348854


gibraltar : Pin
gibraltar =
    pin 344 1595 36.162037 -5.350013


malmo : Pin
malmo =
    pin 1445 428 55.593527 13.004635


pin : Float -> Float -> Float -> Float -> Pin
pin x y north east =
    { x = Length.millimeters x
    , y = Length.millimeters y
    , north = Angle.degrees north
    , east = Angle.degrees east
    }


mapSize :
    { width : Length
    , height : Length
    }
mapSize =
    { width = Length.millimeters 1820
    , height = Length.millimeters 1140
    }
