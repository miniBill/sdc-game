module Pins exposing (northEastToXY)


northEastToXY : Float -> Float -> ( Float, Float )
northEastToXY lat long =
    ( Tuple.first coeffsEast + Tuple.second coeffsEast * long
    , Tuple.first coeffsNorth + Tuple.second coeffsNorth * lat
    )


coeffsEast : ( Float, Float )
coeffsEast =
    calculateCoeffs .east (.x >> toFloat)


coeffsNorth : ( Float, Float )
coeffsNorth =
    calculateCoeffs .north (.y >> toFloat)


calculateCoeffs : (Pin -> Float) -> (Pin -> Float) -> ( Float, Float )
calculateCoeffs xf yf =
    let
        xs =
            List.map xf pins

        ys =
            List.map yf pins

        xAverage =
            List.sum xs / toFloat (List.length xs)

        yAverage =
            List.sum ys / toFloat (List.length ys)

        xDelta =
            List.map (\x -> x - xAverage) xs

        yDelta =
            List.map (\y -> y - yAverage) ys

        beta =
            List.sum (List.map2 (\xd yd -> xd * yd) xDelta yDelta)
                / List.sum (List.map (\xd -> xd * xd) xDelta)

        alpha =
            yAverage - beta * xAverage
    in
    ( alpha, beta )


pins : List Pin
pins =
    [ udine
    , london
    , paris
    , gibraltar
    , malmo
    ]


type alias Pin =
    { x : Int
    , y : Int
    , north : Float
    , east : Float
    }


udine : Pin
udine =
    pin 1469 1001 46.065602 13.237361


london : Pin
london =
    pin 669 675 51.508551 -0.128021


paris : Pin
paris =
    pin 817 832 48.853258 2.348854


gibraltar : Pin
gibraltar =
    pin 354 1595 36.162037 -5.350013


malmo : Pin
malmo =
    pin 1455 428 55.593527 13.004635


pin : Int -> Int -> Float -> Float -> Pin
pin x y north east =
    { x = x, y = y, north = north, east = east }
