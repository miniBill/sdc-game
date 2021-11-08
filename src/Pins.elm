module Pins exposing (latLongToXY)


latLongToXY lat long =
    let
        x =
            0

        y =
            0

        _ =
            Debug.todo
    in
    ( x, y )


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
