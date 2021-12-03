module MapPixels exposing
    ( MapLength
    , MapPixel
    , inPixels
    , pixels
    )

import Quantity exposing (Quantity)


type alias MapLength =
    Quantity Float MapPixel


type MapPixel
    = MapPixel


pixels : number -> Quantity number MapPixel
pixels =
    Quantity.unsafe


inPixels : Quantity number MapPixel -> number
inPixels =
    Quantity.unwrap
