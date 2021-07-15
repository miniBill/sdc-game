module Bare exposing (Data, Next, Scene, dataCodec)

{-| Input:

    type Scene {
        text : string
        image : optional<string>
        next : []Next
    }

    type Next {
        label : string
        scene : int
    }

-}

import Codec.Bare as Codec exposing (Codec)


type alias Data =
    List Scene


dataCodec : Codec Data
dataCodec =
    Codec.list sceneCodec


type alias Scene =
    { text : String
    , image : String
    , next : List Next
    }


{-| Codec for the `Scene` type.
-}
sceneCodec : Codec Scene
sceneCodec =
    Codec.struct (\text image next -> { text = text, image = image, next = next })
        |> Codec.field .text Codec.string
        |> Codec.field .image Codec.string
        |> Codec.field .next (Codec.list nextCodec)
        |> Codec.buildStruct


type alias Next =
    { label : String
    , scene : Int
    }


{-| Codec for the `Next` type.
-}
nextCodec : Codec Next
nextCodec =
    Codec.struct (\label scene -> { label = label, scene = scene })
        |> Codec.field .label Codec.string
        |> Codec.field .scene Codec.int
        |> Codec.buildStruct
