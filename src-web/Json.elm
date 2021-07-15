module Json exposing (Data, Scene, dataCodec, toBare)

import Bare
import Codec exposing (Codec)
import Dict exposing (Dict)


type alias Data =
    Dict String Scene


dataCodec : Codec Data
dataCodec =
    Codec.dict sceneCodec


sceneCodec : Codec Scene
sceneCodec =
    Codec.object (\text next image -> Scene text (Maybe.withDefault [] next) (Maybe.withDefault "" image))
        |> Codec.field "text" .text Codec.string
        |> Codec.maybeField "next"
            (justIfNot List.isEmpty << .next)
            (Codec.list <| Codec.tuple Codec.string Codec.string)
        |> Codec.maybeField "image" (justIfNot String.isEmpty << .image) Codec.string
        |> Codec.buildObject


justIfNot empty value =
    if empty value then
        Nothing

    else
        Just value


type alias Scene =
    { text : String
    , next : List ( String, String )
    , image : String
    }


toBare : Data -> List Bare.Scene
toBare dict =
    let
        keysList =
            Dict.keys dict
                |> List.sortBy
                    (\key ->
                        if key == "main" then
                            ( 0, key )

                        else
                            ( 1, key )
                    )

        keysDict =
            keysList
                |> List.indexedMap (\i e -> ( e, i ))
                |> Dict.fromList

        sceneToBare { text, image, next } =
            { text = text
            , image = image
            , next = List.filterMap nextToBare next
            }

        nextToBare ( k, v ) =
            Maybe.map
                (\i -> { label = k, scene = i })
                (Dict.get v keysDict)
    in
    keysList
        |> List.filterMap (\key -> Dict.get key dict)
        |> List.map sceneToBare
