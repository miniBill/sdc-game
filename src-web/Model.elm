module Model exposing (Data, Scene, dataCodec, dfsSort)

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


justIfNot : (b -> Bool) -> b -> Maybe b
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


dfsSort : String -> Data -> List ( String, Scene )
dfsSort root scenes =
    case Dict.get root scenes of
        Nothing ->
            []

        Just scene ->
            let
                ( visible, nonvisible ) =
                    List.foldl
                        (\( _, v ) ( res, queue ) ->
                            let
                                found =
                                    dfsSort v queue

                                newQueue =
                                    List.foldl (\( k, _ ) -> Dict.remove k) queue found
                            in
                            ( res ++ found, newQueue )
                        )
                        ( [ ( root, scene ) ], Dict.remove root scenes )
                        scene.next
            in
            if root == "main" then
                visible ++ Dict.toList nonvisible

            else
                visible
