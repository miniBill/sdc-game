module Model exposing (dataCodec, dfsSort, emptyScene, replaceScene)

import Codec exposing (Codec)
import Dict
import List.Extra as List
import Types exposing (Data, Scene)


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


replaceScene : String -> String -> Scene -> Data -> Data
replaceScene oldKey newKey newValue data =
    data
        |> Dict.remove oldKey
        |> Dict.insert newKey newValue
        |> (if oldKey == "" then
                identity

            else
                Dict.map
                    (\_ scene ->
                        { scene
                            | next =
                                List.updateIf
                                    (Tuple.second >> (==) oldKey)
                                    (\( label, _ ) -> ( label, newKey ))
                                    scene.next
                        }
                    )
           )
        |> clean


clean : Data -> Data
clean =
    Dict.filter (\k v -> not (String.isEmpty k) || not (v == emptyScene))
        >> Dict.map (always cleanNext)


cleanNext : Scene -> Scene
cleanNext scene =
    { scene
        | next =
            scene.next
                |> List.filter
                    (\( k, v ) -> not (String.isEmpty k) || not (String.isEmpty v))
    }


emptyScene : Scene
emptyScene =
    { text = ""
    , image = ""
    , next = []
    }