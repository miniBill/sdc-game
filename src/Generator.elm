module Generator exposing (generate)

import Dict exposing (Dict)
import Model exposing (dfsSort)
import Types exposing (Data, Scene)


generate : Data -> String
generate model =
    let
        scenes =
            dfsSort "main" model

        map =
            scenes
                |> List.indexedMap (\i ( name, _ ) -> ( name, i ))
                |> Dict.fromList
    in
    """
    #include "logic.h\"""" ++ String.concat (List.map sceneToInclude scenes) ++ String.concat (List.map sceneToDeclarations scenes) ++ """

    const scene end_scene = { "THE END", 0, 0, 0 };

    scene step(int *current_scene, int choice) {
        switch(*current_scene) {""" ++ String.concat (List.indexedMap (sceneToCase map) scenes) ++ """
            default:
                return end_scene;
        }
    } 
    """ |> deindent


sceneToInclude : ( String, Scene ) -> String
sceneToInclude ( _, { image } ) =
    if String.isEmpty image then
        ""

    else
        "\n    #include \"art/" ++ image ++ ".h\""


sceneToCase : Dict String Int -> Int -> ( String, Scene ) -> String
sceneToCase map index ( name, scene ) =
    case scene.next of
        [] ->
            """
            case """ ++ String.fromInt index ++ """: // """ ++ name ++ """
                *current_scene = -1;
                return end_scene;
"""

        [ ( _, key ) ] ->
            """
            case """ ++ String.fromInt index ++ """: // """ ++ name ++ """
                *current_scene = """ ++ String.fromInt (Maybe.withDefault -1 (Dict.get key map)) ++ """;
                return """ ++ key ++ """_scene;
"""

        _ ->
            """
            case """ ++ String.fromInt index ++ """: // """ ++ name ++ """
                switch(choice) {""" ++ String.concat (List.indexedMap (nextToCase map) scene.next) ++ """
                    default:
                        *current_scene = -1;
                        return end_scene;
                }
"""


nextToCase : Dict String Int -> Int -> ( String, String ) -> String
nextToCase map choice ( _, key ) =
    case Dict.get key map of
        Nothing ->
            ""

        Just index ->
            """
                    case """ ++ String.fromInt choice ++ """:
                        *current_scene = """ ++ String.fromInt index ++ """;
                        return """ ++ key ++ """_scene;"""


sceneToDeclarations : ( String, Scene ) -> String
sceneToDeclarations ( name, scene ) =
    let
        nextCount =
            List.length scene.next

        ( labelsDeclaration, labels ) =
            if nextCount == 0 then
                ( "", "" )

            else
                ( "    const char * const " ++ name ++ "_labels[" ++ String.fromInt nextCount ++ "] = {" ++ String.join ", " (List.map (Tuple.first >> escape) scene.next) ++ "};\n"
                , ", " ++ name ++ "_labels"
                )

        image =
            if String.isEmpty scene.image then
                "0"

            else
                "&" ++ scene.image ++ "_image"

        sceneDeclaration =
            "    const scene " ++ name ++ "_scene = {" ++ escape scene.text ++ ", " ++ image ++ ", " ++ String.fromInt nextCount ++ labels ++ "};"
    in
    "\n\n" ++ labelsDeclaration ++ sceneDeclaration


escape : String -> String
escape s =
    s
        |> String.replace "\\" "\\\\"
        |> String.replace "\n" "\\n"
        |> String.replace "\"" "\\\""
        |> (\r -> "\"" ++ r ++ "\"")


deindent : String -> String
deindent =
    String.split "\n"
        >> List.map (String.dropLeft 4)
        >> String.join "\n"
