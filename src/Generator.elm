module Generator exposing (generate)

import Dict exposing (Dict)
import Model exposing (City, Data)


generate : Data -> String
generate model =
    let
        citys =
            model

        map =
            citys
                |> List.indexedMap (\i ( name, _ ) -> ( name, i ))
                |> Dict.fromList
    in
    """
    #include "logic.h\"""" ++ String.concat (List.map cityToInclude citys) ++ String.concat (List.map cityToDeclarations citys) ++ """

    const city end_city = { "THE END", 0, 0, 0 };

    city step(int *current_city, int choice) {
        switch(*current_city) {""" ++ String.concat (List.indexedMap (cityToCase map) citys) ++ """
            default:
                return end_city;
        }
    } 
    """ |> deindent


cityToInclude : ( String, City ) -> String
cityToInclude ( _, { image } ) =
    if String.isEmpty image then
        ""

    else
        "\n    #include \"art/" ++ image ++ ".h\""


cityToCase : Dict String Int -> Int -> ( String, City ) -> String
cityToCase map index ( name, city ) =
    case city.next of
        [] ->
            """
            case """ ++ String.fromInt index ++ """: // """ ++ name ++ """
                *current_city = -1;
                return end_city;
"""

        [ ( _, key ) ] ->
            """
            case """ ++ String.fromInt index ++ """: // """ ++ name ++ """
                *current_city = """ ++ String.fromInt (Maybe.withDefault -1 (Dict.get key map)) ++ """;
                return """ ++ key ++ """_city;
"""

        _ ->
            """
            case """ ++ String.fromInt index ++ """: // """ ++ name ++ """
                switch(choice) {""" ++ String.concat (List.indexedMap (nextToCase map) city.next) ++ """
                    default:
                        *current_city = -1;
                        return end_city;
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
                        *current_city = """ ++ String.fromInt index ++ """;
                        return """ ++ key ++ """_city;"""


cityToDeclarations : ( String, City ) -> String
cityToDeclarations ( name, city ) =
    let
        nextCount =
            List.length city.next

        ( labelsDeclaration, labels ) =
            if nextCount == 0 then
                ( "", "" )

            else
                ( "    const char * const " ++ name ++ "_labels[" ++ String.fromInt nextCount ++ "] = {" ++ String.join ", " (List.map (Tuple.first >> escape) city.next) ++ "};\n"
                , ", " ++ name ++ "_labels"
                )

        image =
            if String.isEmpty city.image then
                "0"

            else
                "&" ++ city.image ++ "_image"

        cityDeclaration =
            "    const city " ++ name ++ "_city = {" ++ escape city.text ++ ", " ++ image ++ ", " ++ String.fromInt nextCount ++ labels ++ "};"
    in
    "\n\n" ++ labelsDeclaration ++ cityDeclaration


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
