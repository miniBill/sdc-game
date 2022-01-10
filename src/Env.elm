module Env exposing (..)

import Url.Builder


backblazeKeyID : String
backblazeKeyID =
    ""


backblazeKeyName : String
backblazeKeyName =
    ""


backblazeApplicationKey : String
backblazeApplicationKey =
    ""


filesBaseUrl : String
filesBaseUrl =
    "/"


imageToUrl : String -> String
imageToUrl src =
    let
        segments =
            src
                |> String.split "/"
                |> List.filter (not << String.isEmpty)
    in
    if filesBaseUrl == "/" then
        Url.Builder.absolute segments []

    else
        Url.Builder.crossOrigin filesBaseUrl segments []
