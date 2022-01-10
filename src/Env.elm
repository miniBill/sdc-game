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
    "/art"


imageToUrl : String -> String
imageToUrl src =
    let
        segments =
            src
                |> String.split "/"
                |> List.filter (not << String.isEmpty)
    in
    if filesBaseUrl == "/art" then
        Url.Builder.absolute ("art" :: segments) []

    else
        Url.Builder.crossOrigin filesBaseUrl segments []
