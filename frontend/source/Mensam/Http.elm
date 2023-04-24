module Mensam.Http exposing (..)

import Http


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl x ->
            "HTTP: Bad URL: " ++ x

        Http.Timeout ->
            "HTTP: Timeout."

        Http.NetworkError ->
            "HTTP: Network."

        Http.BadStatus status ->
            "HTTP: Bad Status: " ++ String.fromInt status

        Http.BadBody body ->
            "HTTP: Bad Body: " ++ body
