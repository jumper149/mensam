module Main exposing (main)

import Json.Encode as Json
import Mensam.Main as Mensam


main : Program Json.Value Mensam.Model Mensam.Message
main =
    Mensam.main
