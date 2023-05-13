module Main exposing (main)

import Json.Encode as Encode
import Mensam.Main as Mensam


main : Program Encode.Value Mensam.Model Mensam.Message
main =
    Mensam.main
