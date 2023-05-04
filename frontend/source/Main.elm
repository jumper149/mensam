module Main exposing (..)

import Json.Encode
import Mensam.Main


main : Program Json.Encode.Value Mensam.Main.Model Mensam.Main.Message
main =
    Mensam.Main.main
