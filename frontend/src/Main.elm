module Main exposing (main)

{-| @docs main
-}

import Json.Encode as Encode
import Mensam.Main as Mensam


{-| abc
-}
main : Program Encode.Value Mensam.Model Mensam.Message
main =
    Mensam.main
