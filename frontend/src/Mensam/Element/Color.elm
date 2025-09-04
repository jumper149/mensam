module Mensam.Element.Color exposing (..)

import Element
import Mensam.Color


type alias Color =
    Transparency -> Element.Color


dark : AnsiIso6429
dark =
    { black = toElementColor Mensam.Color.dark.black
    , red = toElementColor Mensam.Color.dark.red
    , green = toElementColor Mensam.Color.dark.green
    , yellow = toElementColor Mensam.Color.dark.yellow
    , blue = toElementColor Mensam.Color.dark.blue
    , magenta = toElementColor Mensam.Color.dark.magenta
    , cyan = toElementColor Mensam.Color.dark.cyan
    , white = toElementColor Mensam.Color.dark.white
    }


bright : AnsiIso6429
bright =
    { black = toElementColor Mensam.Color.bright.black
    , red = toElementColor Mensam.Color.bright.red
    , green = toElementColor Mensam.Color.bright.green
    , yellow = toElementColor Mensam.Color.bright.yellow
    , blue = toElementColor Mensam.Color.bright.blue
    , magenta = toElementColor Mensam.Color.bright.magenta
    , cyan = toElementColor Mensam.Color.bright.cyan
    , white = toElementColor Mensam.Color.bright.white
    }


transparent : Element.Color
transparent =
    Element.rgba255 0 0 0 0


type Transparency
    = Opaque100
    | Opaque50
    | Opaque25
    | Opaque10
    | Opaque05


type alias AnsiIso6429 =
    { black : Color
    , red : Color
    , green : Color
    , yellow : Color
    , blue : Color
    , magenta : Color
    , cyan : Color
    , white : Color
    }


toElementColor : Mensam.Color.Color -> Transparency -> Element.Color
toElementColor color transparency =
    Element.rgba255 color.r color.g color.b <|
        case transparency of
            Opaque100 ->
                1

            Opaque50 ->
                0.5

            Opaque25 ->
                0.25

            Opaque10 ->
                0.1

            Opaque05 ->
                0.05
