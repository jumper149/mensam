module Mensam.Element.Color exposing (..)

import Element
import Mensam.Color


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


type alias AnsiIso6429 =
    { black : Element.Color
    , red : Element.Color
    , green : Element.Color
    , yellow : Element.Color
    , blue : Element.Color
    , magenta : Element.Color
    , cyan : Element.Color
    , white : Element.Color
    }


transparent : Element.Color
transparent =
    Element.rgba255 0 0 0 0


toElementColor : Mensam.Color.Color -> Element.Color
toElementColor color =
    Element.rgb255 color.r color.g color.b
