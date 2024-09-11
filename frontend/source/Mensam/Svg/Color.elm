module Mensam.Svg.Color exposing (..)

import Mensam.Color


type alias Color =
    String


dark : AnsiIso6429
dark =
    { black = toSvgColor Mensam.Color.dark.black
    , red = toSvgColor Mensam.Color.dark.red
    , green = toSvgColor Mensam.Color.dark.green
    , yellow = toSvgColor Mensam.Color.dark.yellow
    , blue = toSvgColor Mensam.Color.dark.blue
    , magenta = toSvgColor Mensam.Color.dark.magenta
    , cyan = toSvgColor Mensam.Color.dark.cyan
    , white = toSvgColor Mensam.Color.dark.white
    }


bright : AnsiIso6429
bright =
    { black = toSvgColor Mensam.Color.bright.black
    , red = toSvgColor Mensam.Color.bright.red
    , green = toSvgColor Mensam.Color.bright.green
    , yellow = toSvgColor Mensam.Color.bright.yellow
    , blue = toSvgColor Mensam.Color.bright.blue
    , magenta = toSvgColor Mensam.Color.bright.magenta
    , cyan = toSvgColor Mensam.Color.bright.cyan
    , white = toSvgColor Mensam.Color.bright.white
    }


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


transparent : Color
transparent =
    "none"


toSvgColor : Mensam.Color.Color -> Color
toSvgColor color =
    String.concat
        [ "rgb("
        , String.fromInt color.r
        , ","
        , String.fromInt color.g
        , ","
        , String.fromInt color.b
        , ")"
        ]
