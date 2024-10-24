module Mensam.Color exposing (..)


type alias Color =
    { r : Int, g : Int, b : Int }


dark : AnsiIso6429
dark =
    { black = { r = 40, g = 42, b = 46 }
    , red = { r = 165, g = 66, b = 66 }
    , green = { r = 140, g = 148, b = 64 }
    , yellow = { r = 222, g = 147, b = 95 }
    , blue = { r = 95, g = 129, b = 157 }
    , magenta = { r = 133, g = 103, b = 143 }
    , cyan = { r = 94, g = 141, b = 135 }
    , white = { r = 112, g = 120, b = 128 }
    }


bright : AnsiIso6429
bright =
    { black = { r = 55, g = 59, b = 65 }
    , red = { r = 204, g = 102, b = 102 }
    , green = { r = 181, g = 189, b = 104 }
    , yellow = { r = 240, g = 198, b = 116 }
    , blue = { r = 129, g = 162, b = 190 }
    , magenta = { r = 178, g = 148, b = 187 }
    , cyan = { r = 138, g = 190, b = 183 }
    , white = { r = 197, g = 200, b = 198 }
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
