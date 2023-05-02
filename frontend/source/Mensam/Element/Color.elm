module Mensam.Element.Color exposing (..)

import Element


dark : AnsiIso6429
dark =
    { black = Element.rgb255 40 42 46
    , red = Element.rgb255 165 66 66
    , green = Element.rgb255 140 148 64
    , yellow = Element.rgb255 222 147 95
    , blue = Element.rgb255 95 129 157
    , magenta = Element.rgb255 133 103 143
    , cyan = Element.rgb255 94 141 135
    , white = Element.rgb255 112 120 128
    }


bright : AnsiIso6429
bright =
    { black = Element.rgb255 55 59 65
    , red = Element.rgb255 204 102 102
    , green = Element.rgb255 181 189 104
    , yellow = Element.rgb255 240 198 116
    , blue = Element.rgb255 129 162 190
    , magenta = Element.rgb255 178 148 187
    , cyan = Element.rgb255 138 190 183
    , white = Element.rgb255 197 200 198
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
