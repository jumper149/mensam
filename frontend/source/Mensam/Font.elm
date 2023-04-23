module Mensam.Font exposing (..)

import Element
import Element.Font


families : Element.Attribute msg
families =
    Element.Font.family
        [ Element.Font.external
            { url = "static/fonts.css"
            , name = "Fira Sans"
            }
        , Element.Font.external
            { url = "static/fonts.css"
            , name = "Fira Sans Condensed"
            }
        , Element.Font.external
            { url = "static/fonts.css"
            , name = "Fira Sans Mono"
            }
        ]


sansSerif : Element.Font.Font
sansSerif =
    Element.Font.typeface "Fira Sans"


condensed : Element.Font.Font
condensed =
    Element.Font.typeface "Fira Sans Condensed"


monospace : Element.Font.Font
monospace =
    Element.Font.typeface "Fira Mono"
