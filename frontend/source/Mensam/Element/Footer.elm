module Mensam.Element.Footer exposing (..)

import Element
import Element.Background
import Element.Font
import Html.Attributes
import Mensam.Element.Color
import Mensam.Element.Font


type alias Content =
    { sourceUrl : String
    }


element : Content -> Element.Element msg
element content =
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px 45
        , Element.Background.color <| Element.rgba 1 1 1 0.1
        , Element.Font.family [ Mensam.Element.Font.condensed ]
        , Element.Font.light
        , Element.Font.size 17
        , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
        , Element.clip
        ]
    <|
        Element.row
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ elementSource content
            ]


elementSource : Content -> Element.Element msg
elementSource content =
    Element.newTabLink
        [ Element.centerX
        , Element.centerY
        ]
        { url = content.sourceUrl
        , label =
            Element.el
                [ Element.padding 3
                , Element.Font.family [ Mensam.Element.Font.monospace ]
                , Element.Font.regular
                , Element.Font.size 14
                , Element.Font.color Mensam.Element.Color.dark.blue
                , Element.mouseOver
                    [ Element.Font.color Mensam.Element.Color.bright.blue
                    ]
                ]
            <|
                Element.text "Source on GitHub"
        }
