module Mensam.Element.Footer exposing (..)

import Element
import Element.Background
import Element.Events.Pointer
import Element.Font
import Html.Attributes
import Mensam.Element.Color
import Mensam.Element.Font


type alias Content =
    { sourceUrl : String
    }


type Message
    = ClickedSomewhere


element : Content -> Element.Element Message
element content =
    Element.row
        [ Element.width Element.fill
        , Element.height <| Element.px 45
        , Element.Background.color <| Element.rgba 1 1 1 0.1
        , Element.Font.family [ Mensam.Element.Font.condensed ]
        , Element.Font.light
        , Element.Font.size 17
        , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
        , Element.clip
        ]
        [ Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.Events.Pointer.onClick <| \_ -> ClickedSomewhere
            , Element.alignLeft
            ]
            Element.none
        , Element.column
            [ Element.height Element.fill ]
            [ Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Events.Pointer.onClick <| \_ -> ClickedSomewhere
                , Element.alignTop
                ]
                Element.none
            , elementSource content
            , Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Events.Pointer.onClick <| \_ -> ClickedSomewhere
                , Element.alignBottom
                ]
                Element.none
            ]
        , Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.Events.Pointer.onClick <| \_ -> ClickedSomewhere
            , Element.alignRight
            ]
            Element.none
        ]


elementSource : Content -> Element.Element Message
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
                , Element.Font.color <| Mensam.Element.Color.dark.blue Mensam.Element.Color.Opaque100
                , Element.mouseOver
                    [ Element.Font.color <| Mensam.Element.Color.bright.blue Mensam.Element.Color.Opaque100
                    ]
                ]
            <|
                Element.text "Source on GitHub"
        }
