module Mensam.Element.Footer exposing (..)

import Element
import Element.Background
import Element.Events.Pointer
import Element.Font
import Html.Attributes
import Mensam.Element.Color
import Mensam.Element.Font


type alias Content =
    {}


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
            , elementTermsAndConditions
            , Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Events.Pointer.onClick <| \_ -> ClickedSomewhere
                , Element.alignBottom
                ]
                Element.none
            ]
        , Element.el
            [ Element.width <| Element.px 12
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
            , elementPrivacyPolicy
            , Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Events.Pointer.onClick <| \_ -> ClickedSomewhere
                , Element.alignBottom
                ]
                Element.none
            ]
        , Element.el
            [ Element.width <| Element.px 12
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
            , elementSource
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


elementSource : Element.Element Message
elementSource =
    Element.newTabLink
        [ Element.centerX
        , Element.centerY
        ]
        { url = "/openapi"
        , label =
            Element.el
                ([ Element.padding 3
                 , Element.Font.size 14
                 , Element.Font.color <| Mensam.Element.Color.dark.blue Mensam.Element.Color.Opaque100
                 , Element.mouseOver
                    [ Element.Font.color <| Mensam.Element.Color.bright.blue Mensam.Element.Color.Opaque100
                    ]
                 ]
                    ++ (Mensam.Element.Font.font <|
                            Mensam.Element.Font.Condensed
                                { weight = Mensam.Element.Font.Regular400
                                , italic = False
                                }
                       )
                )
            <|
                Element.text "OpenAPI"
        }


elementTermsAndConditions : Element.Element Message
elementTermsAndConditions =
    Element.newTabLink
        [ Element.centerX
        , Element.centerY
        ]
        { url = "/terms"
        , label =
            Element.el
                ([ Element.padding 3
                 , Element.Font.size 14
                 , Element.Font.color <| Mensam.Element.Color.dark.blue Mensam.Element.Color.Opaque100
                 , Element.mouseOver
                    [ Element.Font.color <| Mensam.Element.Color.bright.blue Mensam.Element.Color.Opaque100
                    ]
                 ]
                    ++ (Mensam.Element.Font.font <|
                            Mensam.Element.Font.Condensed
                                { weight = Mensam.Element.Font.Regular400
                                , italic = False
                                }
                       )
                )
            <|
                Element.text "Terms and Conditions"
        }


elementPrivacyPolicy : Element.Element Message
elementPrivacyPolicy =
    Element.newTabLink
        [ Element.centerX
        , Element.centerY
        ]
        { url = "/privacy"
        , label =
            Element.el
                ([ Element.padding 3
                 , Element.Font.size 14
                 , Element.Font.color <| Mensam.Element.Color.dark.blue Mensam.Element.Color.Opaque100
                 , Element.mouseOver
                    [ Element.Font.color <| Mensam.Element.Color.bright.blue Mensam.Element.Color.Opaque100
                    ]
                 ]
                    ++ (Mensam.Element.Font.font <|
                            Mensam.Element.Font.Condensed
                                { weight = Mensam.Element.Font.Regular400
                                , italic = False
                                }
                       )
                )
            <|
                Element.text "Privacy Policy"
        }
