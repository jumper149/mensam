module Mensam.Element.Screen exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Mensam.Element.Color
import Mensam.Element.Font


type alias Screen msg =
    { main : Element.Element msg
    , popup : Maybe (Element.Element msg)
    , closePopup : msg
    }


element : Screen msg -> Element.Element msg
element screen =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Font.size 16
        , Element.Font.family [ Mensam.Element.Font.sansSerif ]
        , Element.inFront <|
            case screen.popup of
                Nothing ->
                    Element.none

                Just popup ->
                    Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.padding 0
                        , Element.spacing 0
                        ]
                        [ Element.row
                            [ Element.width Element.fill
                            , Element.height <| Element.px 30
                            , Element.alignTop
                            , Element.Events.onClick screen.closePopup
                            ]
                            []
                        , Element.row
                            [ Element.width Element.fill
                            , Element.height <| Element.px 550
                            , Element.padding 0
                            , Element.spacing 0
                            ]
                            [ Element.column
                                [ Element.width Element.fill
                                , Element.height Element.fill
                                , Element.alignLeft
                                , Element.Events.onClick screen.closePopup
                                ]
                                []
                            , Element.column
                                [ Element.Background.color Mensam.Element.Color.bright.black
                                , Element.width <| Element.minimum 300 <| Element.maximum 330 <| Element.fill
                                , Element.height Element.fill
                                , Element.centerX
                                , Element.paddingXY 30 30
                                ]
                                [ popup ]
                            , Element.column
                                [ Element.width Element.fill
                                , Element.height Element.fill
                                , Element.alignRight
                                , Element.Events.onClick screen.closePopup
                                ]
                                []
                            ]
                        , Element.row
                            [ Element.width Element.fill
                            , Element.height Element.fill
                            , Element.alignBottom
                            , Element.Events.onClick screen.closePopup
                            ]
                            []
                        ]
        ]
    <|
        screen.main
