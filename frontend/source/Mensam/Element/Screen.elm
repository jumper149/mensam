module Mensam.Element.Screen exposing (..)

import Element
import Element.Background
import Element.Font
import Mensam.Element.Color
import Mensam.Element.Font


type alias Screen msg =
    { main : Element.Element msg
    , popup : Maybe (Element.Element msg)
    }


element : Screen msg -> Element.Element msg
element screen =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 10
        , Element.Background.color (Element.rgba 0 0 0 0.1)
        , Element.Font.size 16
        , Element.Font.family [ Mensam.Element.Font.sansSerif ]
        , Element.inFront <|
            case screen.popup of
                Nothing ->
                    Element.none

                Just popup ->
                    Element.el
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.paddingXY 30 30
                        ]
                    <|
                        Element.el
                            [ Element.Background.color Mensam.Element.Color.bright.black
                            , Element.centerX
                            , Element.width <| Element.maximum 500 <| Element.fill
                            , Element.height <| Element.px 465
                            , Element.paddingXY 30 30
                            ]
                        <|
                            popup
        ]
    <|
        screen.main
