module Mensam.Icon exposing (..)

import Html
import Mensam.Svg.Color
import Svg
import Svg.Attributes


user : Html.Html msg
user =
    Svg.svg
        [ Svg.Attributes.width <| String.fromInt 20
        , Svg.Attributes.height <| String.fromInt 20
        , Svg.Attributes.viewBox <|
            String.concat
                [ String.fromInt -10
                , " "
                , String.fromInt -10
                , " "
                , String.fromInt 20
                , " "
                , String.fromInt 20
                ]
        , Svg.Attributes.transform "scale(1,-1)"
        ]
        [ Svg.polyline
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke Mensam.Svg.Color.dark.black
            , Svg.Attributes.points "-5,-9 -5,1 5,1, 5,-9"
            ]
            []
        , Svg.rect
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke Mensam.Svg.Color.dark.black
            , Svg.Attributes.x "-3"
            , Svg.Attributes.y "4"
            , Svg.Attributes.width "6"
            , Svg.Attributes.height "5"
            ]
            []
        ]


settings : Html.Html msg
settings =
    Svg.svg
        [ Svg.Attributes.width <| String.fromInt 20
        , Svg.Attributes.height <| String.fromInt 20
        , Svg.Attributes.viewBox <|
            String.concat
                [ String.fromInt -10
                , " "
                , String.fromInt -10
                , " "
                , String.fromInt 20
                , " "
                , String.fromInt 20
                ]
        , Svg.Attributes.transform "scale(1,-1)"
        ]
        [ Svg.polyline
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke Mensam.Svg.Color.dark.black
            , Svg.Attributes.points "-9,5 -3.5,5"
            ]
            []
        , Svg.polyline
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke Mensam.Svg.Color.dark.black
            , Svg.Attributes.points "-0.5,5 9,5"
            ]
            []
        , Svg.rect
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke Mensam.Svg.Color.dark.black
            , Svg.Attributes.x "-3.5"
            , Svg.Attributes.y "3.5"
            , Svg.Attributes.width "3"
            , Svg.Attributes.height "3"
            ]
            []
        , Svg.polyline
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke Mensam.Svg.Color.dark.black
            , Svg.Attributes.points "-9,0 4,0"
            ]
            []
        , Svg.polyline
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke Mensam.Svg.Color.dark.black
            , Svg.Attributes.points "7,0 9,0"
            ]
            []
        , Svg.rect
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke Mensam.Svg.Color.dark.black
            , Svg.Attributes.x "4"
            , Svg.Attributes.y "-1.5"
            , Svg.Attributes.width "3"
            , Svg.Attributes.height "3"
            ]
            []
        , Svg.polyline
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke Mensam.Svg.Color.dark.black
            , Svg.Attributes.points "-9,-5 -5,-5"
            ]
            []
        , Svg.polyline
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke Mensam.Svg.Color.dark.black
            , Svg.Attributes.points "-2,-5 9,-5"
            ]
            []
        , Svg.rect
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke Mensam.Svg.Color.dark.black
            , Svg.Attributes.x "-5"
            , Svg.Attributes.y "-6.5"
            , Svg.Attributes.width "3"
            , Svg.Attributes.height "3"
            ]
            []
        ]
