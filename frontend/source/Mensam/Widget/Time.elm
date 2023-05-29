module Mensam.Widget.Time exposing (..)

import Element
import Mensam.Time
import Svg
import Svg.Attributes
import Svg.Events


type Message
    = SetHour Mensam.Time.Hour
    | SetMinute Mensam.Time.Minute


elementPickTime : Mensam.Time.Time -> Element.Element Message
elementPickTime (Mensam.Time.MkTime _) =
    Element.el
        [ Element.width <| Element.px 200 ]
    <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "-120 -120 240 240"
                , Svg.Attributes.width "100%"
                ]
            <|
                Svg.circle
                    [ Svg.Attributes.cx "0"
                    , Svg.Attributes.cy "0"
                    , Svg.Attributes.r "120"
                    , Svg.Attributes.fill "rgba(255,255,255,0.1)"
                    ]
                    []
                    :: List.concat
                        [ svgOnCircle 100 "00" (SetMinute <| Mensam.Time.MkMinute 0) 0 -1
                        , svgOnCircle 100 "05" (SetMinute <| Mensam.Time.MkMinute 5) (1 / 2) -(sqrt 3 / 2)
                        , svgOnCircle 100 "10" (SetMinute <| Mensam.Time.MkMinute 10) (sqrt 3 / 2) -(1 / 2)
                        , svgOnCircle 100 "15" (SetMinute <| Mensam.Time.MkMinute 15) 1 0
                        , svgOnCircle 100 "20" (SetMinute <| Mensam.Time.MkMinute 20) (sqrt 3 / 2) (1 / 2)
                        , svgOnCircle 100 "25" (SetMinute <| Mensam.Time.MkMinute 25) (1 / 2) (sqrt 3 / 2)
                        , svgOnCircle 100 "30" (SetMinute <| Mensam.Time.MkMinute 30) 0 1
                        , svgOnCircle 100 "35" (SetMinute <| Mensam.Time.MkMinute 35) -(1 / 2) (sqrt 3 / 2)
                        , svgOnCircle 100 "40" (SetMinute <| Mensam.Time.MkMinute 40) -(sqrt 3 / 2) (1 / 2)
                        , svgOnCircle 100 "45" (SetMinute <| Mensam.Time.MkMinute 45) -1 0
                        , svgOnCircle 100 "50" (SetMinute <| Mensam.Time.MkMinute 50) -(sqrt 3 / 2) -(1 / 2)
                        , svgOnCircle 100 "55" (SetMinute <| Mensam.Time.MkMinute 55) -(1 / 2) -(sqrt 3 / 2)
                        , svgOnCircle 60 "12" (SetHour <| Mensam.Time.MkHour 12) 0 -1
                        , svgOnCircle 60 "1" (SetHour <| Mensam.Time.MkHour 1) (1 / 2) -(sqrt 3 / 2)
                        , svgOnCircle 60 "2" (SetHour <| Mensam.Time.MkHour 2) (sqrt 3 / 2) -(1 / 2)
                        , svgOnCircle 60 "3" (SetHour <| Mensam.Time.MkHour 3) 1 0
                        , svgOnCircle 60 "4" (SetHour <| Mensam.Time.MkHour 4) (sqrt 3 / 2) (1 / 2)
                        , svgOnCircle 60 "5" (SetHour <| Mensam.Time.MkHour 5) (1 / 2) (sqrt 3 / 2)
                        , svgOnCircle 60 "6" (SetHour <| Mensam.Time.MkHour 6) 0 1
                        , svgOnCircle 60 "7" (SetHour <| Mensam.Time.MkHour 7) -(1 / 2) (sqrt 3 / 2)
                        , svgOnCircle 60 "8" (SetHour <| Mensam.Time.MkHour 8) -(sqrt 3 / 2) (1 / 2)
                        , svgOnCircle 60 "9" (SetHour <| Mensam.Time.MkHour 9) -1 0
                        , svgOnCircle 60 "10" (SetHour <| Mensam.Time.MkHour 10) -(sqrt 3 / 2) -(1 / 2)
                        , svgOnCircle 60 "11" (SetHour <| Mensam.Time.MkHour 11) -(1 / 2) -(sqrt 3 / 2)
                        , svgOnCircle 40 "0" (SetHour <| Mensam.Time.MkHour 0) 0 -1
                        , svgOnCircle 40 "13" (SetHour <| Mensam.Time.MkHour 13) (1 / 2) -(sqrt 3 / 2)
                        , svgOnCircle 40 "14" (SetHour <| Mensam.Time.MkHour 14) (sqrt 3 / 2) -(1 / 2)
                        , svgOnCircle 40 "15" (SetHour <| Mensam.Time.MkHour 15) 1 0
                        , svgOnCircle 40 "16" (SetHour <| Mensam.Time.MkHour 16) (sqrt 3 / 2) (1 / 2)
                        , svgOnCircle 40 "17" (SetHour <| Mensam.Time.MkHour 17) (1 / 2) (sqrt 3 / 2)
                        , svgOnCircle 40 "18" (SetHour <| Mensam.Time.MkHour 18) 0 1
                        , svgOnCircle 40 "19" (SetHour <| Mensam.Time.MkHour 19) -(1 / 2) (sqrt 3 / 2)
                        , svgOnCircle 40 "20" (SetHour <| Mensam.Time.MkHour 20) -(sqrt 3 / 2) (1 / 2)
                        , svgOnCircle 40 "21" (SetHour <| Mensam.Time.MkHour 21) -1 0
                        , svgOnCircle 40 "22" (SetHour <| Mensam.Time.MkHour 22) -(sqrt 3 / 2) -(1 / 2)
                        , svgOnCircle 40 "23" (SetHour <| Mensam.Time.MkHour 23) -(1 / 2) -(sqrt 3 / 2)
                        ]


svgOnCircle : Float -> String -> msg -> Float -> Float -> List (Svg.Svg msg)
svgOnCircle factor string msg x y =
    [ Svg.circle
        [ Svg.Attributes.cx <| String.fromFloat <| factor * x
        , Svg.Attributes.cy <| String.fromFloat <| factor * y
        , Svg.Attributes.r "10"
        , Svg.Attributes.fill "rgba(255,255,255,0.1)"
        ]
        []
    , Svg.text_
        [ Svg.Attributes.x <| String.fromFloat <| factor * x
        , Svg.Attributes.y <| String.fromFloat <| factor * y
        , Svg.Attributes.textAnchor "middle"
        , Svg.Attributes.dominantBaseline "central"
        , Svg.Attributes.style "user-select: none;"
        ]
        [ Svg.text string ]
    , Svg.circle
        [ Svg.Attributes.cx <| String.fromFloat <| factor * x
        , Svg.Attributes.cy <| String.fromFloat <| factor * y
        , Svg.Attributes.r "10"
        , Svg.Attributes.style "cursor: pointer;"
        , Svg.Attributes.fill "rgba(255,255,255,0.0)"
        , Svg.Events.onClick msg
        ]
        []
    ]
