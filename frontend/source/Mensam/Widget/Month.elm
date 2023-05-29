module Mensam.Widget.Month exposing (..)

import Element
import Element.Background
import Element.Events
import Html.Attributes
import Mensam.Time


type Model
    = MkModel
        { year : Mensam.Time.Year
        , month : Mensam.Time.Month
        }


type Message
    = PreviousMonth
    | NextMonth


elementPickMonth : Model -> Element.Element Message
elementPickMonth (MkModel model) =
    Element.el
        [ Element.width <| Element.px 230
        , Element.height <| Element.px 40
        , Element.spaceEvenly
        , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
        ]
    <|
        Element.row
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ Element.el
                [ Element.width <| Element.px 40
                , Element.height <| Element.px 40
                , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                , Element.mouseOver
                    [ Element.Background.color <| Element.rgba 1 1 1 0.1
                    ]
                , Element.Events.onClick PreviousMonth
                ]
              <|
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                <|
                    Element.text "<"
            , Element.el
                [ Element.width <| Element.fill
                , Element.height <| Element.fill
                ]
              <|
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                <|
                    Element.text <|
                        Mensam.Time.yearToString model.year
                            ++ ", "
                            ++ Mensam.Time.monthToString model.month
            , Element.el
                [ Element.width <| Element.px 35
                , Element.height <| Element.px 35
                , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                , Element.mouseOver
                    [ Element.Background.color <| Element.rgba 1 1 1 0.1
                    ]
                , Element.Events.onClick NextMonth
                ]
              <|
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                <|
                    Element.text ">"
            ]
