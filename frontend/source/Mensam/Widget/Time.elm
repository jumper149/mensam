module Mensam.Widget.Time exposing (..)

import Element
import Element.Background
import Element.Font
import Element.Input
import Mensam.Element.Font
import Mensam.Time


type Model
    = MkModel
        { selected : Mensam.Time.Time
        }


unModel : Model -> { selected : Mensam.Time.Time }
unModel (MkModel value) =
    value


type Message
    = SetHour Mensam.Time.Hour
    | SetMinute Mensam.Time.Minute


elementPickTime : Model -> Element.Element Message
elementPickTime model =
    Element.el
        [ Element.width <| Element.px 200
        , Element.height <| Element.px 200
        , Element.centerX
        , Element.centerY
        ]
    <|
        Element.row
            ([ Element.centerX
             , Element.centerY
             , Element.padding 20
             , Element.Font.size 22
             ]
                ++ Mensam.Element.Font.font Mensam.Element.Font.Monospace400
            )
            [ Element.column
                [ Element.width <| Element.px 60
                , Element.height <| Element.px 90
                , Element.alignLeft
                , Element.centerY
                ]
                [ elementButton
                    { attributes = [ Element.alignTop ]
                    , message = SetHour <| increaseHour <| getHour model
                    , label = "+"
                    }
                , elementHour <| getHour model
                , elementButton
                    { attributes = [ Element.alignBottom ]
                    , message = SetHour <| decreaseHour <| getHour model
                    , label = "-"
                    }
                ]
            , Element.el
                [ Element.centerX
                , Element.centerY
                ]
              <|
                Element.text ":"
            , Element.column
                [ Element.width <| Element.px 60
                , Element.height <| Element.px 90
                , Element.alignRight
                , Element.centerY
                ]
                [ elementButton
                    { attributes = [ Element.alignTop ]
                    , message = SetMinute <| increaseMinute <| getMinute model
                    , label = "+"
                    }
                , elementMinute <| getMinute model
                , elementButton
                    { attributes = [ Element.alignBottom ]
                    , message = SetMinute <| decreaseMinute <| getMinute model
                    , label = "-"
                    }
                ]
            ]


getHour : Model -> Mensam.Time.Hour
getHour (MkModel model) =
    (Mensam.Time.unTime model.selected).hour


increaseHour : Mensam.Time.Hour -> Mensam.Time.Hour
increaseHour (Mensam.Time.MkHour hour) =
    Mensam.Time.MkHour <| modBy 24 <| hour + 1


decreaseHour : Mensam.Time.Hour -> Mensam.Time.Hour
decreaseHour (Mensam.Time.MkHour hour) =
    Mensam.Time.MkHour <| modBy 24 <| hour - 1


elementHour : Mensam.Time.Hour -> Element.Element Message
elementHour (Mensam.Time.MkHour hour) =
    element2Digit hour


getMinute : Model -> Mensam.Time.Minute
getMinute (MkModel model) =
    (Mensam.Time.unTime model.selected).minute


increaseMinute : Mensam.Time.Minute -> Mensam.Time.Minute
increaseMinute (Mensam.Time.MkMinute minute) =
    Mensam.Time.MkMinute <| modBy 60 <| minute + 5


decreaseMinute : Mensam.Time.Minute -> Mensam.Time.Minute
decreaseMinute (Mensam.Time.MkMinute minute) =
    Mensam.Time.MkMinute <| modBy 60 <| minute - 5


elementMinute : Mensam.Time.Minute -> Element.Element Message
elementMinute (Mensam.Time.MkMinute minute) =
    element2Digit minute


element2Digit : Int -> Element.Element Message
element2Digit n =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
    <|
        Element.el
            [ Element.centerX
            , Element.centerY
            ]
        <|
            Element.text <|
                String.padLeft 2 '0' <|
                    String.fromInt n


elementButton :
    { attributes : List (Element.Attribute Never)
    , message : Message
    , label : String
    }
    -> Element.Element Message
elementButton args =
    Element.Input.button
        ([ Element.width Element.fill
         , Element.height <| Element.px 25
         , Element.Background.color <| Element.rgba 1 1 1 0.05
         , Element.mouseOver
            [ Element.Background.color <| Element.rgba 1 1 1 0.1
            ]
         ]
            ++ List.map (Element.mapAttribute never) args.attributes
        )
        { onPress = Just args.message
        , label =
            Element.el
                [ Element.centerX
                , Element.centerY
                ]
            <|
                Element.text args.label
        }
