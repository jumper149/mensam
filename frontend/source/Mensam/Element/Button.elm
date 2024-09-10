module Mensam.Element.Button exposing (..)

import Element
import Element.Background
import Element.Font
import Element.Input
import Html.Attributes
import Mensam.Element.Color
import Mensam.Element.Font


type Button msg
    = MkButton
        { size : Size
        , color : Color
        , label : Element.Element Never
        , message : Maybe msg
        , attributes : List (Element.Attribute msg)
        }


type Size
    = Small
    | Medium


type Color
    = Yellow
    | Blue
    | Red
    | Gray
    | Transparent


button : Button msg -> Element.Element msg
button (MkButton buttonData) =
    Element.Input.button
        ([ Element.padding <|
            case buttonData.size of
                Small ->
                    7

                Medium ->
                    10
         , Element.Background.color <|
            case buttonData.color of
                Yellow ->
                    Mensam.Element.Color.bright.yellow

                Blue ->
                    Mensam.Element.Color.bright.blue

                Red ->
                    Mensam.Element.Color.bright.red

                Gray ->
                    Mensam.Element.Color.dark.white

                Transparent ->
                    Mensam.Element.Color.transparent
         , Element.Font.color <|
            case buttonData.color of
                Yellow ->
                    Mensam.Element.Color.dark.black

                Blue ->
                    Mensam.Element.Color.dark.black

                Red ->
                    Mensam.Element.Color.dark.black

                Gray ->
                    Mensam.Element.Color.dark.black

                Transparent ->
                    Mensam.Element.Color.bright.blue
         , Element.mouseOver
            [ Element.Background.color <|
                case buttonData.color of
                    Yellow ->
                        Mensam.Element.Color.bright.green

                    Blue ->
                        Mensam.Element.Color.bright.green

                    Red ->
                        Mensam.Element.Color.bright.white

                    Gray ->
                        Mensam.Element.Color.bright.white

                    Transparent ->
                        Mensam.Element.Color.transparent
            , Element.Font.color <|
                case buttonData.color of
                    Yellow ->
                        Mensam.Element.Color.dark.black

                    Blue ->
                        Mensam.Element.Color.dark.black

                    Red ->
                        Mensam.Element.Color.dark.black

                    Gray ->
                        Mensam.Element.Color.dark.black

                    Transparent ->
                        Mensam.Element.Color.bright.green
            ]
         ]
            ++ buttonData.attributes
        )
        { onPress = buttonData.message
        , label =
            Element.el
                [ Element.centerX
                , Element.centerY
                , Element.Font.family [ Mensam.Element.Font.condensed ]
                , Element.Font.size <|
                    case buttonData.size of
                        Small ->
                            15

                        Medium ->
                            17
                , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                ]
            <|
                Element.map never buttonData.label
        }
