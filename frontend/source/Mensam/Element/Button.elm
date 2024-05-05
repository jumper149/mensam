module Mensam.Element.Button exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Html.Attributes
import Mensam.Element.Color
import Mensam.Element.Font


type Button msg
    = MkButton
        { color : Color
        , text : String
        , message : msg
        , attributes : List (Element.Attribute msg)
        }


type Color
    = Yellow
    | Blue
    | Red


button : Button msg -> Element.Element msg
button (MkButton buttonData) =
    Element.el
        ([ Element.padding 10
         , Element.Background.color <|
            case buttonData.color of
                Yellow ->
                    Mensam.Element.Color.bright.yellow

                Blue ->
                    Mensam.Element.Color.bright.blue

                Red ->
                    Mensam.Element.Color.bright.red
         , Element.mouseOver
            [ Element.Background.color <|
                case buttonData.color of
                    Yellow ->
                        Mensam.Element.Color.bright.green

                    Blue ->
                        Mensam.Element.Color.bright.green

                    Red ->
                        Mensam.Element.Color.bright.white
            ]
         , Element.Font.color Mensam.Element.Color.dark.black
         , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
         , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
         , Element.Events.onClick buttonData.message
         ]
            ++ buttonData.attributes
        )
    <|
        Element.el
            [ Element.centerX
            , Element.centerY
            , Element.Font.family [ Mensam.Element.Font.condensed ]
            , Element.Font.size 17
            , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
            ]
        <|
            Element.text buttonData.text
