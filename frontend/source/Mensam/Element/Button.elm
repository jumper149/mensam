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
        , enabled : Bool
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
    let
        opacity =
            if buttonData.enabled then
                Mensam.Element.Color.Opaque100

            else
                Mensam.Element.Color.Opaque50

        attributes =
            [ Element.padding <|
                case buttonData.size of
                    Small ->
                        7

                    Medium ->
                        10
            , Element.Background.color <|
                case buttonData.color of
                    Yellow ->
                        Mensam.Element.Color.bright.yellow opacity

                    Blue ->
                        Mensam.Element.Color.bright.blue opacity

                    Red ->
                        Mensam.Element.Color.bright.red opacity

                    Gray ->
                        Mensam.Element.Color.dark.white opacity

                    Transparent ->
                        Mensam.Element.Color.transparent
            , Element.Font.color <|
                case buttonData.color of
                    Yellow ->
                        Mensam.Element.Color.dark.black opacity

                    Blue ->
                        Mensam.Element.Color.dark.black opacity

                    Red ->
                        Mensam.Element.Color.dark.black opacity

                    Gray ->
                        Mensam.Element.Color.dark.black opacity

                    Transparent ->
                        Mensam.Element.Color.bright.blue opacity
            , Element.mouseOver
                [ Element.Background.color <|
                    case buttonData.color of
                        Yellow ->
                            Mensam.Element.Color.bright.green opacity

                        Blue ->
                            Mensam.Element.Color.bright.green opacity

                        Red ->
                            Mensam.Element.Color.bright.white opacity

                        Gray ->
                            Mensam.Element.Color.bright.white opacity

                        Transparent ->
                            Mensam.Element.Color.transparent
                , Element.Font.color <|
                    case buttonData.color of
                        Yellow ->
                            Mensam.Element.Color.dark.black opacity

                        Blue ->
                            Mensam.Element.Color.dark.black opacity

                        Red ->
                            Mensam.Element.Color.dark.black opacity

                        Gray ->
                            Mensam.Element.Color.dark.black opacity

                        Transparent ->
                            Mensam.Element.Color.bright.green opacity
                ]
            ]
                ++ buttonData.attributes
    in
    if buttonData.enabled then
        Element.Input.button attributes
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

    else
        Element.el
            (attributes
                ++ [ Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                   ]
            )
        <|
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
