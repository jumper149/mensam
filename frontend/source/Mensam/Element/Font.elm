module Mensam.Element.Font exposing (..)

import Element
import Element.Font


font : Font -> List (Element.Attribute msg)
font x =
    [ Element.Font.family
        [ case x of
            SansSerif _ ->
                sansSerif

            Condensed _ ->
                condensed

            Monospace400 ->
                monospace

            Monospace500 ->
                monospace

            Monospace700 ->
                monospace
        ]
    , case x of
        SansSerif { italic } ->
            if italic then
                Element.Font.italic

            else
                Element.Font.unitalicized

        Condensed { italic } ->
            if italic then
                Element.Font.italic

            else
                Element.Font.unitalicized

        Monospace400 ->
            Element.Font.unitalicized

        Monospace500 ->
            Element.Font.unitalicized

        Monospace700 ->
            Element.Font.unitalicized
    , case x of
        SansSerif { weight } ->
            fontWeight weight

        Condensed { weight } ->
            fontWeight weight

        Monospace400 ->
            fontWeight Regular400

        Monospace500 ->
            fontWeight Medium500

        Monospace700 ->
            fontWeight Bold700
    ]


fontWeight : Weight -> Element.Attribute msg
fontWeight weight =
    case weight of
        Hairline100 ->
            Element.Font.hairline

        ExtraLight200 ->
            Element.Font.extraLight

        Light300 ->
            Element.Font.light

        Regular400 ->
            Element.Font.regular

        Medium500 ->
            Element.Font.medium

        SemiBold600 ->
            Element.Font.semiBold

        Bold700 ->
            Element.Font.bold

        ExtraBold800 ->
            Element.Font.extraBold

        Heavy900 ->
            Element.Font.heavy


type Font
    = SansSerif
        { weight : Weight
        , italic : Bool
        }
    | Condensed
        { weight : Weight
        , italic : Bool
        }
    | Monospace400
    | Monospace500
    | Monospace700


type Weight
    = Hairline100
    | ExtraLight200
    | Light300
    | Regular400
    | Medium500
    | SemiBold600
    | Bold700
    | ExtraBold800
    | Heavy900


sansSerif : Element.Font.Font
sansSerif =
    Element.Font.typeface "Fira Sans"


condensed : Element.Font.Font
condensed =
    Element.Font.typeface "Fira Sans Condensed"


monospace : Element.Font.Font
monospace =
    Element.Font.typeface "Fira Mono"
