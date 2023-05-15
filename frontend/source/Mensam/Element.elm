module Mensam.Element exposing (..)

import Browser
import Element
import Element.Background
import Element.Font
import Html.Attributes
import Mensam.Element.Color
import Mensam.Element.Font


document : Element.Element msg -> Browser.Document msg
document element =
    { title = "Mensam"
    , body =
        [ Element.layout
            [ Element.Background.gradient
                { angle = 0
                , steps = [ Mensam.Element.Color.dark.yellow, Mensam.Element.Color.bright.yellow ]
                }
            , Element.Font.color Mensam.Element.Color.bright.white
            , Element.Font.alignLeft
            , Element.Font.family [ Mensam.Element.Font.sansSerif ]
            , Element.Font.regular
            , Element.Font.size 16
            ]
          <|
            Element.el
                [ Element.htmlAttribute <| Html.Attributes.style "min-width" "393px"
                , Element.htmlAttribute <| Html.Attributes.style "max-width" "851px"
                , Element.htmlAttribute <| Html.Attributes.style "margin-left" "auto"
                , Element.htmlAttribute <| Html.Attributes.style "margin-right" "auto"
                , Element.width Element.fill
                , Element.height <| Element.minimum 851 Element.fill
                , Element.Background.color Mensam.Element.Color.dark.black
                ]
                element
        ]
    }


screen : (msgScreen -> msg) -> Element.Element msgScreen -> Element.Element msg
screen embedMessage element =
    Element.map embedMessage <|
        Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            element
