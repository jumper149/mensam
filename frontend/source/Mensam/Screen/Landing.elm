module Mensam.Screen.Landing exposing (..)

import Element
import Element.Background
import Element.Font
import Element.Input
import Html.Attributes
import Mensam.Color
import Mensam.Error
import Mensam.Font


type alias Model =
    ()


init : Model
init =
    ()


element : Model -> Element.Element Message
element model =
    Element.el
        [ Element.Font.color Mensam.Color.bright.white
        , Element.Font.size 16
        , Element.width Element.fill
        , Element.height Element.fill
        ]
    <|
        Element.column
            [ Element.spacing 20
            , Element.centerX
            , Element.centerY
            , Element.width <| Element.maximum 300 Element.fill
            ]
            [ Element.el
                [ Element.Font.size 40
                , Element.Font.extraLight
                , Element.Font.italic
                , Element.Font.color Mensam.Color.bright.yellow
                , Element.centerX
                ]
              <|
                Element.text "Mensam"
            , Element.el
                [ Element.Font.size 18
                , Element.Font.extraLight
                , Element.centerX
                ]
              <|
                Element.paragraph
                    []
                    [ Element.text "A Desk-Booking Application." ]
            , Element.el
                [ Element.width Element.fill
                ]
              <|
                Element.row
                    [ Element.width Element.fill
                    , Element.spacing 30
                    ]
                    [ Element.Input.button
                        [ Element.Background.color Mensam.Color.bright.yellow
                        , Element.mouseOver [ Element.Background.color Mensam.Color.bright.green ]
                        , Element.Font.color Mensam.Color.dark.black
                        , Element.width Element.fill
                        , Element.padding 10
                        ]
                        { onPress = Just <| MessageEffect <| Register
                        , label =
                            Element.el
                                [ Element.centerX
                                , Element.centerY
                                , Element.Font.family [ Mensam.Font.condensed ]
                                , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                ]
                            <|
                                Element.text "Sign up"
                        }
                    , Element.Input.button
                        [ Element.Background.color Mensam.Color.bright.yellow
                        , Element.mouseOver [ Element.Background.color Mensam.Color.bright.green ]
                        , Element.Font.color Mensam.Color.dark.black
                        , Element.width Element.fill
                        , Element.padding 10
                        ]
                        { onPress = Just <| MessageEffect <| Login
                        , label =
                            Element.el
                                [ Element.centerX
                                , Element.centerY
                                , Element.Font.family [ Mensam.Font.condensed ]
                                , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                ]
                            <|
                                Element.text "Sign in"
                        }
                    ]
            ]


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect


type MessagePure
    = EmptyMessage


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        EmptyMessage ->
            model


type MessageEffect
    = ReportError Mensam.Error.Error
    | Login
    | Register