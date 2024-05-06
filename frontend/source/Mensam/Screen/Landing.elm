module Mensam.Screen.Landing exposing (..)

import Element
import Element.Font
import Mensam.Element.Button
import Mensam.Element.Color


type alias Model =
    ()


init : Model
init =
    ()


element : Model -> Element.Element Message
element () =
    Element.el
        [ Element.Font.color Mensam.Element.Color.bright.white
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
                , Element.Font.color Mensam.Element.Color.bright.yellow
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
                    [ Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = [ Element.width Element.fill ]
                            , color = Mensam.Element.Button.Yellow
                            , message = Just <| MessageEffect Register
                            , text = "Sign up"
                            }
                    , Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = [ Element.width Element.fill ]
                            , color = Mensam.Element.Button.Yellow
                            , message = Just <| MessageEffect Login
                            , text = "Sign in"
                            }
                    ]
            , Element.el
                [ Element.Font.size 30
                , Element.Font.bold
                , Element.Font.color Mensam.Element.Color.bright.magenta
                , Element.centerX
                ]
              <|
                Element.paragraph
                    []
                    [ Element.text "WORK IN PROGRESS"
                    ]
            , Element.el
                [ Element.Font.size 20
                , Element.Font.regular
                , Element.Font.color Mensam.Element.Color.dark.magenta
                , Element.centerX
                ]
              <|
                Element.paragraph
                    []
                    [ Element.text "Don't expect this to work yet!"
                    ]
            ]


type Message
    = MessageEffect MessageEffect


type MessageEffect
    = Login
    | Register
