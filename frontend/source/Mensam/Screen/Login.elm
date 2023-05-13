module Mensam.Screen.Login exposing (..)

import Element
import Element.Background
import Element.Font
import Element.Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Mensam.Api.Login
import Mensam.Auth.Basic
import Mensam.Auth.Bearer
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Error
import Time


type alias Model =
    { username : String, password : String, hint : String }


init : Model
init =
    { username = "", password = "", hint = "" }


element : Model -> Element.Element Message
element model =
    Element.el
        [ Element.Background.color (Element.rgba 1 1 1 0.1)
        , Element.Font.color Mensam.Element.Color.bright.white
        , Element.Font.size 16
        , Element.centerX
        , Element.centerY
        ]
    <|
        Element.column
            [ Element.padding 20
            , Element.spacing 20
            ]
            [ Element.el
                [ Element.Font.size 30
                , Element.Font.hairline
                ]
              <|
                Element.text "Sign in"
            , Element.Input.username
                [ onEnter <| MessageEffect SubmitLogin
                , Element.Font.color Mensam.Element.Color.dark.black
                ]
                { onChange = MessagePure << EnterUsername
                , text = model.username
                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Username"
                , label = Element.Input.labelAbove [] <| Element.text "Username"
                }
            , Element.Input.currentPassword
                [ onEnter <| MessageEffect SubmitLogin
                , Element.Font.color Mensam.Element.Color.dark.black
                ]
                { onChange = MessagePure << EnterPassword
                , text = model.password
                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Password"
                , label = Element.Input.labelAbove [] <| Element.text "Password"
                , show = False
                }
            , Element.el
                [ Element.width Element.fill
                , Element.paddingEach
                    { top = 0
                    , right = 10
                    , bottom = 5
                    , left = 10
                    }
                ]
              <|
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 3
                    ]
                    [ Element.el
                        [ Element.height <| Element.px 14
                        , Element.paddingXY 5 0
                        , Element.Font.size 14
                        , Element.Font.color Mensam.Element.Color.bright.red
                        ]
                      <|
                        Element.text <|
                            model.hint
                    , Element.Input.button
                        [ Element.Background.color Mensam.Element.Color.bright.yellow
                        , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.green ]
                        , Element.Font.color Mensam.Element.Color.dark.black
                        , Element.width Element.fill
                        , Element.padding 10
                        ]
                        { onPress = Just <| MessageEffect <| SubmitLogin
                        , label =
                            Element.el
                                [ Element.centerX
                                , Element.centerY
                                , Element.Font.family [ Mensam.Element.Font.condensed ]
                                , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                ]
                            <|
                                Element.text "Sign in"
                        }
                    ]
            , Element.el
                [ Element.width Element.fill
                ]
              <|
                Element.row
                    [ Element.centerX
                    ]
                    [ Element.Input.button
                        [ Element.Font.color Mensam.Element.Color.bright.blue
                        , Element.mouseOver [ Element.Font.color Mensam.Element.Color.bright.green ]
                        ]
                        { onPress = Just <| MessageEffect <| Register
                        , label =
                            Element.el
                                [ Element.centerX
                                , Element.centerY
                                , Element.Font.family [ Mensam.Element.Font.condensed ]
                                , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                ]
                            <|
                                Element.text "Register"
                        }
                    ]
            ]


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect


type MessagePure
    = EnterUsername String
    | EnterPassword String
    | SetHintUsername
    | SetHintPassword


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        EnterUsername username ->
            { model | username = username }

        EnterPassword password ->
            { model | password = password }

        SetHintUsername ->
            { model | username = "", password = "", hint = "Invalid username." }

        SetHintPassword ->
            { model | password = "", hint = "Invalid password." }


type MessageEffect
    = ReportError Mensam.Error.Error
    | SubmitLogin
    | Register
    | SetSession { jwt : Mensam.Auth.Bearer.Jwt, expiration : Maybe Time.Posix }


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


login : Model -> Cmd Message
login model =
    Mensam.Api.Login.request
        (Mensam.Api.Login.BasicAuth <|
            Mensam.Auth.Basic.MkCredentials
                { username = model.username
                , password = model.password
                }
        )
    <|
        \result ->
            case result of
                Ok (Mensam.Api.Login.Success value) ->
                    MessageEffect <| SetSession value

                Ok (Mensam.Api.Login.ErrorAuth Mensam.Auth.Basic.ErrorUsername) ->
                    MessagePure <| SetHintUsername

                Ok (Mensam.Api.Login.ErrorAuth Mensam.Auth.Basic.ErrorPassword) ->
                    MessagePure <| SetHintPassword

                Ok (Mensam.Api.Login.ErrorAuth Mensam.Auth.Basic.ErrorIndefinite) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Login failed" <|
                                Mensam.Error.message "Authentication" <|
                                    Mensam.Error.message "Indefinite" Mensam.Error.undefined

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Login failed" <|
                                Mensam.Error.http error
