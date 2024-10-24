module Mensam.Screen.Login exposing (..)

import Element
import Element.Background
import Element.Font
import Element.Input
import Html.Events
import Json.Decode as Decode
import Mensam.Api.Login
import Mensam.Auth.Basic
import Mensam.Auth.Bearer
import Mensam.Element.Button
import Mensam.Element.Color
import Mensam.Error
import Mensam.Url
import Mensam.User
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
        , Element.Font.color <| Mensam.Element.Color.bright.white Mensam.Element.Color.Opaque100
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
                , Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                ]
                { onChange = MessagePure << EnterUsername
                , text = model.username
                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Username"
                , label = Element.Input.labelAbove [] <| Element.text "Username"
                }
            , Element.Input.currentPassword
                [ onEnter <| MessageEffect SubmitLogin
                , Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                ]
                { onChange = MessagePure << EnterPassword
                , text = model.password
                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Password"
                , label = Element.Input.labelAbove [] <| Element.text "Password"
                , show = False
                }
            , Element.column
                [ Element.width Element.fill
                , Element.paddingEach
                    { top = 0
                    , right = 10
                    , bottom = 5
                    , left = 10
                    }
                , Element.spacing 3
                ]
                [ Element.el
                    [ Element.height <| Element.px 14
                    , Element.paddingXY 5 0
                    , Element.Font.size 14
                    , Element.Font.color <| Mensam.Element.Color.bright.red Mensam.Element.Color.Opaque100
                    ]
                  <|
                    Element.text <|
                        model.hint
                , Mensam.Element.Button.button <|
                    Mensam.Element.Button.MkButton
                        { attributes = [ Element.width Element.fill ]
                        , color = Mensam.Element.Button.Yellow
                        , enabled = True
                        , label = Element.text "Sign in"
                        , message = Just <| MessageEffect SubmitLogin
                        , size = Mensam.Element.Button.Medium
                        }
                , Element.el [ Element.height <| Element.px 10 ] Element.none
                , Mensam.Element.Button.button <|
                    Mensam.Element.Button.MkButton
                        { attributes = [ Element.centerX ]
                        , color = Mensam.Element.Button.Transparent
                        , enabled = True
                        , label = Element.text "Register"
                        , message = Just <| MessageEffect Register
                        , size = Mensam.Element.Button.Medium
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
    | SetSession { jwt : Mensam.Auth.Bearer.Jwt, expiration : Maybe Time.Posix, id : Mensam.User.Identifier }


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


login : Mensam.Url.BaseUrl -> Model -> Cmd Message
login baseUrl model =
    Mensam.Api.Login.request baseUrl
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
