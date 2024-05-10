module Mensam.Screen.Register exposing (..)

import Element
import Element.Background
import Element.Font
import Element.Input
import Html.Events
import Json.Decode as Decode
import Mensam.Api.Register
import Mensam.Element.Button
import Mensam.Element.Color
import Mensam.Error
import Mensam.User


type alias Model =
    { username : String
    , password : String
    , email : String
    , emailVisible : Bool
    , hint : String
    }


init : Model
init =
    { username = ""
    , password = ""
    , email = ""
    , emailVisible = False
    , hint = ""
    }


element : Model -> Element.Element Message
element model =
    Element.el
        [ Element.Background.color (Element.rgba 1 1 1 0.1)
        , Element.Font.color Mensam.Element.Color.bright.white
        , Element.Font.size 16
        , Element.centerX
        , Element.centerY
        , Element.width <| Element.px 300
        , Element.height <| Element.px 400
        ]
    <|
        Element.column
            [ Element.padding 20
            , Element.spacing 20
            , Element.width Element.fill
            ]
            [ Element.el
                [ Element.Font.size 30
                , Element.Font.hairline
                ]
              <|
                Element.text "Register"
            , Element.Input.username
                [ onEnter <| submitRegisterMessage model
                , Element.Font.color Mensam.Element.Color.dark.black
                ]
                { onChange = MessagePure << EnterUsername
                , text = model.username
                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Username"
                , label = Element.Input.labelAbove [] <| Element.text "Username"
                }
            , Element.Input.newPassword
                [ onEnter <| submitRegisterMessage model
                , Element.Font.color Mensam.Element.Color.dark.black
                ]
                { onChange = MessagePure << EnterPassword
                , text = model.password
                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Password"
                , label = Element.Input.labelAbove [] <| Element.text "Password"
                , show = False
                }
            , Element.Input.email
                [ onEnter <| submitRegisterMessage model
                , Element.Font.color Mensam.Element.Color.dark.black
                ]
                { onChange = MessagePure << EnterEmail
                , text = model.email
                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Email"
                , label = Element.Input.labelAbove [] <| Element.text "Email"
                }
            , Element.el
                [ Element.width Element.fill
                , Element.paddingEach
                    { top = 0
                    , right = 10
                    , bottom = 5
                    , left = 10
                    }
                , Element.alignBottom
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
                        , Element.width Element.fill
                        ]
                      <|
                        Element.text <|
                            model.hint
                    , Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = [ Element.width Element.fill ]
                            , color = Mensam.Element.Button.Yellow
                            , message = Just <| submitRegisterMessage model
                            , text = "Sign up"
                            }
                    ]
            ]


submitRegisterMessage : Model -> Message
submitRegisterMessage model =
    case Mensam.User.parsePassword model.password of
        Nothing ->
            MessagePure SetHintPasswordNotAccepted

        Just password ->
            MessageEffect <|
                Submit
                    { username = model.username
                    , password = password
                    , email = model.email
                    , emailVisible = model.emailVisible
                    }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect


type MessagePure
    = EnterUsername String
    | EnterPassword String
    | EnterEmail String
    | SetHintPasswordNotAccepted
    | SetHintUsernameIsTaken


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        EnterUsername username ->
            { model | username = username }

        EnterPassword password ->
            { model | password = password }

        EnterEmail email ->
            { model | email = email }

        SetHintPasswordNotAccepted ->
            { model | hint = "Password: " ++ Mensam.User.passwordRegexPattern }

        SetHintUsernameIsTaken ->
            { model | hint = "Username is already taken." }


type MessageEffect
    = ReportError Mensam.Error.Error
    | Submit
        { username : String
        , password : Mensam.User.Password
        , email : String
        , emailVisible : Bool
        }
    | Submitted { emailSent : Bool }


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


register :
    { username : String
    , password : Mensam.User.Password
    , email : String
    , emailVisible : Bool
    }
    -> Cmd Message
register args =
    Mensam.Api.Register.request
        { email = args.email
        , emailVisible = args.emailVisible
        , name = args.username
        , password = args.password
        }
    <|
        \result ->
            case result of
                Ok (Mensam.Api.Register.Success value) ->
                    MessageEffect <| Submitted value

                Ok Mensam.Api.Register.ErrorUsernameIsTaken ->
                    MessagePure SetHintUsernameIsTaken

                Ok (Mensam.Api.Register.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Registration failed" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Registration failed" <|
                                Mensam.Error.http error
