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
    , hint : List String
    }


init : Model
init =
    { username = ""
    , password = ""
    , email = ""
    , emailVisible = False
    , hint = []
    }


element : Model -> Element.Element Message
element model =
    Element.el
        [ Element.Background.color (Element.rgba 1 1 1 0.1)
        , Element.Font.color <| Mensam.Element.Color.bright.white Mensam.Element.Color.Opaque100
        , Element.Font.size 16
        , Element.centerX
        , Element.centerY
        , Element.width <| Element.px 300
        , Element.height <| Element.px 440
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
                , Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                ]
                { onChange = MessagePure << EnterUsername
                , text = model.username
                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Username"
                , label = Element.Input.labelAbove [] <| Element.text "Username"
                }
            , Element.Input.newPassword
                [ onEnter <| submitRegisterMessage model
                , Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                ]
                { onChange = MessagePure << EnterPassword
                , text = model.password
                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Password"
                , label = Element.Input.labelAbove [] <| Element.text "Password"
                , show = False
                }
            , Element.Input.email
                [ onEnter <| submitRegisterMessage model
                , Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                ]
                { onChange = MessagePure << EnterEmail
                , text = model.email
                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Email"
                , label = Element.Input.labelAbove [] <| Element.text "Email"
                }
            , Element.column
                [ Element.width Element.fill
                , Element.spacing 3
                , Element.alignBottom
                , Element.paddingXY 10 0
                ]
                [ Element.column
                    [ Element.height <| Element.px 50
                    , Element.spacing 2
                    , Element.paddingXY 5 0
                    , Element.Font.size 14
                    , Element.Font.color <| Mensam.Element.Color.bright.red Mensam.Element.Color.Opaque100
                    , Element.width Element.fill
                    , Element.alignBottom
                    ]
                  <|
                    List.map (\line -> Element.text <| line ++ "\n") model.hint
                , Mensam.Element.Button.button <|
                    Mensam.Element.Button.MkButton
                        { attributes = [ Element.width Element.fill, Element.alignBottom ]
                        , color = Mensam.Element.Button.Yellow
                        , label = Element.text "Sign up"
                        , message = Just <| submitRegisterMessage model
                        , size = Mensam.Element.Button.Medium
                        }
                ]
            ]


submitRegisterMessage : Model -> Message
submitRegisterMessage model =
    case Mensam.User.nameParse model.username of
        Err err ->
            MessagePure <| SetHintNameNotAccepted err

        Ok username ->
            case Mensam.User.passwordParse model.password of
                Err err ->
                    MessagePure <| SetHintPasswordNotAccepted err

                Ok password ->
                    case Mensam.User.emailParse model.email of
                        Nothing ->
                            MessagePure SetHintEmailNotParsed

                        Just email ->
                            MessageEffect <|
                                Submit
                                    { username = username
                                    , password = password
                                    , email = email
                                    , emailVisible = model.emailVisible
                                    }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect


type MessagePure
    = EnterUsername String
    | EnterPassword String
    | EnterEmail String
    | SetHintNameNotAccepted Mensam.User.ErrorNameParse
    | SetHintPasswordNotAccepted Mensam.User.ErrorPasswordParse
    | SetHintEmailNotParsed
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

        SetHintNameNotAccepted err ->
            { model
                | hint =
                    case err of
                        Mensam.User.MkErrorNameParseTooShort ->
                            [ "Username too short."
                            , "Needs atleast 4 characters."
                            ]

                        Mensam.User.MkErrorNameParseTooLong ->
                            [ "Username too short."
                            , "Takes atmost 32 characters."
                            ]

                        Mensam.User.MkErrorNameParseInvalidCharacter ->
                            [ "Username uses invalid character."
                            , "Only some symbols are allowed."
                            , String.fromList Mensam.User.passwordValidSymbols
                            ]
            }

        SetHintPasswordNotAccepted err ->
            { model
                | hint =
                    case err of
                        Mensam.User.MkErrorPasswordParseTooShort ->
                            [ "Password too short."
                            , "Needs atleast 4 characters."
                            ]

                        Mensam.User.MkErrorPasswordParseTooLong ->
                            [ "Password too short."
                            , "Takes atmost 32 characters."
                            ]

                        Mensam.User.MkErrorPasswordParseInvalidCharacter ->
                            [ "Password uses invalid character."
                            , "Only some symbols are allowed."
                            , String.fromList Mensam.User.passwordValidSymbols
                            ]
            }

        SetHintEmailNotParsed ->
            { model
                | hint =
                    [ "Cannot recognize email address."
                    , "Use a valid email address."
                    ]
            }

        SetHintUsernameIsTaken ->
            { model
                | hint =
                    [ "Username is already taken."
                    , "Choose a different username."
                    ]
            }


type MessageEffect
    = ReportError Mensam.Error.Error
    | Submit
        { username : Mensam.User.Name
        , password : Mensam.User.Password
        , email : Mensam.User.Email
        , emailVisible : Bool
        }
    | Submitted { username : Mensam.User.Name, password : Mensam.User.Password, emailSent : Bool }


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
    { username : Mensam.User.Name
    , password : Mensam.User.Password
    , email : Mensam.User.Email
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
                    MessageEffect <|
                        Submitted
                            { username = args.username
                            , password = args.password
                            , emailSent = value.emailSent
                            }

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
