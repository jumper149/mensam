module Mensam.Screen.Register exposing (..)

import Element
import Element.Background
import Element.Font
import Element.Input
import Html.Attributes
import Html.Events
import Json.Decode
import Mensam.Api.Register
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Error


type alias Model =
    { username : String
    , password : String
    , email : String

    -- TODO: Add checkbox.
    , emailVisible : Bool
    , hint : String
    }


init : Model
init =
    { username = ""
    , password = ""
    , email = ""
    , emailVisible = True
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
                Element.text "Register"
            , Element.Input.username
                [ onEnter <| MessageEffect Submit
                , Element.Font.color Mensam.Element.Color.dark.black
                ]
                { onChange = MessagePure << EnterUsername
                , text = model.username
                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Username"
                , label = Element.Input.labelAbove [] <| Element.text "Username"
                }
            , Element.Input.newPassword
                [ onEnter <| MessageEffect Submit
                , Element.Font.color Mensam.Element.Color.dark.black
                ]
                { onChange = MessagePure << EnterPassword
                , text = model.password
                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Password"
                , label = Element.Input.labelAbove [] <| Element.text "Password"
                , show = False
                }
            , Element.Input.email
                [ onEnter <| MessageEffect Submit
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
                        { onPress = Just <| MessageEffect <| Submit
                        , label =
                            Element.el
                                [ Element.centerX
                                , Element.centerY
                                , Element.Font.family [ Mensam.Element.Font.condensed ]
                                , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                ]
                            <|
                                Element.text "Sign up"
                        }
                    ]
            ]


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect


type MessagePure
    = EnterUsername String
    | EnterPassword String
    | EnterEmail String


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        EnterUsername username ->
            { model | username = username }

        EnterPassword password ->
            { model | password = password }

        EnterEmail email ->
            { model | email = email }


type MessageEffect
    = ReportError Mensam.Error.Error
    | Submit
    | Submitted { emailSent : Bool }


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.Decode.succeed msg

                        else
                            Json.Decode.fail "Not the enter key"
                    )
            )
        )


register : Model -> Cmd Message
register model =
    Mensam.Api.Register.request { email = model.email, emailVisible = model.emailVisible, name = model.username, password = model.password } <|
        \result ->
            case result of
                Ok (Mensam.Api.Register.Success value) ->
                    MessageEffect <| Submitted value

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
