module Mensam.Screen.UserSettings exposing (..)

import Element
import Element.Font
import Element.Input
import Html.Events
import Json.Decode as Decode
import Mensam.Api.PasswordChange
import Mensam.Api.Profile
import Mensam.Auth.Bearer
import Mensam.Element.Button
import Mensam.Element.Color
import Mensam.Element.Screen
import Mensam.Error
import Mensam.User


type alias Model =
    { id : Mensam.User.Identifier
    , name : Mensam.User.Name
    , email : Maybe String
    , popup : Maybe PopupModel
    }


type PopupModel
    = PopupChangePassword
        { newPassword : String
        , hint : String
        }


init : { id : Mensam.User.Identifier } -> Model
init value =
    { id = value.id
    , name = Mensam.User.MkName ""
    , email = Nothing
    , popup = Nothing
    }


element : Model -> Element.Element Message
element model =
    Mensam.Element.Screen.element
        { main =
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing 10
                ]
                [ Element.column
                    [ Element.width Element.fill
                    , Element.height <| Element.fillPortion 1
                    ]
                    [ Element.row
                        [ Element.width Element.fill
                        , Element.height <| Element.px 45
                        , Element.padding 10
                        , Element.spacing 30
                        ]
                        [ Element.el
                            [ Element.Font.size 22
                            , Element.Font.hairline
                            , Element.alignBottom
                            , Element.alignLeft
                            ]
                          <|
                            Element.text "User Settings"
                        ]
                    , Element.row
                        [ Element.width Element.fill
                        , Element.height <| Element.px 60
                        , Element.padding 10
                        , Element.spacing 20
                        ]
                        [ Element.el
                            [ Element.alignLeft
                            , Element.centerY
                            ]
                          <|
                            Element.text "Identifier:"
                        , Element.el [] <| Element.text <| Mensam.User.identifierToString model.id
                        ]
                    , Element.row
                        [ Element.width Element.fill
                        , Element.height <| Element.px 60
                        , Element.padding 10
                        , Element.spacing 20
                        ]
                        [ Element.el
                            [ Element.alignLeft
                            , Element.centerY
                            ]
                          <|
                            Element.text "Username:"
                        , Element.el [] <| Element.text <| Mensam.User.nameToString model.name
                        ]
                    , Element.row
                        [ Element.width Element.fill
                        , Element.height <| Element.px 60
                        , Element.padding 10
                        , Element.spacing 30
                        ]
                        [ Element.el
                            [ Element.alignLeft
                            , Element.centerY
                            ]
                          <|
                            Element.text "Password"
                        , Mensam.Element.Button.button <|
                            Mensam.Element.Button.MkButton
                                { attributes = [ Element.alignRight, Element.centerY ]
                                , color = Mensam.Element.Button.Yellow
                                , message = Just <| MessagePure OpenDialogToChangePassword
                                , text = "Change Password"
                                }
                        ]
                    , Element.row
                        [ Element.width Element.fill
                        , Element.height <| Element.px 60
                        , Element.padding 10
                        , Element.spacing 20
                        ]
                        [ Element.el
                            [ Element.alignLeft
                            , Element.centerY
                            ]
                          <|
                            Element.text "Email:"
                        , Element.el [] <|
                            Element.text <|
                                case model.email of
                                    Nothing ->
                                        "hidden"

                                    Just email ->
                                        email
                        ]
                    ]
                ]
        , popup =
            case model.popup of
                Nothing ->
                    Nothing

                Just (PopupChangePassword popupModel) ->
                    Just <|
                        Element.column
                            [ Element.spacing 20
                            , Element.width Element.fill
                            , Element.height Element.fill
                            ]
                            [ Element.el
                                [ Element.Font.size 30
                                , Element.Font.hairline
                                ]
                              <|
                                Element.text "Change Password"
                            , Element.Input.newPassword
                                [ onEnter <| submitNewPasswordMessage popupModel
                                , Element.Font.color Mensam.Element.Color.dark.black
                                ]
                                { onChange = MessagePure << EnterNewPassword
                                , text = popupModel.newPassword
                                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "New Password"
                                , label = Element.Input.labelHidden "New Password"
                                , show = False
                                }
                            , Element.el
                                [ Element.height <| Element.px 14
                                , Element.paddingXY 5 0
                                , Element.Font.size 14
                                , Element.Font.color Mensam.Element.Color.bright.red
                                , Element.alignBottom
                                ]
                              <|
                                Element.text <|
                                    popupModel.hint
                            , Element.row
                                [ Element.width Element.fill
                                , Element.spacing 10
                                , Element.alignBottom
                                ]
                                [ Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = []
                                        , color = Mensam.Element.Button.Yellow
                                        , message = Just <| MessagePure <| ClosePopup
                                        , text = "Go back"
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Blue
                                        , message = Just <| submitNewPasswordMessage popupModel
                                        , text = "Submit new Password"
                                        }
                                ]
                            ]
        , closePopup = MessagePure ClosePopup
        }


submitNewPasswordMessage :
    { newPassword : String
    , hint : String
    }
    -> Message
submitNewPasswordMessage popupModel =
    case Mensam.User.parsePassword popupModel.newPassword of
        Nothing ->
            MessagePure <| SetPasswordHint <| "Password: " ++ Mensam.User.passwordRegexPattern

        Just password ->
            MessageEffect <| SubmitNewPassword { newPassword = password }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect
    | Messages (List Message)


type MessagePure
    = SetName Mensam.User.Name
    | SetEmail (Maybe String)
    | OpenDialogToChangePassword
    | EnterNewPassword String
    | SetPasswordHint String
    | ClosePopup


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetName name ->
            { model | name = name }

        SetEmail email ->
            { model | email = email }

        OpenDialogToChangePassword ->
            { model | popup = Just <| PopupChangePassword { newPassword = "", hint = "" } }

        EnterNewPassword newPassword ->
            { model
                | popup =
                    case model.popup of
                        Nothing ->
                            Nothing

                        Just (PopupChangePassword popupModel) ->
                            Just <| PopupChangePassword { popupModel | newPassword = newPassword }
            }

        SetPasswordHint hint ->
            { model
                | popup =
                    case model.popup of
                        Nothing ->
                            Nothing

                        Just (PopupChangePassword popupModel) ->
                            Just <| PopupChangePassword { popupModel | hint = hint }
            }

        ClosePopup ->
            { model | popup = Nothing }


type MessageEffect
    = ReportError Mensam.Error.Error
    | Refresh
    | SubmitNewPassword { newPassword : Mensam.User.Password }


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


profile : Mensam.Auth.Bearer.Jwt -> Mensam.User.Identifier -> Cmd Message
profile jwt userId =
    Mensam.Api.Profile.request
        { jwt = jwt
        , id = userId
        }
    <|
        \response ->
            case response of
                Ok (Mensam.Api.Profile.Success body) ->
                    Messages
                        [ MessagePure <| SetName body.name
                        , MessagePure <| SetEmail body.email
                        ]

                Ok Mensam.Api.Profile.ErrorUnknownUser ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Unknown user while requesting information" <|
                                Mensam.Error.undefined

                Ok (Mensam.Api.Profile.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to request profile" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.Profile.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to request profile" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to request profile" <|
                                Mensam.Error.http error


changePassword : { jwt : Mensam.Auth.Bearer.Jwt, newPassword : Mensam.User.Password } -> Cmd Message
changePassword args =
    Mensam.Api.PasswordChange.request
        { jwt = args.jwt
        , newPassword = args.newPassword
        }
    <|
        \response ->
            case response of
                Ok Mensam.Api.PasswordChange.Success ->
                    Messages
                        [ MessagePure ClosePopup ]

                Ok (Mensam.Api.PasswordChange.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to change password" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.PasswordChange.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to change password" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to change password" <|
                                Mensam.Error.http error
