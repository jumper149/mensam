module Mensam.Screen.UserSettings exposing (..)

import Element
import Element.Font
import Element.Input
import Html.Events
import Json.Decode as Decode
import Mensam.Api.ConfirmationRequest
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
    , email : Maybe Mensam.User.Email
    , emailVerified : Bool
    , popup : Maybe PopupModel
    }


type PopupModel
    = PopupChangePassword
        { newPassword : String
        , hint : List String
        }


init : { id : Mensam.User.Identifier } -> Model
init value =
    { id = value.id
    , name = Mensam.User.MkNameUnsafe ""
    , email = Nothing
    , emailVerified = True
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
                                , label = Element.text "Change Password"
                                , message = Just <| MessagePure OpenDialogToChangePassword
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
                                        Mensam.User.emailToString email
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
                            Element.text "Email Address verified?"
                        , if model.emailVerified then
                            Element.el
                                [ Element.alignLeft
                                , Element.centerY
                                ]
                            <|
                                Element.text "Yes"

                          else
                            Mensam.Element.Button.button <|
                                Mensam.Element.Button.MkButton
                                    { attributes = [ Element.alignRight, Element.centerY ]
                                    , color = Mensam.Element.Button.Yellow
                                    , label = Element.text "Confirm Email Address"
                                    , message = Just <| MessageEffect SubmitConfirmationRequest
                                    }
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
                            , Element.column
                                [ Element.height <| Element.px 50
                                , Element.spacing 2
                                , Element.paddingXY 5 0
                                , Element.Font.size 14
                                , Element.Font.color Mensam.Element.Color.bright.red
                                , Element.width Element.fill
                                , Element.alignBottom
                                ]
                              <|
                                List.map (\line -> Element.text <| line ++ "\n") popupModel.hint
                            , Element.row
                                [ Element.width Element.fill
                                , Element.spacing 10
                                , Element.alignBottom
                                ]
                                [ Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = []
                                        , color = Mensam.Element.Button.Yellow
                                        , label = Element.text "Go back"
                                        , message = Just <| MessagePure <| ClosePopup
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Blue
                                        , label = Element.text "Submit new Password"
                                        , message = Just <| submitNewPasswordMessage popupModel
                                        }
                                ]
                            ]
        , closePopup = MessagePure ClosePopup
        }


submitNewPasswordMessage :
    { newPassword : String
    , hint : List String
    }
    -> Message
submitNewPasswordMessage popupModel =
    case Mensam.User.passwordParse popupModel.newPassword of
        Err err ->
            MessagePure <| SetPasswordHint err

        Ok password ->
            MessageEffect <| SubmitNewPassword { newPassword = password }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect
    | Messages (List Message)


type MessagePure
    = SetName Mensam.User.Name
    | SetEmail (Maybe Mensam.User.Email)
    | SetEmailVerified Bool
    | OpenDialogToChangePassword
    | EnterNewPassword String
    | SetPasswordHint Mensam.User.ErrorPasswordParse
    | ClosePopup


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetName name ->
            { model | name = name }

        SetEmail email ->
            { model | email = email }

        SetEmailVerified verified ->
            { model | emailVerified = verified }

        OpenDialogToChangePassword ->
            { model | popup = Just <| PopupChangePassword { newPassword = "", hint = [] } }

        EnterNewPassword newPassword ->
            { model
                | popup =
                    case model.popup of
                        Nothing ->
                            Nothing

                        Just (PopupChangePassword popupModel) ->
                            Just <| PopupChangePassword { popupModel | newPassword = newPassword }
            }

        SetPasswordHint err ->
            { model
                | popup =
                    case model.popup of
                        Nothing ->
                            Nothing

                        Just (PopupChangePassword popupModel) ->
                            Just <|
                                PopupChangePassword
                                    { popupModel
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
            }

        ClosePopup ->
            { model | popup = Nothing }


type MessageEffect
    = ReportError Mensam.Error.Error
    | Refresh
    | SubmitNewPassword { newPassword : Mensam.User.Password }
    | SubmitConfirmationRequest


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
                        , MessagePure <| SetEmailVerified body.emailVerified
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


confirmationRequest : { jwt : Mensam.Auth.Bearer.Jwt } -> Cmd Message
confirmationRequest args =
    Mensam.Api.ConfirmationRequest.request
        { jwt = args.jwt
        }
    <|
        \response ->
            case response of
                Ok Mensam.Api.ConfirmationRequest.Success ->
                    Messages
                        [ MessagePure ClosePopup ]

                Ok (Mensam.Api.ConfirmationRequest.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to perform confirmation request" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to perform confirmation request" <|
                                Mensam.Error.http error
