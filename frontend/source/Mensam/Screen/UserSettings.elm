module Mensam.Screen.UserSettings exposing (..)

import Element
import Element.Border
import Element.Font
import Element.Input
import File
import File.Select
import Html.Events
import Json.Decode as Decode
import Mensam.Api.ConfirmationRequest
import Mensam.Api.NotificationPreferences
import Mensam.Api.PasswordChange
import Mensam.Api.PictureDelete
import Mensam.Api.PictureDownload
import Mensam.Api.PictureUpload
import Mensam.Api.Profile
import Mensam.Auth.Bearer
import Mensam.Element.Button
import Mensam.Element.Color
import Mensam.Element.Screen
import Mensam.Error
import Mensam.User
import Url.Builder


type alias Model =
    { id : Mensam.User.Identifier
    , name : Mensam.User.Name
    , profilePictureUrl : String
    , email : Maybe Mensam.User.Email
    , emailVerified : Bool
    , notificationPreferences :
        { receiveEmailNotifications : Bool }
    , popup : Maybe PopupModel
    }


type PopupModel
    = PopupChangePassword
        { newPassword : String
        , hint : List String
        }
    | PopupNotificationPreferences
        { receiveEmailNotifications : Bool
        }
    | PopupDeleteProfilePicture


init : { id : Mensam.User.Identifier } -> Model
init value =
    { id = value.id
    , name = Mensam.User.MkNameUnsafe ""
    , profilePictureUrl =
        Url.Builder.absolute
            [ "static"
            , "default-profile-picture.jpeg"
            ]
            []
    , email = Nothing
    , emailVerified = True
    , notificationPreferences = { receiveEmailNotifications = False }
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
                        , Mensam.Element.Button.button <|
                            Mensam.Element.Button.MkButton
                                { attributes = [ Element.alignRight ]
                                , color = Mensam.Element.Button.Gray
                                , enabled = True
                                , label = Element.text "Public Profile"
                                , message = Just <| MessageEffect <| OpenPageUserProfile { id = model.id }
                                , size = Mensam.Element.Button.Small
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
                        , Element.height <| Element.px 220
                        , Element.padding 10
                        , Element.spacing 30
                        ]
                        [ Element.el
                            [ Element.alignLeft
                            , Element.centerY
                            , Element.padding 10
                            ]
                          <|
                            Element.image
                                [ Element.width <| Element.px 180
                                , Element.height <| Element.px 180
                                , Element.Border.rounded 30
                                , Element.clip
                                ]
                                { src = model.profilePictureUrl
                                , description = "Profile picture."
                                }
                        , Element.column
                            [ Element.padding 10
                            , Element.spacing 30
                            , Element.alignRight
                            , Element.centerY
                            ]
                            [ Mensam.Element.Button.button <|
                                Mensam.Element.Button.MkButton
                                    { attributes = []
                                    , color = Mensam.Element.Button.Yellow
                                    , enabled = True
                                    , label = Element.text "Upload Pic"
                                    , message = Just <| MessageEffect UploadProfilePictureRequested
                                    , size = Mensam.Element.Button.Medium
                                    }
                            , Mensam.Element.Button.button <|
                                Mensam.Element.Button.MkButton
                                    { attributes = []
                                    , color = Mensam.Element.Button.Red
                                    , enabled = True
                                    , label = Element.text "Delete Pic"
                                    , message = Just <| MessagePure OpenDialogToDeleteProfilePicture
                                    , size = Mensam.Element.Button.Medium
                                    }
                            ]
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
                                , enabled = True
                                , label = Element.text "Change Password"
                                , message = Just <| MessagePure OpenDialogToChangePassword
                                , size = Mensam.Element.Button.Medium
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
                                    , enabled = True
                                    , label = Element.text "Request Email"
                                    , message = Just <| MessageEffect SubmitConfirmationRequest
                                    , size = Mensam.Element.Button.Medium
                                    }
                        ]
                    , Element.column
                        [ Element.width Element.fill
                        , Element.height <| Element.px 90
                        , Element.padding 10
                        , Element.spacing 10
                        ]
                        [ Element.row
                            [ Element.width Element.fill
                            , Element.height <| Element.px 30
                            , Element.spacing 30
                            , Element.alignTop
                            ]
                            [ Element.el
                                [ Element.alignLeft
                                , Element.centerY
                                ]
                              <|
                                Element.text "Receive Email notifications?"
                            , if model.notificationPreferences.receiveEmailNotifications then
                                Element.el
                                    [ Element.alignLeft
                                    , Element.centerY
                                    ]
                                <|
                                    Element.text "Yes"

                              else
                                Element.el
                                    [ Element.alignLeft
                                    , Element.centerY
                                    ]
                                <|
                                    Element.text "No"
                            ]
                        , Mensam.Element.Button.button <|
                            Mensam.Element.Button.MkButton
                                { attributes = [ Element.alignBottom, Element.alignRight, Element.centerY ]
                                , color = Mensam.Element.Button.Yellow
                                , enabled = True
                                , label = Element.text "Notification Preferences"
                                , message = Just <| MessagePure OpenDialogToSetNotificationPreferences
                                , size = Mensam.Element.Button.Medium
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
                                , Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
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
                                , Element.Font.color <| Mensam.Element.Color.bright.red Mensam.Element.Color.Opaque100
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
                                        , enabled = True
                                        , label = Element.text "Go back"
                                        , message = Just <| MessagePure <| ClosePopup
                                        , size = Mensam.Element.Button.Medium
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Blue
                                        , enabled = True
                                        , label = Element.text "Submit new Password"
                                        , message = Just <| submitNewPasswordMessage popupModel
                                        , size = Mensam.Element.Button.Medium
                                        }
                                ]
                            ]

                Just (PopupNotificationPreferences popupModel) ->
                    Just <|
                        Element.column
                            [ Element.spacing 20
                            , Element.width Element.fill
                            , Element.height Element.fill
                            ]
                            [ Element.el
                                [ Element.Font.size 26
                                , Element.Font.hairline
                                ]
                              <|
                                Element.text "Notification Preferences"
                            , Element.Input.checkbox
                                []
                                { onChange = MessagePure << SetNotificationPreferencesInDialog
                                , icon = Element.Input.defaultCheckbox
                                , checked = popupModel.receiveEmailNotifications
                                , label = Element.Input.labelRight [] <| Element.text "Receive email notifications"
                                }
                            , if model.emailVerified then
                                Element.none

                              else
                                Element.paragraph
                                    [ Element.alignBottom ]
                                    [ Element.text "You have to verify your email address before you can change your notification settings."
                                    ]
                            , Element.row
                                [ Element.width Element.fill
                                , Element.spacing 10
                                , Element.alignBottom
                                ]
                                [ Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = []
                                        , color = Mensam.Element.Button.Yellow
                                        , enabled = True
                                        , label = Element.text "Go back"
                                        , message = Just <| MessagePure <| ClosePopup
                                        , size = Mensam.Element.Button.Medium
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Blue
                                        , enabled = model.emailVerified
                                        , label = Element.text "Submit Preferences"
                                        , message = Just <| MessageEffect <| SubmitNotificationPreferences popupModel
                                        , size = Mensam.Element.Button.Medium
                                        }
                                ]
                            ]

                Just PopupDeleteProfilePicture ->
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
                                Element.text "Delete Profile Picture"
                            , Element.paragraph
                                []
                                [ Element.text "Are you sure you want to delete your profile picture?"
                                ]
                            , Element.row
                                [ Element.width Element.fill
                                , Element.spacing 10
                                , Element.alignBottom
                                ]
                                [ Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = []
                                        , color = Mensam.Element.Button.Yellow
                                        , enabled = True
                                        , label = Element.text "Go back"
                                        , message = Just <| MessagePure <| ClosePopup
                                        , size = Mensam.Element.Button.Medium
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Red
                                        , enabled = True
                                        , label = Element.text "Delete Profile Picture"
                                        , message = Just <| MessageEffect DeleteProfilePictureRequest
                                        , size = Mensam.Element.Button.Medium
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
    | OpenDialogToDeleteProfilePicture
    | SetProfilePicture { url : String }
    | SetEmail (Maybe Mensam.User.Email)
    | SetEmailVerified Bool
    | SetNotificationPreferences { receiveEmailNotifications : Bool }
    | OpenDialogToChangePassword
    | EnterNewPassword String
    | SetPasswordHint Mensam.User.ErrorPasswordParse
    | OpenDialogToSetNotificationPreferences
    | SetNotificationPreferencesInDialog Bool
    | ClosePopup


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetName name ->
            { model | name = name }

        OpenDialogToDeleteProfilePicture ->
            { model | popup = Just PopupDeleteProfilePicture }

        SetProfilePicture picture ->
            { model | profilePictureUrl = picture.url }

        SetEmail email ->
            { model | email = email }

        SetEmailVerified verified ->
            { model | emailVerified = verified }

        SetNotificationPreferences notificationPreferences ->
            { model | notificationPreferences = notificationPreferences }

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

                        Just (PopupNotificationPreferences _) ->
                            Nothing

                        Just PopupDeleteProfilePicture ->
                            Nothing
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

                        Just (PopupNotificationPreferences _) ->
                            Nothing

                        Just PopupDeleteProfilePicture ->
                            Nothing
            }

        OpenDialogToSetNotificationPreferences ->
            { model | popup = Just <| PopupNotificationPreferences { receiveEmailNotifications = model.notificationPreferences.receiveEmailNotifications } }

        SetNotificationPreferencesInDialog receiveEmailNotifications ->
            { model
                | popup =
                    case model.popup of
                        Nothing ->
                            Nothing

                        Just (PopupChangePassword _) ->
                            Nothing

                        Just (PopupNotificationPreferences popupModel) ->
                            Just <| PopupNotificationPreferences { popupModel | receiveEmailNotifications = receiveEmailNotifications }

                        Just PopupDeleteProfilePicture ->
                            Nothing
            }

        ClosePopup ->
            { model | popup = Nothing }


type MessageEffect
    = ReportError Mensam.Error.Error
    | Refresh
    | SubmitNewPassword { newPassword : Mensam.User.Password }
    | SubmitConfirmationRequest
    | RefreshNotificationPreferences
    | SubmitNotificationPreferences { receiveEmailNotifications : Bool }
    | UploadProfilePictureRequested
    | UploadProfilePictureUpload File.File
    | DeleteProfilePictureRequest
    | DownloadProfilePictureRequest
    | OpenPageUserProfile { id : Mensam.User.Identifier }


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
                        , MessageEffect <| RefreshNotificationPreferences
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


setNotificationPreferences : { jwt : Mensam.Auth.Bearer.Jwt, receiveEmailNotifications : Maybe Bool } -> Cmd Message
setNotificationPreferences args =
    Mensam.Api.NotificationPreferences.request
        { jwt = args.jwt
        , receiveEmailNotifications = args.receiveEmailNotifications
        }
    <|
        \response ->
            case response of
                Ok (Mensam.Api.NotificationPreferences.Success notificationPreferences) ->
                    Messages
                        [ MessagePure ClosePopup
                        , MessagePure <| SetNotificationPreferences notificationPreferences
                        ]

                Ok Mensam.Api.NotificationPreferences.ErrorEmailNotVerified ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to set notification preferences" <|
                                Mensam.Error.message "Your email is not verified" <|
                                    Mensam.Error.message "Cannot set unless your email is verified" <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.NotificationPreferences.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to set notification preferences" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.NotificationPreferences.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to set notification preferences" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to change password" <|
                                Mensam.Error.http error


selectProfilePictureToUpload : Cmd Message
selectProfilePictureToUpload =
    File.Select.file [ "image/jpeg" ] <| \file -> MessageEffect <| UploadProfilePictureUpload file


downloadProfilePicture : Mensam.Auth.Bearer.Jwt -> Mensam.User.Identifier -> Cmd Message
downloadProfilePicture jwt user =
    Mensam.Api.PictureDownload.request
        { jwt = jwt
        , user = user
        }
    <|
        \response ->
            case response of
                Ok (Mensam.Api.PictureDownload.Success picture) ->
                    MessagePure <| SetProfilePicture picture

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to download profile picture" <|
                                Mensam.Error.http error


uploadProfilePicture : Mensam.Auth.Bearer.Jwt -> File.File -> Cmd Message
uploadProfilePicture jwt file =
    Mensam.Api.PictureUpload.request
        { jwt = jwt
        , picture = file
        }
    <|
        \response ->
            case response of
                Ok Mensam.Api.PictureUpload.Success ->
                    MessageEffect DownloadProfilePictureRequest

                Ok (Mensam.Api.PictureUpload.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to upload profile picture" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message "Make sure to use JPEG" <|
                                        Mensam.Error.message error <|
                                            Mensam.Error.undefined

                Ok (Mensam.Api.PictureUpload.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to upload profile picture" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to upload profile picture" <|
                                Mensam.Error.message "Try reducing the file size before uploading" <|
                                    Mensam.Error.http error


deleteProfilePicture : Mensam.Auth.Bearer.Jwt -> Cmd Message
deleteProfilePicture jwt =
    Mensam.Api.PictureDelete.request
        { jwt = jwt
        }
    <|
        \response ->
            case response of
                Ok Mensam.Api.PictureDelete.Success ->
                    Messages
                        [ MessagePure ClosePopup
                        , MessageEffect DownloadProfilePictureRequest
                        ]

                Ok (Mensam.Api.PictureDelete.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to upload profile picture" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to upload profile picture" <|
                                Mensam.Error.message "Try reducing the file size before uploading" <|
                                    Mensam.Error.http error
