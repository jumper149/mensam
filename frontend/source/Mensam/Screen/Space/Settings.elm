module Mensam.Screen.Space.Settings exposing (..)

import Element
import Element.Background
import Element.Border
import Element.Events.Pointer
import Element.Font
import Element.Input
import File
import File.Select
import Html.Attributes
import Mensam.Api.SpaceDelete
import Mensam.Api.SpaceEdit
import Mensam.Api.SpacePictureDelete
import Mensam.Api.SpacePictureDownload
import Mensam.Api.SpacePictureUpload
import Mensam.Auth.Bearer
import Mensam.Element.Button
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Element.Screen
import Mensam.Error
import Mensam.Http.Tracker
import Mensam.Space
import Mensam.Space.Role
import Mensam.Time
import Mensam.Url


type alias Model =
    { id : Mensam.Space.Identifier
    , old :
        { name : Mensam.Space.Name
        , timezone : Mensam.Time.Timezone
        , discoverability : Mensam.Space.Discoverability
        }
    , new :
        { name : Maybe Mensam.Space.Name
        , timezone :
            Maybe
                { selected : Mensam.Time.Timezone
                , hovering : Maybe Int
                }
        , discoverability : Maybe Mensam.Space.Discoverability
        }
    , spacePictureUrl : String
    , popup : Maybe PopupModel
    }


type PopupModel
    = PopupDeleteSpace
    | PopupDeleteSpacePicture


init : Mensam.Url.BaseUrl -> { id : Mensam.Space.Identifier } -> Model
init baseUrl args =
    { id = args.id
    , old =
        { name = Mensam.Space.MkName ""
        , timezone = Mensam.Time.timezoneEtcUtc
        , discoverability = Mensam.Space.MkDiscoverabilityPublic
        }
    , new =
        { name = Nothing
        , timezone = Nothing
        , discoverability = Nothing
        }
    , spacePictureUrl =
        Mensam.Url.absolute baseUrl
            [ "static"
            , "default-space-picture.jpeg"
            ]
            []
    , popup = Nothing
    }


element : Model -> Element.Element Message
element model =
    Mensam.Element.Screen.element
        { main =
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                ]
                [ Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.clipY
                    , Element.scrollbarY
                    , Element.htmlAttribute <| Html.Attributes.style "contain" "size"
                    ]
                    [ Element.row
                        [ Element.width Element.fill
                        , Element.height <| Element.px 70
                        , Element.padding 10
                        , Element.spacing 10
                        ]
                        [ Element.el
                            [ Element.Font.size 30
                            , Element.Font.hairline
                            , Element.alignLeft
                            , Element.centerY
                            ]
                          <|
                            Element.text "Settings"
                        , Mensam.Element.Button.button <|
                            Mensam.Element.Button.MkButton
                                { attributes = [ Element.alignLeft, Element.centerY ]
                                , color = Mensam.Element.Button.Red
                                , enabled = True
                                , label = Element.text "Delete"
                                , message = Just <| MessagePure OpenDialogToDeleteSpace
                                , size = Mensam.Element.Button.Medium
                                }
                        , Mensam.Element.Button.button <|
                            Mensam.Element.Button.MkButton
                                { attributes = [ Element.alignRight, Element.centerY ]
                                , color = Mensam.Element.Button.Yellow
                                , enabled = True
                                , label = Element.text "Roles"
                                , message = Just <| MessageEffect OpenPageToRoles
                                , size = Mensam.Element.Button.Medium
                                }
                        , Mensam.Element.Button.button <|
                            Mensam.Element.Button.MkButton
                                { attributes = [ Element.alignRight, Element.centerY ]
                                , color = Mensam.Element.Button.Gray
                                , enabled = True
                                , label = Element.text "Go back"
                                , message = Just <| MessageEffect ReturnToSpace
                                , size = Mensam.Element.Button.Medium
                                }
                        ]
                    , Element.column
                        [ Element.spacing 20
                        , Element.width Element.fill
                        , Element.height <| Element.px 140
                        ]
                        [ Element.row
                            [ Element.spacing 20
                            , Element.width Element.fill
                            , Element.height <| Element.px 25
                            ]
                            [ Element.el [] <| Element.text "Current Name:"
                            , Element.el [] <| Element.text <| Mensam.Space.nameToString model.old.name
                            ]
                        , Element.el
                            [ Element.paddingXY 30 5
                            , Element.width Element.fill
                            , Element.height <| Element.px 115
                            ]
                          <|
                            case model.new.name of
                                Nothing ->
                                    Mensam.Element.Button.button <|
                                        Mensam.Element.Button.MkButton
                                            { attributes = [ Element.width Element.fill ]
                                            , color = Mensam.Element.Button.Yellow
                                            , enabled = True
                                            , label = Element.text "Edit Name"
                                            , message = Just <| MessagePure <| EnterName <| Just <| model.old.name
                                            , size = Mensam.Element.Button.Medium
                                            }

                                Just name ->
                                    Element.Input.text
                                        [ Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                                        ]
                                        { onChange = MessagePure << EnterName << Just << Mensam.Space.MkName
                                        , text = Mensam.Space.nameToString name
                                        , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Name"
                                        , label = Element.Input.labelHidden "Name"
                                        }
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
                                { src = model.spacePictureUrl
                                , description = "Space picture."
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
                                    , message = Just <| MessageEffect UploadSpacePictureRequested
                                    , size = Mensam.Element.Button.Medium
                                    }
                            , Mensam.Element.Button.button <|
                                Mensam.Element.Button.MkButton
                                    { attributes = []
                                    , color = Mensam.Element.Button.Red
                                    , enabled = True
                                    , label = Element.text "Delete Pic"
                                    , message = Just <| MessagePure OpenDialogToDeleteSpacePicture
                                    , size = Mensam.Element.Button.Medium
                                    }
                            ]
                        ]
                    , Element.column
                        [ Element.spacing 20
                        , Element.width Element.fill
                        , Element.height <| Element.px 245
                        ]
                        [ Element.row
                            [ Element.spacing 20
                            , Element.width Element.fill
                            , Element.height <| Element.px 25
                            ]
                            [ Element.el [] <| Element.text "Current Timezone:"
                            , Element.el [] <| Element.text <| Mensam.Time.timezoneToString model.old.timezone
                            ]
                        , Element.el
                            [ Element.paddingXY 30 5
                            , Element.width Element.fill
                            , Element.height <| Element.px 200
                            ]
                          <|
                            case model.new.timezone of
                                Nothing ->
                                    Mensam.Element.Button.button <|
                                        Mensam.Element.Button.MkButton
                                            { attributes = [ Element.width Element.fill ]
                                            , color = Mensam.Element.Button.Yellow
                                            , enabled = True
                                            , label = Element.text "Edit Timezone"
                                            , message = Just <| MessagePure <| SetTimezone <| Just model.old.timezone
                                            , size = Mensam.Element.Button.Medium
                                            }

                                Just timezoneWidget ->
                                    Element.indexedTable
                                        [ Element.width Element.fill
                                        , Element.height <| Element.px 150
                                        , Element.Background.color (Element.rgba 0 0 0 0.1)
                                        , Element.Font.family [ Mensam.Element.Font.condensed ]
                                        , Element.Font.size 16
                                        , Element.clipY
                                        , Element.scrollbarY
                                        , Element.htmlAttribute <| Html.Attributes.style "contain" "size"
                                        ]
                                        { data = Mensam.Time.allTimezones
                                        , columns =
                                            let
                                                cell =
                                                    Element.el
                                                        [ Element.height <| Element.px 40
                                                        , Element.padding 10
                                                        ]
                                            in
                                            [ { header = Element.none
                                              , width = Element.fill
                                              , view =
                                                    \n timezone ->
                                                        Element.el
                                                            [ Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetHoveringTimezone Nothing
                                                            , Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetHoveringTimezone <| Just n
                                                            , Element.Events.Pointer.onClick <| \_ -> MessagePure <| SetTimezone <| Just timezone
                                                            , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                                            , let
                                                                alpha =
                                                                    case timezoneWidget.hovering of
                                                                        Nothing ->
                                                                            0.2

                                                                        Just m ->
                                                                            if m == n then
                                                                                0.4

                                                                            else
                                                                                0.2
                                                              in
                                                              if timezoneWidget.selected == timezone then
                                                                Element.Background.color (Element.rgba 0 0.2 0 alpha)

                                                              else
                                                                Element.Background.color (Element.rgba 0 0 0 alpha)
                                                            ]
                                                        <|
                                                            cell <|
                                                                Element.el
                                                                    [ Element.width <| Element.maximum 100 <| Element.fill ]
                                                                <|
                                                                    Element.text <|
                                                                        Mensam.Time.timezoneToString timezone
                                              }
                                            ]
                                        }
                        ]
                    , Element.column
                        [ Element.spacing 20
                        , Element.width Element.fill
                        , Element.height <| Element.px 140
                        ]
                        [ Element.row
                            [ Element.spacing 20
                            , Element.width Element.fill
                            , Element.height <| Element.px 25
                            ]
                            [ Element.el [] <| Element.text "Current Discoverability:"
                            , Element.el [] <| Element.text <| Mensam.Space.discoverabilityToString model.old.discoverability
                            ]
                        , Element.el
                            [ Element.paddingXY 30 5
                            , Element.width Element.fill
                            , Element.height <| Element.px 115
                            ]
                          <|
                            case model.new.discoverability of
                                Nothing ->
                                    Mensam.Element.Button.button <|
                                        Mensam.Element.Button.MkButton
                                            { attributes = [ Element.width Element.fill ]
                                            , color = Mensam.Element.Button.Yellow
                                            , enabled = True
                                            , label = Element.text "Edit Discoverability"
                                            , message = Just <| MessagePure <| SetDiscoverability <| Just <| model.old.discoverability
                                            , size = Mensam.Element.Button.Medium
                                            }

                                Just discoverability ->
                                    Element.row
                                        [ Element.spacing 20
                                        , Element.width Element.fill
                                        , Element.height <| Element.px 60
                                        ]
                                        -- TODO: Use `Mensam.Element.Button.button`.
                                        [ Element.Input.button
                                            [ if discoverability == Mensam.Space.MkDiscoverabilityPublic then
                                                Element.Background.color <| Mensam.Element.Color.bright.green Mensam.Element.Color.Opaque100

                                              else
                                                Element.Background.color <| Mensam.Element.Color.bright.white Mensam.Element.Color.Opaque100
                                            , Element.mouseOver [ Element.Background.color <| Mensam.Element.Color.bright.magenta Mensam.Element.Color.Opaque100 ]
                                            , Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                                            , Element.width Element.fill
                                            , Element.padding 10
                                            ]
                                            { onPress = Just <| MessagePure <| SetDiscoverability <| Just <| Mensam.Space.MkDiscoverabilityPublic
                                            , label =
                                                Element.el
                                                    [ Element.centerX
                                                    , Element.centerY
                                                    , Element.Font.family [ Mensam.Element.Font.condensed ]
                                                    ]
                                                <|
                                                    Element.text "public"
                                            }
                                        , Element.Input.button
                                            [ if discoverability == Mensam.Space.MkDiscoverabilityPrivate then
                                                Element.Background.color <| Mensam.Element.Color.bright.green Mensam.Element.Color.Opaque100

                                              else
                                                Element.Background.color <| Mensam.Element.Color.bright.white Mensam.Element.Color.Opaque100
                                            , Element.mouseOver [ Element.Background.color <| Mensam.Element.Color.bright.magenta Mensam.Element.Color.Opaque100 ]
                                            , Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                                            , Element.width Element.fill
                                            , Element.padding 10
                                            ]
                                            { onPress = Just <| MessagePure <| SetDiscoverability <| Just <| Mensam.Space.MkDiscoverabilityPrivate
                                            , label =
                                                Element.el
                                                    [ Element.centerX
                                                    , Element.centerY
                                                    , Element.Font.family [ Mensam.Element.Font.condensed ]
                                                    ]
                                                <|
                                                    Element.text "private"
                                            }
                                        ]
                        ]
                    ]
                , Element.row
                    [ Element.spacing 20
                    , Element.padding 20
                    , Element.width Element.fill
                    , Element.alignBottom
                    , Element.Background.color <| Mensam.Element.Color.bright.white Mensam.Element.Color.Opaque05
                    ]
                    [ Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = []
                            , color = Mensam.Element.Button.Gray
                            , enabled = True
                            , label = Element.text "Go back"
                            , message = Just <| MessageEffect ReturnToSpace
                            , size = Mensam.Element.Button.Medium
                            }
                    , Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = [ Element.width Element.fill ]
                            , color = Mensam.Element.Button.Blue
                            , enabled = True
                            , label = Element.text "Apply Settings"
                            , message = Just <| MessageEffect SubmitSettings
                            , size = Mensam.Element.Button.Medium
                            }
                    ]
                ]
        , popup =
            case model.popup of
                Nothing ->
                    Nothing

                Just PopupDeleteSpace ->
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
                                Element.text "Delete Space"
                            , Element.paragraph
                                []
                                [ Element.text "Are you sure you want to delete this space?"
                                ]
                            , Element.paragraph
                                []
                                [ Element.text "Deleting a space will permanently delete all of its data and is not recoverable. This includes desks, roles, memberships and reservations."
                                ]
                            , Element.row
                                [ Element.width Element.fill
                                , Element.spacing 10
                                , Element.alignBottom
                                ]
                                [ Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Yellow
                                        , enabled = True
                                        , label = Element.text "Abort"
                                        , message = Just <| MessagePure CloseDialogToDeleteSpace
                                        , size = Mensam.Element.Button.Medium
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Red
                                        , enabled = True
                                        , label = Element.text "Delete Space permanently"
                                        , message = Just <| MessageEffect SubmitDeleteSpace
                                        , size = Mensam.Element.Button.Medium
                                        }
                                ]
                            ]

                Just PopupDeleteSpacePicture ->
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
                                Element.text "Delete Space Logo"
                            , Element.paragraph
                                []
                                [ Element.text "Are you sure you want to delete the current picture?"
                                ]
                            , Element.row
                                [ Element.width Element.fill
                                , Element.spacing 10
                                , Element.alignBottom
                                ]
                                [ Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Yellow
                                        , enabled = True
                                        , label = Element.text "Abort"
                                        , message = Just <| MessagePure CloseDialogToDeleteSpacePicture
                                        , size = Mensam.Element.Button.Medium
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Red
                                        , enabled = True
                                        , label = Element.text "Delete Picture"
                                        , message = Just <| MessageEffect DeleteSpacePictureRequest
                                        , size = Mensam.Element.Button.Medium
                                        }
                                ]
                            ]
        , closePopup = MessagePure ClosePopup
        }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect
    | Messages (List Message)


type MessagePure
    = ClosePopup
    | SetOldSettings
        { name : Mensam.Space.Name
        , timezone : Mensam.Time.Timezone
        , discoverability : Mensam.Space.Discoverability
        }
    | ResetNewSettings
    | EnterName (Maybe Mensam.Space.Name)
    | SetTimezone (Maybe Mensam.Time.Timezone)
    | SetHoveringTimezone (Maybe Int)
    | SetDiscoverability (Maybe Mensam.Space.Discoverability)
    | SetSpacePicture { url : String }
    | OpenDialogToDeleteSpace
    | CloseDialogToDeleteSpace
    | OpenDialogToDeleteSpacePicture
    | CloseDialogToDeleteSpacePicture


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        ClosePopup ->
            { model | popup = Nothing }

        SetOldSettings args ->
            { model | old = args }

        ResetNewSettings ->
            { model | new = { name = Nothing, timezone = Nothing, discoverability = Nothing } }

        EnterName name ->
            let
                newSettings =
                    model.new
            in
            { model | new = { newSettings | name = name } }

        SetTimezone maybeNewNewTimezone ->
            let
                newSettings =
                    model.new
            in
            { model
                | new =
                    { newSettings
                        | timezone =
                            case maybeNewNewTimezone of
                                Nothing ->
                                    Nothing

                                Just newNewTimezone ->
                                    case newSettings.timezone of
                                        Nothing ->
                                            Just { selected = newNewTimezone, hovering = Nothing }

                                        Just oldNewTimezone ->
                                            Just { oldNewTimezone | selected = newNewTimezone }
                    }
            }

        SetHoveringTimezone maybeN ->
            let
                newSettings =
                    model.new
            in
            case newSettings.timezone of
                Nothing ->
                    model

                Just newTimezone ->
                    { model | new = { newSettings | timezone = Just { newTimezone | hovering = maybeN } } }

        SetDiscoverability discoverability ->
            let
                newSettings =
                    model.new
            in
            { model | new = { newSettings | discoverability = discoverability } }

        SetSpacePicture picture ->
            { model | spacePictureUrl = picture.url }

        OpenDialogToDeleteSpace ->
            { model | popup = Just PopupDeleteSpace }

        CloseDialogToDeleteSpace ->
            updatePure ClosePopup model

        OpenDialogToDeleteSpacePicture ->
            { model | popup = Just PopupDeleteSpacePicture }

        CloseDialogToDeleteSpacePicture ->
            updatePure ClosePopup model


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshOldSettings
    | SubmitSettings
    | ReturnToSpace
    | ReturnToSpaces
    | SubmitDeleteSpace
    | UploadSpacePictureRequested
    | UploadSpacePictureUpload File.File
    | DeleteSpacePictureRequest
    | DownloadSpacePictureRequest
    | OpenPageToRoles


spaceEdit :
    Maybe Mensam.Http.Tracker.Tracker
    -> Mensam.Url.BaseUrl
    ->
        { jwt : Mensam.Auth.Bearer.Jwt
        , id : Mensam.Space.Identifier
        , name : Maybe Mensam.Space.Name
        , timezone : Maybe Mensam.Time.Timezone
        , discoverability : Maybe Mensam.Space.Discoverability
        }
    -> Cmd Message
spaceEdit tracker baseUrl requestArgs =
    Mensam.Api.SpaceEdit.request tracker baseUrl requestArgs <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceEdit.Success args) ->
                    Messages
                        [ MessagePure <| SetOldSettings { name = args.name, timezone = args.timezone, discoverability = args.discoverability }
                        , MessagePure ResetNewSettings
                        ]

                Ok (Mensam.Api.SpaceEdit.ErrorInsufficientPermission permission) ->
                    MessageEffect <| ReportError <| Mensam.Space.Role.errorInsufficientPermission permission

                Ok Mensam.Api.SpaceEdit.ErrorSpaceNotFound ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Try a different space identifier" <|
                                Mensam.Error.message "Space not found" <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.SpaceEdit.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Bad request body" <|
                                Mensam.Error.message error <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.SpaceEdit.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <| ReportError <| Mensam.Error.http error


spaceDelete :
    Maybe Mensam.Http.Tracker.Tracker
    -> Mensam.Url.BaseUrl
    ->
        { jwt : Mensam.Auth.Bearer.Jwt
        , id : Mensam.Space.Identifier
        }
    -> Cmd Message
spaceDelete tracker baseUrl requestArgs =
    Mensam.Api.SpaceDelete.request tracker baseUrl requestArgs <|
        \result ->
            case result of
                Ok Mensam.Api.SpaceDelete.Success ->
                    MessageEffect ReturnToSpaces

                Ok (Mensam.Api.SpaceDelete.ErrorInsufficientPermission permission) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to upload space picture" <|
                                Mensam.Space.Role.errorInsufficientPermission permission

                Ok Mensam.Api.SpaceDelete.ErrorSpaceNotFound ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to upload space picture" <|
                                Mensam.Error.message "Try a different space identifier" <|
                                    Mensam.Error.message "Space not found" <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.SpaceDelete.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to upload space picture" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.SpaceDelete.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <| ReportError <| Mensam.Error.http error


selectSpacePictureToUpload : Cmd Message
selectSpacePictureToUpload =
    File.Select.file [ "image/jpeg" ] <| \file -> MessageEffect <| UploadSpacePictureUpload file


downloadSpacePicture : Maybe Mensam.Http.Tracker.Tracker -> Mensam.Url.BaseUrl -> Mensam.Auth.Bearer.Jwt -> Mensam.Space.Identifier -> Cmd Message
downloadSpacePicture tracker baseUrl jwt space =
    Mensam.Api.SpacePictureDownload.request tracker
        baseUrl
        { jwt = jwt
        , space = space
        }
    <|
        \response ->
            case response of
                Ok (Mensam.Api.SpacePictureDownload.Success picture) ->
                    MessagePure <| SetSpacePicture picture

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to download space picture" <|
                                Mensam.Error.http error


uploadSpacePicture : Maybe Mensam.Http.Tracker.Tracker -> Mensam.Url.BaseUrl -> Mensam.Auth.Bearer.Jwt -> Mensam.Space.Identifier -> File.File -> Cmd Message
uploadSpacePicture tracker baseUrl jwt space file =
    Mensam.Api.SpacePictureUpload.request tracker
        baseUrl
        { jwt = jwt
        , space = space
        , picture = file
        }
    <|
        \response ->
            case response of
                Ok Mensam.Api.SpacePictureUpload.Success ->
                    MessageEffect DownloadSpacePictureRequest

                Ok (Mensam.Api.SpacePictureUpload.ErrorInsufficientPermission permission) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Space.Role.errorInsufficientPermission permission

                Ok Mensam.Api.SpacePictureUpload.ErrorSpaceNotFound ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to upload space picture" <|
                                Mensam.Error.message "Try a different space identifier" <|
                                    Mensam.Error.message "Space not found" <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.SpacePictureUpload.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to upload space picture" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message "Make sure to use JPEG" <|
                                        Mensam.Error.message error <|
                                            Mensam.Error.undefined

                Ok (Mensam.Api.SpacePictureUpload.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to upload space picture" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to upload space picture" <|
                                Mensam.Error.message "Try reducing the file size before uploading" <|
                                    Mensam.Error.http error


deleteSpacePicture : Maybe Mensam.Http.Tracker.Tracker -> Mensam.Url.BaseUrl -> Mensam.Auth.Bearer.Jwt -> Mensam.Space.Identifier -> Cmd Message
deleteSpacePicture tracker baseUrl jwt space =
    Mensam.Api.SpacePictureDelete.request tracker
        baseUrl
        { jwt = jwt
        , space = space
        }
    <|
        \response ->
            case response of
                Ok Mensam.Api.SpacePictureDelete.Success ->
                    Messages
                        [ MessagePure ClosePopup
                        , MessageEffect DownloadSpacePictureRequest
                        ]

                Ok (Mensam.Api.SpacePictureDelete.ErrorInsufficientPermission permission) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to upload space picture" <|
                                Mensam.Space.Role.errorInsufficientPermission permission

                Ok Mensam.Api.SpacePictureDelete.ErrorSpaceNotFound ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to upload space picture" <|
                                Mensam.Error.message "Try a different space identifier" <|
                                    Mensam.Error.message "Space not found" <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.SpacePictureDelete.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to upload space picture" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to upload space picture" <|
                                Mensam.Error.message "Try reducing the file size before uploading" <|
                                    Mensam.Error.http error
