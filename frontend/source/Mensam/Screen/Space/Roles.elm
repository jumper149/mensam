module Mensam.Screen.Space.Roles exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Element.Input
import Html.Attributes
import Mensam.Api.RoleCreate
import Mensam.Api.SpaceView
import Mensam.Auth.Bearer
import Mensam.Element.Button
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Element.Screen
import Mensam.Error
import Mensam.Space
import Mensam.Space.Role


type alias Model =
    { spaceId : Mensam.Space.Identifier
    , spaceName : Mensam.Space.Name
    , roles :
        List
            { id : Mensam.Space.Role.Identifier
            , name : Mensam.Space.Role.Name
            , accessibility : Mensam.Space.Role.Accessibility
            , permissions : Mensam.Space.Role.Permissions
            }
    , selected : Maybe Int
    , popup : Maybe PopupModel
    }


type PopupModel
    = PopupCreateRole
        { name : Mensam.Space.Role.Name
        , accessibility : Mensam.Space.Role.Accessibility
        , password : Maybe String
        , permissions :
            { viewSpace : Bool
            , editDesk : Bool
            , editUser : Bool
            , editRole : Bool
            , editSpace : Bool
            , createReservation : Bool
            , cancelReservation : Bool
            }
        }


init : { id : Mensam.Space.Identifier } -> Model
init args =
    { spaceId = args.id
    , spaceName = Mensam.Space.MkName ""
    , roles = []
    , selected = Nothing
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
                [ Element.row
                    [ Element.width Element.fill
                    , Element.height <| Element.px 70
                    , Element.padding 10
                    , Element.spacing 20
                    ]
                    [ Element.el
                        [ Element.Font.size 30
                        , Element.Font.hairline
                        , Element.alignLeft
                        , Element.centerY
                        ]
                      <|
                        Element.text "Edit Roles"
                    , Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = [ Element.alignRight, Element.centerY ]
                            , color = Mensam.Element.Button.Yellow
                            , label = Element.text "New Role"
                            , message = Just <| MessagePure OpenDialogToCreateRole
                            }
                    , Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = [ Element.alignRight, Element.centerY ]
                            , color = Mensam.Element.Button.Yellow
                            , label = Element.text "Go back"
                            , message = Just <| MessageEffect ReturnToSpaceSettings
                            }
                    ]
                , Element.indexedTable
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.Background.color (Element.rgba 0 0 0 0.1)
                    , Element.Font.family [ Mensam.Element.Font.condensed ]
                    , Element.Font.size 16
                    , Element.clipY
                    , Element.scrollbarY
                    ]
                    { data = model.roles
                    , columns =
                        let
                            cell =
                                Element.el
                                    [ Element.height <| Element.px 40
                                    , Element.padding 10
                                    ]
                        in
                        [ { header =
                                Element.el
                                    [ Element.Background.color (Element.rgba 0 0 0 0.3)
                                    ]
                                <|
                                    cell <|
                                        Element.el
                                            []
                                        <|
                                            Element.text "ID"
                          , width = Element.px 100
                          , view =
                                \n role ->
                                    Element.el
                                        [ Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
                                        , Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                                        , Element.Events.onClick <| MessageEffect <| ChooseRole role.id
                                        , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                        , let
                                            alpha =
                                                case model.selected of
                                                    Nothing ->
                                                        0.2

                                                    Just m ->
                                                        if m == n then
                                                            0.4

                                                        else
                                                            0.2
                                          in
                                          Element.Background.color (Element.rgba 0 0 0 alpha)
                                        ]
                                    <|
                                        cell <|
                                            Element.el
                                                [ Element.width <| Element.maximum 100 <| Element.fill ]
                                            <|
                                                Element.text <|
                                                    Mensam.Space.Role.identifierToString role.id
                          }
                        , { header =
                                Element.el
                                    [ Element.Background.color (Element.rgba 0 0 0 0.3)
                                    ]
                                <|
                                    cell <|
                                        Element.el
                                            []
                                        <|
                                            Element.text "Name"
                          , width = Element.fill
                          , view =
                                \n role ->
                                    Element.el
                                        [ Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
                                        , Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                                        , Element.Events.onClick <| MessageEffect <| ChooseRole role.id
                                        , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                        , let
                                            alpha =
                                                case model.selected of
                                                    Nothing ->
                                                        0.2

                                                    Just m ->
                                                        if m == n then
                                                            0.4

                                                        else
                                                            0.2
                                          in
                                          Element.Background.color (Element.rgba 0 0 0 alpha)
                                        ]
                                    <|
                                        cell <|
                                            Element.el
                                                [ Element.width <| Element.maximum 100 <| Element.fill ]
                                            <|
                                                Element.text <|
                                                    Mensam.Space.Role.nameToString role.name
                          }
                        , { header =
                                Element.el
                                    [ Element.Background.color (Element.rgba 0 0 0 0.3)
                                    ]
                                <|
                                    cell <|
                                        Element.el
                                            []
                                        <|
                                            Element.text "Accessibility"
                          , width = Element.px 100
                          , view =
                                \n role ->
                                    Element.el
                                        [ Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
                                        , Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                                        , Element.Events.onClick <| MessageEffect <| ChooseRole role.id
                                        , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                        , let
                                            alpha =
                                                case model.selected of
                                                    Nothing ->
                                                        0.2

                                                    Just m ->
                                                        if m == n then
                                                            0.4

                                                        else
                                                            0.2
                                          in
                                          Element.Background.color (Element.rgba 0 0 0 alpha)
                                        ]
                                    <|
                                        cell <|
                                            Element.el
                                                [ Element.width <| Element.maximum 100 <| Element.fill ]
                                            <|
                                                Element.text <|
                                                    Mensam.Space.Role.accessibilityToString role.accessibility
                          }
                        , { header =
                                Element.el
                                    [ Element.Background.color (Element.rgba 0 0 0 0.3)
                                    ]
                                <|
                                    cell <|
                                        Element.el
                                            []
                                        <|
                                            Element.text "Permissions"
                          , width = Element.fill
                          , view =
                                \n role ->
                                    Element.el
                                        [ Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
                                        , Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                                        , Element.Events.onClick <| MessageEffect <| ChooseRole role.id
                                        , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                        , let
                                            alpha =
                                                case model.selected of
                                                    Nothing ->
                                                        0.2

                                                    Just m ->
                                                        if m == n then
                                                            0.4

                                                        else
                                                            0.2
                                          in
                                          Element.Background.color (Element.rgba 0 0 0 alpha)
                                        ]
                                    <|
                                        cell <|
                                            Element.row
                                                [ Element.width <| Element.maximum 100 <| Element.fill, Element.spacing 5 ]
                                            <|
                                                List.map (Element.text << Mensam.Space.Role.permissionToString) <|
                                                    Mensam.Space.Role.permissionsToList role.permissions
                          }
                        ]
                    }
                ]
        , popup =
            case model.popup of
                Nothing ->
                    Nothing

                Just (PopupCreateRole popupModel) ->
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
                                Element.text "Create Role"
                            , Element.Input.text
                                [ Element.Font.color Mensam.Element.Color.dark.black
                                ]
                                { onChange = MessagePure << CreateRoleSetName << Mensam.Space.Role.MkName
                                , text = Mensam.Space.Role.nameToString popupModel.name
                                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Name"
                                , label = Element.Input.labelHidden "Name"
                                }
                            , Element.column
                                [ Element.spacing 10
                                , Element.paddingXY 20 0
                                , Element.width Element.fill
                                , Element.height <| Element.px 160
                                , Element.alignRight
                                ]
                                [ Element.Input.button
                                    [ if popupModel.accessibility == Mensam.Space.Role.MkAccessibilityInaccessible then
                                        Element.Background.color Mensam.Element.Color.bright.green

                                      else
                                        Element.Background.color Mensam.Element.Color.bright.white
                                    , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.magenta ]
                                    , Element.Font.color Mensam.Element.Color.dark.black
                                    , Element.width Element.fill
                                    , Element.paddingXY 10 8
                                    ]
                                    { onPress = Just <| MessagePure <| CreateRoleSetAccessibility <| Mensam.Space.Role.MkAccessibilityInaccessible
                                    , label =
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            , Element.Font.family [ Mensam.Element.Font.condensed ]
                                            ]
                                        <|
                                            Element.text "Inaccessible"
                                    }
                                , Element.Input.button
                                    [ if popupModel.accessibility == Mensam.Space.Role.MkAccessibilityJoinableWithPassword then
                                        Element.Background.color Mensam.Element.Color.bright.green

                                      else
                                        Element.Background.color Mensam.Element.Color.bright.white
                                    , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.magenta ]
                                    , Element.Font.color Mensam.Element.Color.dark.black
                                    , Element.width Element.fill
                                    , Element.paddingXY 10 8
                                    ]
                                    { onPress =
                                        Just <|
                                            Messages
                                                [ MessagePure <| CreateRoleSetAccessibility <| Mensam.Space.Role.MkAccessibilityJoinableWithPassword
                                                , MessagePure <| CreateRoleSetPassword <| Just ""
                                                ]
                                    , label =
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            , Element.Font.family [ Mensam.Element.Font.condensed ]
                                            ]
                                        <|
                                            Element.text "Requires Password"
                                    }
                                , Element.Input.button
                                    [ if popupModel.accessibility == Mensam.Space.Role.MkAccessibilityJoinable then
                                        Element.Background.color Mensam.Element.Color.bright.green

                                      else
                                        Element.Background.color Mensam.Element.Color.bright.white
                                    , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.magenta ]
                                    , Element.Font.color Mensam.Element.Color.dark.black
                                    , Element.width Element.fill
                                    , Element.paddingXY 10 8
                                    ]
                                    { onPress = Just <| MessagePure <| CreateRoleSetAccessibility <| Mensam.Space.Role.MkAccessibilityJoinable
                                    , label =
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            , Element.Font.family [ Mensam.Element.Font.condensed ]
                                            ]
                                        <|
                                            Element.text "Joinable"
                                    }
                                , case popupModel.accessibility of
                                    Mensam.Space.Role.MkAccessibilityJoinableWithPassword ->
                                        Element.Input.text
                                            [ Element.Font.color Mensam.Element.Color.dark.black
                                            ]
                                            { onChange = MessagePure << CreateRoleSetPassword << Just
                                            , text = Maybe.withDefault "" popupModel.password
                                            , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Password"
                                            , label = Element.Input.labelHidden "Password"
                                            }

                                    _ ->
                                        Element.none
                                ]
                            , Element.column
                                [ Element.spacing 5
                                , Element.paddingXY 20 0
                                , Element.width Element.fill
                                , Element.height <| Element.px 120
                                , Element.alignLeft
                                ]
                                [ Element.row
                                    [ Element.spacing 10 ]
                                    [ Element.Input.checkbox []
                                        { onChange = MessagePure << CreateRoleSetPermission Mensam.Space.Role.MkPermissionViewSpace
                                        , icon = Element.Input.defaultCheckbox
                                        , checked = popupModel.permissions.viewSpace
                                        , label = Element.Input.labelHidden "Permission: view-space"
                                        }
                                    , Element.text "View Space"
                                    ]
                                , Element.row
                                    [ Element.spacing 10 ]
                                    [ Element.Input.checkbox []
                                        { onChange = MessagePure << CreateRoleSetPermission Mensam.Space.Role.MkPermissionEditDesk
                                        , icon = Element.Input.defaultCheckbox
                                        , checked = popupModel.permissions.editDesk
                                        , label = Element.Input.labelHidden "Permission: edit-desk"
                                        }
                                    , Element.text "Edit Desks"
                                    ]
                                , Element.row
                                    [ Element.spacing 10 ]
                                    [ Element.Input.checkbox []
                                        { onChange = MessagePure << CreateRoleSetPermission Mensam.Space.Role.MkPermissionEditUser
                                        , icon = Element.Input.defaultCheckbox
                                        , checked = popupModel.permissions.editUser
                                        , label = Element.Input.labelHidden "Permission: edit-user"
                                        }
                                    , Element.text "Edit Users"
                                    ]
                                , Element.row
                                    [ Element.spacing 10 ]
                                    [ Element.Input.checkbox []
                                        { onChange = MessagePure << CreateRoleSetPermission Mensam.Space.Role.MkPermissionEditRole
                                        , icon = Element.Input.defaultCheckbox
                                        , checked = popupModel.permissions.editRole
                                        , label = Element.Input.labelHidden "Permission: edit-role"
                                        }
                                    , Element.text "Edit Roles"
                                    ]
                                , Element.row
                                    [ Element.spacing 10 ]
                                    [ Element.Input.checkbox []
                                        { onChange = MessagePure << CreateRoleSetPermission Mensam.Space.Role.MkPermissionEditSpace
                                        , icon = Element.Input.defaultCheckbox
                                        , checked = popupModel.permissions.editSpace
                                        , label = Element.Input.labelHidden "Permission: edit-space"
                                        }
                                    , Element.text "Edit Space"
                                    ]
                                , Element.row
                                    [ Element.spacing 10 ]
                                    [ Element.Input.checkbox []
                                        { onChange = MessagePure << CreateRoleSetPermission Mensam.Space.Role.MkPermissionCreateReservation
                                        , icon = Element.Input.defaultCheckbox
                                        , checked = popupModel.permissions.createReservation
                                        , label = Element.Input.labelHidden "Permission: create-reservation"
                                        }
                                    , Element.text "Create Reservations"
                                    ]
                                , Element.row
                                    [ Element.spacing 10 ]
                                    [ Element.Input.checkbox []
                                        { onChange = MessagePure << CreateRoleSetPermission Mensam.Space.Role.MkPermissionCancelReservation
                                        , icon = Element.Input.defaultCheckbox
                                        , checked = popupModel.permissions.cancelReservation
                                        , label = Element.Input.labelHidden "Permission: cancel-reservation"
                                        }
                                    , Element.text "Cancel Reservations"
                                    ]
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
                                        , label = Element.text "Abort"
                                        , message = Just <| MessagePure CloseDialogToCreateRole
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Blue
                                        , label = Element.text "Create Role"
                                        , message = Just <| MessageEffect <| SubmitCreateRole popupModel
                                        }
                                ]
                            ]
        , closePopup = MessagePure CloseDialogToCreateRole
        }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect
    | Messages (List Message)


type MessagePure
    = SetSpaceName Mensam.Space.Name
    | SetRoles
        (List
            { id : Mensam.Space.Role.Identifier
            , name : Mensam.Space.Role.Name
            , accessibility : Mensam.Space.Role.Accessibility
            , permissions : Mensam.Space.Role.Permissions
            }
        )
    | SetSelected (Maybe Int)
    | OpenDialogToCreateRole
    | CloseDialogToCreateRole
    | CreateRoleSetName Mensam.Space.Role.Name
    | CreateRoleSetAccessibility Mensam.Space.Role.Accessibility
    | CreateRoleSetPassword (Maybe String)
    | CreateRoleSetPermission Mensam.Space.Role.Permission Bool


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetSpaceName name ->
            { model | spaceName = name }

        SetRoles roles ->
            { model | roles = roles }

        SetSelected n ->
            { model | selected = n }

        OpenDialogToCreateRole ->
            { model
                | popup =
                    Just <|
                        PopupCreateRole
                            { name = Mensam.Space.Role.MkName ""
                            , accessibility = Mensam.Space.Role.MkAccessibilityInaccessible
                            , password = Nothing
                            , permissions =
                                { viewSpace = False
                                , editDesk = False
                                , editUser = False
                                , editRole = False
                                , editSpace = False
                                , createReservation = False
                                , cancelReservation = False
                                }
                            }
            }

        CloseDialogToCreateRole ->
            { model | popup = Nothing }

        CreateRoleSetName name ->
            case model.popup of
                Just (PopupCreateRole popupModel) ->
                    { model | popup = Just <| PopupCreateRole { popupModel | name = name } }

                Nothing ->
                    model

        CreateRoleSetAccessibility accessibility ->
            case model.popup of
                Just (PopupCreateRole popupModel) ->
                    { model | popup = Just <| PopupCreateRole { popupModel | accessibility = accessibility } }

                Nothing ->
                    model

        CreateRoleSetPassword password ->
            case model.popup of
                Just (PopupCreateRole popupModel) ->
                    { model | popup = Just <| PopupCreateRole { popupModel | password = password } }

                Nothing ->
                    model

        CreateRoleSetPermission permission bool ->
            case model.popup of
                Just (PopupCreateRole popupModel) ->
                    let
                        oldPermissions =
                            popupModel.permissions
                    in
                    { model
                        | popup =
                            Just <|
                                PopupCreateRole
                                    { popupModel
                                        | permissions =
                                            case permission of
                                                Mensam.Space.Role.MkPermissionViewSpace ->
                                                    { oldPermissions | viewSpace = bool }

                                                Mensam.Space.Role.MkPermissionEditDesk ->
                                                    { oldPermissions | editDesk = bool }

                                                Mensam.Space.Role.MkPermissionEditUser ->
                                                    { oldPermissions | editUser = bool }

                                                Mensam.Space.Role.MkPermissionEditRole ->
                                                    { oldPermissions | editRole = bool }

                                                Mensam.Space.Role.MkPermissionEditSpace ->
                                                    { oldPermissions | editSpace = bool }

                                                Mensam.Space.Role.MkPermissionCreateReservation ->
                                                    { oldPermissions | createReservation = bool }

                                                Mensam.Space.Role.MkPermissionCancelReservation ->
                                                    { oldPermissions | cancelReservation = bool }
                                    }
                    }

                Nothing ->
                    model


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshRoles
    | ChooseRole Mensam.Space.Role.Identifier
    | SubmitCreateRole
        { name : Mensam.Space.Role.Name
        , accessibility : Mensam.Space.Role.Accessibility
        , password : Maybe String
        , permissions :
            { viewSpace : Bool
            , editDesk : Bool
            , editUser : Bool
            , editRole : Bool
            , editSpace : Bool
            , createReservation : Bool
            , cancelReservation : Bool
            }
        }
    | ReturnToSpaceSettings


spaceView : Mensam.Auth.Bearer.Jwt -> Mensam.Space.Identifier -> Cmd Message
spaceView jwt id =
    Mensam.Api.SpaceView.request { jwt = jwt, id = id } <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceView.Success view) ->
                    Messages
                        [ MessagePure <| SetRoles view.roles
                        , MessagePure <| SetSpaceName view.name
                        ]

                Ok (Mensam.Api.SpaceView.ErrorInsufficientPermission permission) ->
                    MessageEffect <| ReportError <| Mensam.Space.Role.errorInsufficientPermission permission

                Ok (Mensam.Api.SpaceView.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Bad request body" <|
                                Mensam.Error.message error <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.SpaceView.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <| ReportError <| Mensam.Error.http error


roleCreate :
    { jwt : Mensam.Auth.Bearer.Jwt
    , space : Mensam.Space.Identifier
    , name : Mensam.Space.Role.Name
    , accessibility : Mensam.Space.Role.Accessibility
    , password : Maybe String
    , permissions :
        { viewSpace : Bool
        , editDesk : Bool
        , editUser : Bool
        , editRole : Bool
        , editSpace : Bool
        , createReservation : Bool
        , cancelReservation : Bool
        }
    }
    -> Cmd Message
roleCreate args =
    Mensam.Api.RoleCreate.request
        { jwt = args.jwt
        , space = args.space
        , name = args.name
        , accessibility = args.accessibility
        , password = args.password
        , permissions =
            Mensam.Space.Role.permissionsFromList <|
                List.filterMap (\p -> p) <|
                    [ if args.permissions.viewSpace then
                        Just Mensam.Space.Role.MkPermissionViewSpace

                      else
                        Nothing
                    , if args.permissions.editSpace then
                        Just Mensam.Space.Role.MkPermissionEditSpace

                      else
                        Nothing
                    , if args.permissions.editDesk then
                        Just Mensam.Space.Role.MkPermissionEditDesk

                      else
                        Nothing
                    , if args.permissions.editUser then
                        Just Mensam.Space.Role.MkPermissionEditUser

                      else
                        Nothing
                    , if args.permissions.editRole then
                        Just Mensam.Space.Role.MkPermissionEditRole

                      else
                        Nothing
                    , if args.permissions.createReservation then
                        Just Mensam.Space.Role.MkPermissionCreateReservation

                      else
                        Nothing
                    , if args.permissions.cancelReservation then
                        Just Mensam.Space.Role.MkPermissionCancelReservation

                      else
                        Nothing
                    ]
        }
    <|
        \result ->
            case result of
                Ok (Mensam.Api.RoleCreate.Success _) ->
                    Messages
                        [ MessagePure CloseDialogToCreateRole
                        , MessageEffect RefreshRoles
                        ]

                Ok (Mensam.Api.RoleCreate.ErrorInsufficientPermission permission) ->
                    MessageEffect <| ReportError <| Mensam.Space.Role.errorInsufficientPermission permission

                Ok Mensam.Api.RoleCreate.ErrorSpaceNotFound ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Try a different space identifier" <|
                                Mensam.Error.message "Space not found" <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.RoleCreate.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Bad request body" <|
                                Mensam.Error.message error <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.RoleCreate.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <| ReportError <| Mensam.Error.http error
