module Mensam.Screen.Space.Role exposing (..)

import Element
import Element.Background
import Element.Events.Pointer
import Element.Font
import Element.Input
import Html.Attributes
import List.Extra
import Mensam.Api.RoleDelete
import Mensam.Api.RoleEdit
import Mensam.Api.SpaceView
import Mensam.Auth.Bearer
import Mensam.Element.Button
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Element.Screen
import Mensam.Error
import Mensam.Space
import Mensam.Space.Role
import Mensam.Time
import Mensam.User


type alias Model =
    { space :
        { id : Mensam.Space.Identifier
        }
    , roles :
        List
            { id : Mensam.Space.Role.Identifier
            , name : Mensam.Space.Role.Name
            , accessibility : Mensam.Space.Role.Accessibility
            , permissions : Mensam.Space.Role.Permissions
            }
    , role :
        { id : Mensam.Space.Role.Identifier
        , name : Mensam.Space.Role.Name
        , accessibility : Mensam.Space.Role.Accessibility
        , permissions : Mensam.Space.Role.Permissions
        }
    , new :
        { name : Maybe Mensam.Space.Role.Name
        , accessibilityAndPassword :
            Maybe
                { accessibility : Mensam.Space.Role.Accessibility
                , maybePassword : Maybe String
                }
        , permissions :
            Maybe
                { viewSpace : Bool
                , editDesk : Bool
                , editUser : Bool
                , editRole : Bool
                , editSpace : Bool
                , createReservation : Bool
                , cancelReservation : Bool
                }
        }
    , popup : Maybe PopupModel
    }


type PopupModel
    = PopupDeleteRole { selectedN : Maybe Int, chosenFallback : Maybe Mensam.Space.Role.Identifier }


init : { spaceId : Mensam.Space.Identifier, roleId : Mensam.Space.Role.Identifier } -> Model
init args =
    { space =
        { id = args.spaceId
        }
    , roles = []
    , role =
        { id = args.roleId
        , name = Mensam.Space.Role.MkName ""
        , accessibility = Mensam.Space.Role.MkAccessibilityInaccessible
        , permissions = Mensam.Space.Role.permissionsFromList []
        }
    , new =
        { name = Nothing
        , accessibilityAndPassword = Nothing
        , permissions = Nothing
        }
    , popup = Nothing
    }


element : Model -> Element.Element Message
element model =
    Mensam.Element.Screen.element
        { main =
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.clipY
                , Element.scrollbarY
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
                        Element.text "Edit Role"
                    , Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = [ Element.alignLeft, Element.centerY ]
                            , color = Mensam.Element.Button.Red
                            , enabled = True
                            , label = Element.text "Delete Role"
                            , message = Just <| MessagePure OpenDialogToDeleteRole
                            , size = Mensam.Element.Button.Medium
                            }
                    , Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = [ Element.alignRight, Element.centerY ]
                            , color = Mensam.Element.Button.Yellow
                            , enabled = True
                            , label = Element.text "Go back"
                            , message = Just <| MessageEffect ReturnToRoles
                            , size = Mensam.Element.Button.Medium
                            }
                    ]
                , Element.column
                    [ Element.spacing 20
                    , Element.width Element.fill
                    , Element.height Element.fill
                    , Element.paddingEach { top = 0, left = 0, bottom = 15, right = 0 }
                    ]
                    [ Element.row
                        [ Element.spacing 20
                        , Element.width Element.fill
                        , Element.height <| Element.px 25
                        ]
                        [ Element.el [] <| Element.text "Current Name:"
                        , Element.el [] <| Element.text <| Mensam.Space.Role.nameToString model.role.name
                        ]
                    , Element.el
                        [ Element.paddingXY 30 5
                        , Element.width Element.fill
                        , Element.height <| Element.px 40
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
                                        , message = Just <| MessagePure <| EnterName <| Just model.role.name
                                        , size = Mensam.Element.Button.Medium
                                        }

                            Just name ->
                                Element.Input.text
                                    [ Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                                    ]
                                    { onChange = MessagePure << EnterName << Just << Mensam.Space.Role.MkName
                                    , text = Mensam.Space.Role.nameToString name
                                    , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Name"
                                    , label = Element.Input.labelHidden "Name"
                                    }
                    , Element.row
                        [ Element.spacing 20
                        , Element.width Element.fill
                        , Element.height <| Element.px 25
                        ]
                        [ Element.el [] <| Element.text "Current Accessibility:"
                        , Element.el [] <| Element.text <| Mensam.Space.Role.accessibilityToString model.role.accessibility
                        ]
                    , Element.el
                        [ Element.paddingXY 30 5
                        , Element.width Element.fill
                        , Element.height <| Element.px 175
                        ]
                      <|
                        case model.new.accessibilityAndPassword of
                            Nothing ->
                                Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Yellow
                                        , enabled = True
                                        , label = Element.text "Edit Accessibility"
                                        , message = Just <| MessagePure EditRoleAccessibilityAndPasswordStart
                                        , size = Mensam.Element.Button.Medium
                                        }

                            Just accessibilityAndPassword ->
                                Element.column
                                    [ Element.spacing 10
                                    , Element.paddingXY 20 0
                                    , Element.width Element.fill
                                    , Element.height <| Element.px 160
                                    , Element.alignRight
                                    ]
                                    -- TODO: Use `Mensam.Element.Button.button`.
                                    [ Element.Input.button
                                        [ if accessibilityAndPassword.accessibility == Mensam.Space.Role.MkAccessibilityInaccessible then
                                            Element.Background.color <| Mensam.Element.Color.bright.green Mensam.Element.Color.Opaque100

                                          else
                                            Element.Background.color <| Mensam.Element.Color.bright.white Mensam.Element.Color.Opaque100
                                        , Element.mouseOver [ Element.Background.color <| Mensam.Element.Color.bright.magenta Mensam.Element.Color.Opaque100 ]
                                        , Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                                        , Element.width Element.fill
                                        , Element.paddingXY 10 8
                                        ]
                                        { onPress =
                                            Just <|
                                                Messages
                                                    [ MessagePure <| EditRoleSetAccessibility <| Mensam.Space.Role.MkAccessibilityInaccessible
                                                    , MessagePure <| EditRoleSetPassword Nothing
                                                    ]
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
                                        [ if accessibilityAndPassword.accessibility == Mensam.Space.Role.MkAccessibilityJoinableWithPassword then
                                            Element.Background.color <| Mensam.Element.Color.bright.green Mensam.Element.Color.Opaque100

                                          else
                                            Element.Background.color <| Mensam.Element.Color.bright.white Mensam.Element.Color.Opaque100
                                        , Element.mouseOver [ Element.Background.color <| Mensam.Element.Color.bright.magenta Mensam.Element.Color.Opaque100 ]
                                        , Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                                        , Element.width Element.fill
                                        , Element.paddingXY 10 8
                                        ]
                                        { onPress =
                                            Just <|
                                                Messages
                                                    [ MessagePure <| EditRoleSetAccessibility <| Mensam.Space.Role.MkAccessibilityJoinableWithPassword
                                                    , MessagePure <| EditRoleSetPassword <| Just ""
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
                                        [ if accessibilityAndPassword.accessibility == Mensam.Space.Role.MkAccessibilityJoinable then
                                            Element.Background.color <| Mensam.Element.Color.bright.green Mensam.Element.Color.Opaque100

                                          else
                                            Element.Background.color <| Mensam.Element.Color.bright.white Mensam.Element.Color.Opaque100
                                        , Element.mouseOver [ Element.Background.color <| Mensam.Element.Color.bright.magenta Mensam.Element.Color.Opaque100 ]
                                        , Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                                        , Element.width Element.fill
                                        , Element.paddingXY 10 8
                                        ]
                                        { onPress =
                                            Just <|
                                                Messages
                                                    [ MessagePure <| EditRoleSetAccessibility <| Mensam.Space.Role.MkAccessibilityJoinable
                                                    , MessagePure <| EditRoleSetPassword Nothing
                                                    ]
                                        , label =
                                            Element.el
                                                [ Element.centerX
                                                , Element.centerY
                                                , Element.Font.family [ Mensam.Element.Font.condensed ]
                                                ]
                                            <|
                                                Element.text "Joinable"
                                        }
                                    , case accessibilityAndPassword.accessibility of
                                        Mensam.Space.Role.MkAccessibilityJoinableWithPassword ->
                                            Element.Input.text
                                                [ Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                                                ]
                                                { onChange = MessagePure << EditRoleSetPassword << Just
                                                , text = Maybe.withDefault "" accessibilityAndPassword.maybePassword
                                                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Password"
                                                , label = Element.Input.labelHidden "Password"
                                                }

                                        _ ->
                                            Element.none
                                    ]
                    , Element.row
                        [ Element.spacing 20
                        , Element.width Element.fill
                        , Element.height <| Element.px 25
                        ]
                        [ Element.el [] <| Element.text "Permissions"
                        ]
                    , Element.el
                        [ Element.paddingXY 30 5
                        , Element.width Element.fill
                        , Element.height <| Element.px 175
                        ]
                      <|
                        case model.new.permissions of
                            Nothing ->
                                Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Yellow
                                        , enabled = True
                                        , label = Element.text "Edit Permissions"
                                        , message = Just <| MessagePure EditRolePermissionsStart
                                        , size = Mensam.Element.Button.Medium
                                        }

                            Just permissions ->
                                Element.column
                                    [ Element.spacing 10
                                    , Element.paddingXY 20 0
                                    , Element.width Element.fill
                                    , Element.height <| Element.px 120
                                    , Element.alignLeft
                                    ]
                                    [ Element.row
                                        [ Element.spacing 10 ]
                                        [ Element.Input.checkbox []
                                            { onChange = MessagePure << EditRoleSetPermission Mensam.Space.Role.MkPermissionViewSpace
                                            , icon = Element.Input.defaultCheckbox
                                            , checked = permissions.viewSpace
                                            , label = Element.Input.labelHidden "Permission: view-space"
                                            }
                                        , Element.text "View Space"
                                        ]
                                    , Element.row
                                        [ Element.spacing 10 ]
                                        [ Element.Input.checkbox []
                                            { onChange = MessagePure << EditRoleSetPermission Mensam.Space.Role.MkPermissionEditDesk
                                            , icon = Element.Input.defaultCheckbox
                                            , checked = permissions.editDesk
                                            , label = Element.Input.labelHidden "Permission: edit-desk"
                                            }
                                        , Element.text "Edit Desks"
                                        ]
                                    , Element.row
                                        [ Element.spacing 10 ]
                                        [ Element.Input.checkbox []
                                            { onChange = MessagePure << EditRoleSetPermission Mensam.Space.Role.MkPermissionEditUser
                                            , icon = Element.Input.defaultCheckbox
                                            , checked = permissions.editUser
                                            , label = Element.Input.labelHidden "Permission: edit-user"
                                            }
                                        , Element.text "Edit Users"
                                        ]
                                    , Element.row
                                        [ Element.spacing 10 ]
                                        [ Element.Input.checkbox []
                                            { onChange = MessagePure << EditRoleSetPermission Mensam.Space.Role.MkPermissionEditRole
                                            , icon = Element.Input.defaultCheckbox
                                            , checked = permissions.editRole
                                            , label = Element.Input.labelHidden "Permission: edit-role"
                                            }
                                        , Element.text "Edit Roles"
                                        ]
                                    , Element.row
                                        [ Element.spacing 10 ]
                                        [ Element.Input.checkbox []
                                            { onChange = MessagePure << EditRoleSetPermission Mensam.Space.Role.MkPermissionEditSpace
                                            , icon = Element.Input.defaultCheckbox
                                            , checked = permissions.editSpace
                                            , label = Element.Input.labelHidden "Permission: edit-space"
                                            }
                                        , Element.text "Edit Space"
                                        ]
                                    , Element.row
                                        [ Element.spacing 10 ]
                                        [ Element.Input.checkbox []
                                            { onChange = MessagePure << EditRoleSetPermission Mensam.Space.Role.MkPermissionCreateReservation
                                            , icon = Element.Input.defaultCheckbox
                                            , checked = permissions.createReservation
                                            , label = Element.Input.labelHidden "Permission: create-reservation"
                                            }
                                        , Element.text "Create Reservations"
                                        ]
                                    , Element.row
                                        [ Element.spacing 10 ]
                                        [ Element.Input.checkbox []
                                            { onChange = MessagePure << EditRoleSetPermission Mensam.Space.Role.MkPermissionCancelReservation
                                            , icon = Element.Input.defaultCheckbox
                                            , checked = permissions.cancelReservation
                                            , label = Element.Input.labelHidden "Permission: cancel-reservation"
                                            }
                                        , Element.text "Cancel Reservations"
                                        ]
                                    ]
                    , Element.row
                        [ Element.spacing 20
                        , Element.width Element.fill
                        ]
                        [ Mensam.Element.Button.button <|
                            Mensam.Element.Button.MkButton
                                { attributes = []
                                , color = Mensam.Element.Button.Yellow
                                , enabled = True
                                , label = Element.text "Go back"
                                , message = Just <| MessageEffect ReturnToRoles
                                , size = Mensam.Element.Button.Medium
                                }
                        , Mensam.Element.Button.button <|
                            Mensam.Element.Button.MkButton
                                { attributes = [ Element.width Element.fill ]
                                , color = Mensam.Element.Button.Blue
                                , enabled = True
                                , label = Element.text "Apply Settings"
                                , message = Just <| MessageEffect SubmitEditRole
                                , size = Mensam.Element.Button.Medium
                                }
                        ]
                    ]
                ]
        , popup =
            case model.popup of
                Nothing ->
                    Nothing

                Just (PopupDeleteRole popupModel) ->
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
                                Element.text "Delete Role"
                            , Element.paragraph
                                []
                                [ Element.text "Do you want to delete this role permanently?"
                                ]
                            , Element.paragraph
                                []
                                [ Element.text "To delete this role you have to select a fallback role that any remaining members of this role will be assigned to."
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
                                { data = List.filter (\role -> role.id /= model.role.id) model.roles
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
                                                        Element.text "Fallback Role"
                                      , width = Element.fill
                                      , view =
                                            \n role ->
                                                Element.el
                                                    [ Element.Events.Pointer.onLeave <| \_ -> MessagePure <| DeleteRoleSetSelected Nothing
                                                    , Element.Events.Pointer.onEnter <| \_ -> MessagePure <| DeleteRoleSetSelected <| Just n
                                                    , Element.Events.Pointer.onClick <| \_ -> MessagePure <| DeleteRoleChooseRole <| Just role.id
                                                    , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                                    , let
                                                        alpha =
                                                            case popupModel.selectedN of
                                                                Nothing ->
                                                                    0.2

                                                                Just m ->
                                                                    if m == n then
                                                                        0.4

                                                                    else
                                                                        0.2
                                                      in
                                                      if Just role.id == popupModel.chosenFallback then
                                                        Element.Background.color (Element.rgba 0 0.3 0 alpha)

                                                      else
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
                                    ]
                                }
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
                                        , message = Just <| MessagePure CloseDialogToDeleteRole
                                        , size = Mensam.Element.Button.Medium
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Red
                                        , enabled = True
                                        , label = Element.text "Delete Role permanently"
                                        , message = Maybe.map (\fallback -> MessageEffect <| SubmitDeleteRole { fallback = fallback }) popupModel.chosenFallback
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
    = ApplySpaceView { id : Mensam.Space.Identifier, name : Mensam.Space.Name, roles : List { accessibility : Mensam.Space.Role.Accessibility, id : Mensam.Space.Role.Identifier, name : Mensam.Space.Role.Name, permissions : Mensam.Space.Role.Permissions }, users : List { user : Mensam.User.Identifier, role : Mensam.Space.Role.Identifier }, timezone : Mensam.Time.Timezone, visibility : Mensam.Space.Visibility, owner : Mensam.User.Identifier, yourRole : Maybe { accessibility : Mensam.Space.Role.Accessibility, id : Mensam.Space.Role.Identifier, name : Mensam.Space.Role.Name, permissions : Mensam.Space.Role.Permissions } }
    | EnterName (Maybe Mensam.Space.Role.Name)
    | EditRoleAccessibilityAndPasswordStart
    | EditRoleSetAccessibility Mensam.Space.Role.Accessibility
    | EditRoleSetPassword (Maybe String)
    | EditRolePermissionsStart
    | EditRoleSetPermission Mensam.Space.Role.Permission Bool
    | ResetNewRole
    | ClosePopup
    | OpenDialogToDeleteRole
    | CloseDialogToDeleteRole
    | DeleteRoleSetSelected (Maybe Int)
    | DeleteRoleChooseRole (Maybe Mensam.Space.Role.Identifier)


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        ApplySpaceView view ->
            case List.Extra.find (\role -> role.id == model.role.id) view.roles of
                Nothing ->
                    model

                Just role ->
                    { model | role = role, roles = view.roles }

        EnterName name ->
            { model
                | new =
                    let
                        modelNew =
                            model.new
                    in
                    { modelNew | name = name }
            }

        ResetNewRole ->
            { model
                | new =
                    { name = Nothing
                    , accessibilityAndPassword = Nothing
                    , permissions = Nothing
                    }
            }

        EditRoleAccessibilityAndPasswordStart ->
            { model
                | new =
                    let
                        modelNew =
                            model.new
                    in
                    { modelNew
                        | accessibilityAndPassword =
                            Just
                                { accessibility = model.role.accessibility
                                , maybePassword =
                                    case model.role.accessibility of
                                        Mensam.Space.Role.MkAccessibilityJoinable ->
                                            Nothing

                                        Mensam.Space.Role.MkAccessibilityJoinableWithPassword ->
                                            Just ""

                                        Mensam.Space.Role.MkAccessibilityInaccessible ->
                                            Nothing
                                }
                    }
            }

        EditRoleSetAccessibility newAccessibility ->
            { model
                | new =
                    let
                        modelNew =
                            model.new
                    in
                    { modelNew
                        | accessibilityAndPassword =
                            case modelNew.accessibilityAndPassword of
                                Nothing ->
                                    Nothing

                                Just justAccAndPw ->
                                    Just { justAccAndPw | accessibility = newAccessibility }
                    }
            }

        EditRoleSetPassword newMaybePassword ->
            { model
                | new =
                    let
                        modelNew =
                            model.new
                    in
                    { modelNew
                        | accessibilityAndPassword =
                            case modelNew.accessibilityAndPassword of
                                Nothing ->
                                    Nothing

                                Just justAccAndPw ->
                                    Just { justAccAndPw | maybePassword = newMaybePassword }
                    }
            }

        EditRolePermissionsStart ->
            { model
                | new =
                    let
                        modelNew =
                            model.new
                    in
                    { modelNew
                        | permissions =
                            let
                                checkPerm p =
                                    List.member p <| Mensam.Space.Role.permissionsToList model.role.permissions
                            in
                            Just
                                { viewSpace = checkPerm Mensam.Space.Role.MkPermissionViewSpace
                                , editDesk = checkPerm Mensam.Space.Role.MkPermissionEditDesk
                                , editUser = checkPerm Mensam.Space.Role.MkPermissionEditUser
                                , editRole = checkPerm Mensam.Space.Role.MkPermissionEditRole
                                , editSpace = checkPerm Mensam.Space.Role.MkPermissionEditSpace
                                , createReservation = checkPerm Mensam.Space.Role.MkPermissionCreateReservation
                                , cancelReservation = checkPerm Mensam.Space.Role.MkPermissionCancelReservation
                                }
                    }
            }

        EditRoleSetPermission permission boolean ->
            { model
                | new =
                    let
                        modelNew =
                            model.new
                    in
                    { modelNew
                        | permissions =
                            case modelNew.permissions of
                                Nothing ->
                                    Nothing

                                Just permissions ->
                                    let
                                        updatePerm p oldValue =
                                            if p == permission then
                                                boolean

                                            else
                                                oldValue
                                    in
                                    Just
                                        { viewSpace = updatePerm Mensam.Space.Role.MkPermissionViewSpace permissions.viewSpace
                                        , editDesk = updatePerm Mensam.Space.Role.MkPermissionEditDesk permissions.editDesk
                                        , editUser = updatePerm Mensam.Space.Role.MkPermissionEditUser permissions.editUser
                                        , editRole = updatePerm Mensam.Space.Role.MkPermissionEditRole permissions.editRole
                                        , editSpace = updatePerm Mensam.Space.Role.MkPermissionEditSpace permissions.editSpace
                                        , createReservation = updatePerm Mensam.Space.Role.MkPermissionCreateReservation permissions.createReservation
                                        , cancelReservation = updatePerm Mensam.Space.Role.MkPermissionCancelReservation permissions.cancelReservation
                                        }
                    }
            }

        ClosePopup ->
            { model | popup = Nothing }

        OpenDialogToDeleteRole ->
            { model | popup = Just <| PopupDeleteRole { selectedN = Nothing, chosenFallback = Nothing } }

        CloseDialogToDeleteRole ->
            { model | popup = Nothing }

        DeleteRoleSetSelected maybeN ->
            case model.popup of
                Nothing ->
                    model

                Just (PopupDeleteRole popupModel) ->
                    { model | popup = Just <| PopupDeleteRole { popupModel | selectedN = maybeN } }

        DeleteRoleChooseRole maybeRoleId ->
            case model.popup of
                Nothing ->
                    model

                Just (PopupDeleteRole popupModel) ->
                    { model | popup = Just <| PopupDeleteRole { popupModel | chosenFallback = maybeRoleId } }


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshRole
    | SubmitEditRole
    | SubmitDeleteRole { fallback : Mensam.Space.Role.Identifier }
    | ReturnToRoles


spaceView : { jwt : Mensam.Auth.Bearer.Jwt, yourUserId : Mensam.User.Identifier } -> Mensam.Space.Identifier -> Cmd Message
spaceView auth id =
    Mensam.Api.SpaceView.request { jwt = auth.jwt, yourUserId = auth.yourUserId, id = id } <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceView.Success view) ->
                    Messages
                        [ MessagePure <| ApplySpaceView view
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


roleEdit :
    { jwt : Mensam.Auth.Bearer.Jwt
    , id : Mensam.Space.Role.Identifier
    , name : Maybe Mensam.Space.Role.Name
    , accessibilityAndPassword :
        Maybe
            { accessibility : Mensam.Space.Role.Accessibility
            , maybePassword : Maybe String
            }
    , permissions :
        Maybe
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
roleEdit args =
    Mensam.Api.RoleEdit.request
        { jwt = args.jwt
        , id = args.id
        , name = args.name
        , accessibilityAndPassword = args.accessibilityAndPassword
        , permissions =
            case args.permissions of
                Nothing ->
                    Nothing

                Just permissions ->
                    Just <|
                        Mensam.Space.Role.permissionsFromList <|
                            List.filterMap (\p -> p) <|
                                [ if permissions.viewSpace then
                                    Just Mensam.Space.Role.MkPermissionViewSpace

                                  else
                                    Nothing
                                , if permissions.editSpace then
                                    Just Mensam.Space.Role.MkPermissionEditSpace

                                  else
                                    Nothing
                                , if permissions.editDesk then
                                    Just Mensam.Space.Role.MkPermissionEditDesk

                                  else
                                    Nothing
                                , if permissions.editUser then
                                    Just Mensam.Space.Role.MkPermissionEditUser

                                  else
                                    Nothing
                                , if permissions.editRole then
                                    Just Mensam.Space.Role.MkPermissionEditRole

                                  else
                                    Nothing
                                , if permissions.editSpace then
                                    Just Mensam.Space.Role.MkPermissionEditSpace

                                  else
                                    Nothing
                                , if permissions.createReservation then
                                    Just Mensam.Space.Role.MkPermissionCreateReservation

                                  else
                                    Nothing
                                , if permissions.cancelReservation then
                                    Just Mensam.Space.Role.MkPermissionCancelReservation

                                  else
                                    Nothing
                                ]
        }
    <|
        \result ->
            case result of
                Ok Mensam.Api.RoleEdit.Success ->
                    Messages
                        [ MessageEffect RefreshRole
                        , MessagePure ResetNewRole
                        ]

                Ok (Mensam.Api.RoleEdit.ErrorInsufficientPermission permission) ->
                    MessageEffect <| ReportError <| Mensam.Space.Role.errorInsufficientPermission permission

                Ok (Mensam.Api.RoleEdit.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Bad request body" <|
                                Mensam.Error.message error <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.RoleEdit.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <| ReportError <| Mensam.Error.http error


roleDelete :
    { jwt : Mensam.Auth.Bearer.Jwt
    , id : Mensam.Space.Role.Identifier
    , fallbackId : Mensam.Space.Role.Identifier
    }
    -> Cmd Message
roleDelete requestArgs =
    Mensam.Api.RoleDelete.request requestArgs <|
        \result ->
            case result of
                Ok Mensam.Api.RoleDelete.Success ->
                    MessageEffect ReturnToRoles

                Ok (Mensam.Api.RoleDelete.ErrorInsufficientPermission permission) ->
                    MessageEffect <| ReportError <| Mensam.Space.Role.errorInsufficientPermission permission

                Ok (Mensam.Api.RoleDelete.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Bad request body" <|
                                Mensam.Error.message error <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.RoleDelete.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <| ReportError <| Mensam.Error.http error
