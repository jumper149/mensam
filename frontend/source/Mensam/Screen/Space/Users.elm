module Mensam.Screen.Space.Users exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Html.Attributes
import List.Extra
import Mensam.Api.Profile
import Mensam.Api.SpaceKick
import Mensam.Api.SpaceUserRole
import Mensam.Api.SpaceView
import Mensam.Auth.Bearer
import Mensam.Element.Button
import Mensam.Element.Font
import Mensam.Element.Screen
import Mensam.Error
import Mensam.Space
import Mensam.Space.Role
import Mensam.User


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
    , owner : Mensam.User.Identifier
    , users :
        List
            { user : Mensam.User.Identifier
            , role : Mensam.Space.Role.Identifier
            , info :
                Maybe
                    { name : Mensam.User.Name
                    }
            }
    , selected : Maybe Int
    , popup : Maybe PopupModel
    }


type PopupModel
    = PopupEditUser
        { user : Mensam.User.Identifier
        , role : Maybe Mensam.Space.Role.Identifier
        , selected : Maybe Int
        }
    | PopupKickUser
        { user : Mensam.User.Identifier
        }


init : { id : Mensam.Space.Identifier } -> Model
init args =
    { spaceId = args.id
    , spaceName = Mensam.Space.MkName ""
    , roles = []
    , owner = Mensam.User.MkIdentifierUnsafe -1
    , users = []
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
                    , Element.spacing 10
                    ]
                    [ Element.el
                        [ Element.Font.size 30
                        , Element.Font.hairline
                        , Element.alignLeft
                        , Element.centerY
                        ]
                      <|
                        Element.text "Users"
                    , Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = [ Element.alignRight, Element.centerY ]
                            , color = Mensam.Element.Button.Yellow
                            , message = Just <| MessageEffect ReturnToSpace
                            , text = "Go back"
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
                    { data = model.users
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
                          , width = Element.px 40
                          , view =
                                \n user ->
                                    Element.el
                                        [ Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
                                        , Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                                        , Element.Events.onClick <| MessagePure <| ChooseUser user.user
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
                                                    Mensam.User.identifierToString user.user
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
                                \n user ->
                                    Element.el
                                        [ Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
                                        , Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                                        , Element.Events.onClick <| MessagePure <| ChooseUser user.user
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
                                                case user.info of
                                                    Nothing ->
                                                        Element.text ""

                                                    Just info ->
                                                        Element.text <|
                                                            Mensam.User.nameToString info.name
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
                                            Element.text "Role"
                          , width = Element.px 120
                          , view =
                                \n user ->
                                    Element.el
                                        [ Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
                                        , Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                                        , Element.Events.onClick <| MessagePure <| ChooseUser user.user
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
                                                    let
                                                        roleText =
                                                            case List.Extra.find (\role -> role.id == user.role) model.roles of
                                                                Nothing ->
                                                                    Mensam.Space.Role.identifierToString user.role

                                                                Just role ->
                                                                    Mensam.Space.Role.nameToString role.name
                                                    in
                                                    if user.user == model.owner then
                                                        roleText ++ " (Owner)"

                                                    else
                                                        roleText
                          }
                        ]
                    }
                ]
        , popup =
            case model.popup of
                Nothing ->
                    Nothing

                Just (PopupEditUser popupModel) ->
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
                                Element.text "Change Role"
                            , Element.row
                                [ Element.width Element.fill
                                , Element.spacing 10
                                ]
                                [ Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.alignRight ]
                                        , color = Mensam.Element.Button.Red
                                        , message = Just <| MessagePure <| OpenDialogToKick popupModel.user
                                        , text = "Kick User"
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.alignRight ]
                                        , color = Mensam.Element.Button.Yellow
                                        , message = Just <| MessageEffect <| OpenPageToProfile popupModel.user
                                        , text = "Profile"
                                        }
                                ]
                            , Element.row
                                [ Element.spacing 20
                                , Element.width Element.fill
                                , Element.height <| Element.px 25
                                ]
                                [ Element.el [] <| Element.text "User:"
                                , Element.el [] <|
                                    Element.text <|
                                        case List.Extra.find (\user -> user.user == popupModel.user) model.users of
                                            Nothing ->
                                                ""

                                            Just user ->
                                                case user.info of
                                                    Nothing ->
                                                        ""

                                                    Just info ->
                                                        Mensam.User.nameToString info.name
                                ]
                            , Element.paragraph
                                []
                                [ Element.text "Choose a new role for this user."
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
                                      , width = Element.px 40
                                      , view =
                                            \n role ->
                                                Element.el
                                                    [ Element.Events.onMouseLeave <| MessagePure <| SetSelectedRole Nothing
                                                    , Element.Events.onMouseEnter <| MessagePure <| SetSelectedRole <| Just n
                                                    , Element.Events.onClick <| MessagePure <| ChooseNewRole role.id
                                                    , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                                    , let
                                                        alpha =
                                                            case popupModel.selected of
                                                                Nothing ->
                                                                    0.2

                                                                Just m ->
                                                                    if m == n then
                                                                        0.4

                                                                    else
                                                                        0.2
                                                      in
                                                      if popupModel.role == Just role.id then
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
                                                    [ Element.Events.onMouseLeave <| MessagePure <| SetSelectedRole Nothing
                                                    , Element.Events.onMouseEnter <| MessagePure <| SetSelectedRole <| Just n
                                                    , Element.Events.onClick <| MessagePure <| ChooseNewRole role.id
                                                    , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                                    , let
                                                        alpha =
                                                            case popupModel.selected of
                                                                Nothing ->
                                                                    0.2

                                                                Just m ->
                                                                    if m == n then
                                                                        0.4

                                                                    else
                                                                        0.2
                                                      in
                                                      if popupModel.role == Just role.id then
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
                                        , message = Just <| MessagePure <| CloseDialogToEditUser
                                        , text = "Abort"
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Yellow
                                        , message =
                                            case popupModel.role of
                                                Nothing ->
                                                    Nothing

                                                Just role ->
                                                    Just <| MessageEffect <| SubmitEditUser { user = popupModel.user, role = role }
                                        , text = "Change Role"
                                        }
                                ]
                            ]

                Just (PopupKickUser popupModel) ->
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
                                Element.text "Kick User"
                            , Element.row
                                [ Element.spacing 20
                                , Element.width Element.fill
                                , Element.height <| Element.px 25
                                ]
                                [ Element.el [] <| Element.text "User:"
                                , Element.el [] <|
                                    Element.text <|
                                        case List.Extra.find (\user -> user.user == popupModel.user) model.users of
                                            Nothing ->
                                                ""

                                            Just user ->
                                                case user.info of
                                                    Nothing ->
                                                        ""

                                                    Just info ->
                                                        Mensam.User.nameToString info.name
                                ]
                            , Element.paragraph
                                []
                                [ Element.text "Do you really want to remove this user from the space?"
                                ]
                            , Element.paragraph
                                []
                                [ Element.text "Kicking a user will also delete all reservations associated with this user."
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
                                        , message = Just <| MessagePure <| CloseDialogToKick
                                        , text = "Abort"
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Red
                                        , message = Just <| MessageEffect <| SubmitKickUser { user = popupModel.user }
                                        , text = "Kick User"
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
    = SetSpaceName Mensam.Space.Name
    | SetUserIds
        (List
            { user : Mensam.User.Identifier
            , role : Mensam.Space.Role.Identifier
            }
        )
    | SetUserInfo
        { id : Mensam.User.Identifier
        , info :
            { name : Mensam.User.Name
            }
        }
    | SetRoles
        (List
            { id : Mensam.Space.Role.Identifier
            , name : Mensam.Space.Role.Name
            , accessibility : Mensam.Space.Role.Accessibility
            , permissions : Mensam.Space.Role.Permissions
            }
        )
    | SetOwner Mensam.User.Identifier
    | SetSelected (Maybe Int)
    | ClosePopup
    | ChooseUser Mensam.User.Identifier
    | SetSelectedRole (Maybe Int)
    | ChooseNewRole Mensam.Space.Role.Identifier
    | CloseDialogToEditUser
    | OpenDialogToKick Mensam.User.Identifier
    | CloseDialogToKick


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetSpaceName name ->
            { model | spaceName = name }

        SetUserIds users ->
            { model | users = List.map (\user -> { user = user.user, role = user.role, info = Nothing }) users }

        SetUserInfo userWithInfo ->
            { model
                | users =
                    List.filterMap
                        (\user ->
                            if user.user == userWithInfo.id then
                                Just { user | info = Just userWithInfo.info }

                            else
                                Just user
                        )
                        model.users
            }

        SetRoles roles ->
            { model | roles = roles }

        SetOwner owner ->
            { model | owner = owner }

        SetSelected n ->
            { model | selected = n }

        ClosePopup ->
            { model | popup = Nothing }

        ChooseUser userId ->
            { model | popup = Just <| PopupEditUser { user = userId, role = Nothing, selected = Nothing } }

        SetSelectedRole maybeN ->
            case model.popup of
                Nothing ->
                    model

                Just (PopupEditUser popupModel) ->
                    { model | popup = Just <| PopupEditUser { popupModel | selected = maybeN } }

                Just (PopupKickUser popupModel) ->
                    { model | popup = Just <| PopupKickUser popupModel }

        ChooseNewRole roleId ->
            case model.popup of
                Nothing ->
                    model

                Just (PopupEditUser popupModel) ->
                    { model | popup = Just <| PopupEditUser { popupModel | role = Just roleId } }

                Just (PopupKickUser popupModel) ->
                    { model | popup = Just <| PopupKickUser popupModel }

        CloseDialogToEditUser ->
            { model | popup = Nothing }

        OpenDialogToKick userId ->
            { model | popup = Just <| PopupKickUser { user = userId } }

        CloseDialogToKick ->
            { model | popup = Nothing }


type MessageEffect
    = ReportError Mensam.Error.Error
    | Refresh
    | GetProfile Mensam.User.Identifier
    | SubmitEditUser { user : Mensam.User.Identifier, role : Mensam.Space.Role.Identifier }
    | SubmitKickUser { user : Mensam.User.Identifier }
    | ReturnToSpace
    | OpenPageToProfile Mensam.User.Identifier


spaceView : Mensam.Auth.Bearer.Jwt -> Mensam.Space.Identifier -> Cmd Message
spaceView jwt id =
    Mensam.Api.SpaceView.request { jwt = jwt, id = id } <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceView.Success value) ->
                    let
                        (Mensam.Space.MkSpaceView view) =
                            value.space
                    in
                    Messages <|
                        [ MessagePure <| SetUserIds view.users
                        , MessagePure <| SetRoles view.roles
                        , MessagePure <| SetSpaceName view.name
                        , MessagePure <| SetOwner view.owner
                        ]
                            ++ List.map (\user -> MessageEffect <| GetProfile user.user) view.users

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
                    MessagePure <| SetUserInfo { id = userId, info = { name = body.name } }

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


editUserRole : Mensam.Auth.Bearer.Jwt -> Mensam.Space.Identifier -> Mensam.User.Identifier -> Mensam.Space.Role.Identifier -> Cmd Message
editUserRole jwt spaceId userId roleId =
    Mensam.Api.SpaceUserRole.request
        { jwt = jwt
        , space = spaceId
        , user = userId
        , role = roleId
        }
    <|
        \response ->
            case response of
                Ok Mensam.Api.SpaceUserRole.Success ->
                    Messages
                        [ MessagePure CloseDialogToEditUser
                        , MessageEffect Refresh
                        ]

                Ok (Mensam.Api.SpaceUserRole.ErrorInsufficientPermission permission) ->
                    MessageEffect <| ReportError <| Mensam.Space.Role.errorInsufficientPermission permission

                Ok (Mensam.Api.SpaceUserRole.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to edit role" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.SpaceUserRole.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to edit role" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to edit role" <|
                                Mensam.Error.http error


kickUser : Mensam.Auth.Bearer.Jwt -> Mensam.Space.Identifier -> Mensam.User.Identifier -> Cmd Message
kickUser jwt spaceId userId =
    Mensam.Api.SpaceKick.request
        { jwt = jwt
        , space = spaceId
        , user = userId
        }
    <|
        \response ->
            case response of
                Ok Mensam.Api.SpaceKick.Success ->
                    Messages
                        [ MessagePure CloseDialogToKick
                        , MessageEffect Refresh
                        ]

                Ok (Mensam.Api.SpaceKick.ErrorInsufficientPermission permission) ->
                    MessageEffect <| ReportError <| Mensam.Space.Role.errorInsufficientPermission permission

                Ok (Mensam.Api.SpaceKick.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to kick user" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.SpaceKick.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to kick user" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to kick user" <|
                                Mensam.Error.http error
