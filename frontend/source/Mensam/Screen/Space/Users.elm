module Mensam.Screen.Space.Users exposing (..)

import Dict
import Element
import Element.Background
import Element.Border
import Element.Events.Pointer
import Element.Font
import Html.Attributes
import List.Extra
import Mensam.Api.PictureDownload
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
import Url.Builder


type alias Model =
    { spaceId : Mensam.Space.Identifier
    , spaceName : Mensam.Space.Name
    , yourRole :
        Maybe
            { accessibility : Mensam.Space.Role.Accessibility
            , id : Mensam.Space.Role.Identifier
            , name : Mensam.Space.Role.Name
            , permissions : Mensam.Space.Role.Permissions
            }
    , roles :
        List
            { id : Mensam.Space.Role.Identifier
            , name : Mensam.Space.Role.Name
            , accessibility : Mensam.Space.Role.Accessibility
            , permissions : Mensam.Space.Role.Permissions
            }
    , owner : Mensam.User.Identifier
    , users :
        Dict.Dict
            Int
            { user : Mensam.User.Identifier
            , role : Mensam.Space.Role.Identifier
            , info :
                Maybe
                    { name : Mensam.User.Name
                    }
            , profilePictureUrl : String
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


usersKey : Mensam.User.Identifier -> Int
usersKey (Mensam.User.MkIdentifierUnsafe n) =
    n


init : { id : Mensam.Space.Identifier } -> Model
init args =
    { spaceId = args.id
    , spaceName = Mensam.Space.MkName ""
    , yourRole = Nothing
    , roles = []
    , owner = Mensam.User.MkIdentifierUnsafe -1
    , users = Dict.empty
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
                            , color = Mensam.Element.Button.Gray
                            , enabled = True
                            , label = Element.text "Go back"
                            , message = Just <| MessageEffect ReturnToSpace
                            , size = Mensam.Element.Button.Medium
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
                    , Element.htmlAttribute <| Html.Attributes.style "contain" "size"
                    ]
                    { data = Dict.values model.users
                    , columns =
                        let
                            cell =
                                Element.el
                                    [ Element.height <| Element.px 40
                                    , Element.padding 10
                                    , Element.clip
                                    ]
                        in
                        [ { header =
                                Element.el
                                    [ Element.Background.color (Element.rgba 0 0 0 0.3)
                                    ]
                                <|
                                    cell <|
                                        Element.none
                          , width = Element.px 40
                          , view =
                                \n user ->
                                    Element.el
                                        [ Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetSelected Nothing
                                        , Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelected <| Just n
                                        , Element.Events.Pointer.onClick <| \_ -> MessagePure <| ChooseUser user.user
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
                                        Element.el
                                            [ Element.padding 5
                                            , Element.height <| Element.px 40
                                            ]
                                        <|
                                            Element.image
                                                [ Element.width <| Element.px 30
                                                , Element.height <| Element.px 30
                                                , Element.Border.rounded 5
                                                , Element.clip
                                                ]
                                                { src = user.profilePictureUrl
                                                , description = "Profile picture."
                                                }
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
                                            Element.text "ID"
                          , width = Element.px 40
                          , view =
                                \n user ->
                                    Element.el
                                        [ Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetSelected Nothing
                                        , Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelected <| Just n
                                        , Element.Events.Pointer.onClick <| \_ -> MessagePure <| ChooseUser user.user
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
                                        [ Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetSelected Nothing
                                        , Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelected <| Just n
                                        , Element.Events.Pointer.onClick <| \_ -> MessagePure <| ChooseUser user.user
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
                                        [ Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetSelected Nothing
                                        , Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelected <| Just n
                                        , Element.Events.Pointer.onClick <| \_ -> MessagePure <| ChooseUser user.user
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
                                [ case model.yourRole of
                                    Nothing ->
                                        Element.none

                                    Just yourRole ->
                                        if Mensam.Space.Role.permissionCheck Mensam.Space.Role.MkPermissionEditUser yourRole.permissions then
                                            Mensam.Element.Button.button <|
                                                Mensam.Element.Button.MkButton
                                                    { attributes = [ Element.alignRight ]
                                                    , color = Mensam.Element.Button.Red
                                                    , enabled = True
                                                    , label = Element.text "Kick User"
                                                    , message = Just <| MessagePure <| OpenDialogToKick popupModel.user
                                                    , size = Mensam.Element.Button.Medium
                                                    }

                                        else
                                            Element.none
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.alignRight ]
                                        , color = Mensam.Element.Button.Yellow
                                        , enabled = True
                                        , label = Element.text "Profile"
                                        , message = Just <| MessageEffect <| OpenPageToProfile popupModel.user
                                        , size = Mensam.Element.Button.Medium
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
                                        case Dict.get (usersKey popupModel.user) model.users of
                                            Nothing ->
                                                ""

                                            Just user ->
                                                case user.info of
                                                    Nothing ->
                                                        ""

                                                    Just info ->
                                                        Mensam.User.nameToString info.name
                                ]
                            , if Maybe.withDefault False <| Maybe.map (Mensam.Space.Role.permissionCheck Mensam.Space.Role.MkPermissionEditUser) <| Maybe.map (\yourRole -> yourRole.permissions) model.yourRole then
                                Element.column
                                    [ Element.spacing 20
                                    , Element.width Element.fill
                                    , Element.height Element.fill
                                    ]
                                    [ Element.paragraph
                                        [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Light300
                                        ]
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
                                        , Element.htmlAttribute <| Html.Attributes.style "contain" "size"
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
                                                            [ Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetSelectedRole Nothing
                                                            , Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelectedRole <| Just n
                                                            , Element.Events.Pointer.onClick <| \_ -> MessagePure <| ChooseNewRole role.id
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
                                                            [ Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetSelectedRole Nothing
                                                            , Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelectedRole <| Just n
                                                            , Element.Events.Pointer.onClick <| \_ -> MessagePure <| ChooseNewRole role.id
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
                                                , enabled = True
                                                , label = Element.text "Abort"
                                                , message = Just <| MessagePure <| CloseDialogToEditUser
                                                , size = Mensam.Element.Button.Medium
                                                }
                                        , Mensam.Element.Button.button <|
                                            Mensam.Element.Button.MkButton
                                                { attributes = [ Element.width Element.fill ]
                                                , color = Mensam.Element.Button.Blue
                                                , enabled = True
                                                , label = Element.text "Change Role"
                                                , message =
                                                    case popupModel.role of
                                                        Nothing ->
                                                            Nothing

                                                        Just role ->
                                                            Just <| MessageEffect <| SubmitEditUser { user = popupModel.user, role = role }
                                                , size = Mensam.Element.Button.Medium
                                                }
                                        ]
                                    ]

                              else
                                Element.row
                                    [ Element.width Element.fill
                                    , Element.spacing 10
                                    , Element.alignBottom
                                    ]
                                    [ Mensam.Element.Button.button <|
                                        Mensam.Element.Button.MkButton
                                            { attributes = [ Element.width Element.fill ]
                                            , color = Mensam.Element.Button.Gray
                                            , enabled = True
                                            , label = Element.text "Go back"
                                            , message = Just <| MessagePure <| CloseDialogToEditUser
                                            , size = Mensam.Element.Button.Medium
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
                                        case Dict.get (usersKey popupModel.user) model.users of
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
                                [ Element.text "Kicking a user will remove all permissions for this user."
                                ]
                            , Element.paragraph
                                []
                                [ Element.text "Reservations will remain untouched."
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
                                        , message = Just <| MessagePure <| CloseDialogToKick
                                        , size = Mensam.Element.Button.Medium
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Red
                                        , enabled = True
                                        , label = Element.text "Kick User"
                                        , message = Just <| MessageEffect <| SubmitKickUser { user = popupModel.user }
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
    = SetSpaceInfo
        { name : Mensam.Space.Name
        , yourRole :
            Maybe
                { accessibility : Mensam.Space.Role.Accessibility
                , id : Mensam.Space.Role.Identifier
                , name : Mensam.Space.Role.Name
                , permissions : Mensam.Space.Role.Permissions
                }
        , roles :
            List
                { id : Mensam.Space.Role.Identifier
                , name : Mensam.Space.Role.Name
                , accessibility : Mensam.Space.Role.Accessibility
                , permissions : Mensam.Space.Role.Permissions
                }
        , owner : Mensam.User.Identifier
        }
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
    | SetUserProfilePicture
        { id : Mensam.User.Identifier
        , picture : { url : String }
        }
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
        SetSpaceInfo info ->
            { model
                | spaceName = info.name
                , yourRole = info.yourRole
                , roles = info.roles
                , owner = info.owner
            }

        SetUserIds users ->
            { model
                | users =
                    Dict.fromList <|
                        List.map
                            (\user ->
                                ( usersKey user.user
                                , { user = user.user
                                  , role = user.role
                                  , info = Nothing
                                  , profilePictureUrl =
                                        Url.Builder.absolute
                                            [ "static"
                                            , "default-profile-picture.jpeg"
                                            ]
                                            []
                                  }
                                )
                            )
                            users
            }

        SetUserInfo userWithInfo ->
            { model
                | users =
                    Dict.update (usersKey userWithInfo.id)
                        (Maybe.map (\user -> { user | info = Just userWithInfo.info }))
                        model.users
            }

        SetUserProfilePicture userWithPicture ->
            { model
                | users =
                    Dict.update (usersKey userWithPicture.id)
                        (Maybe.map (\user -> { user | profilePictureUrl = userWithPicture.picture.url }))
                        model.users
            }

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
    | GetProfilePicture Mensam.User.Identifier
    | SubmitEditUser { user : Mensam.User.Identifier, role : Mensam.Space.Role.Identifier }
    | SubmitKickUser { user : Mensam.User.Identifier }
    | ReturnToSpace
    | OpenPageToProfile Mensam.User.Identifier


spaceView : { jwt : Mensam.Auth.Bearer.Jwt, yourUserId : Mensam.User.Identifier } -> Mensam.Space.Identifier -> Cmd Message
spaceView auth id =
    Mensam.Api.SpaceView.request { jwt = auth.jwt, yourUserId = auth.yourUserId, id = id } <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceView.Success view) ->
                    Messages <|
                        [ MessagePure <|
                            SetSpaceInfo
                                { name = view.name
                                , yourRole = view.yourRole
                                , roles = view.roles
                                , owner = view.owner
                                }
                        , MessagePure <| SetUserIds view.users
                        ]
                            ++ List.map (\user -> MessageEffect <| GetProfilePicture user.user)
                                view.users
                            ++ List.map (\user -> MessageEffect <| GetProfile user.user)
                                view.users

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


profilePicture : Mensam.Auth.Bearer.Jwt -> Mensam.User.Identifier -> Cmd Message
profilePicture jwt userId =
    Mensam.Api.PictureDownload.request
        { jwt = jwt
        , user = userId
        }
    <|
        \response ->
            case response of
                Ok (Mensam.Api.PictureDownload.Success picture) ->
                    MessagePure <| SetUserProfilePicture { id = userId, picture = picture }

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to request profile picture" <|
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
