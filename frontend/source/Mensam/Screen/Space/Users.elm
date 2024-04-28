module Mensam.Screen.Space.Users exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Element.Input
import Html.Attributes
import List.Extra
import Mensam.Api.Profile
import Mensam.Api.RoleCreate
import Mensam.Api.SpaceView
import Mensam.Auth.Bearer
import Mensam.Element.Color
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
    = PopupEditUser Mensam.User.Identifier


init : { id : Mensam.Space.Identifier } -> Model
init args =
    { spaceId = args.id
    , spaceName = Mensam.Space.MkName ""
    , roles = []
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
                    , Element.spacing 30
                    ]
                    [ Element.column
                        [ Element.spacing 20
                        , Element.width Element.fill
                        , Element.height Element.fill
                        ]
                        [ Element.el
                            [ Element.Font.size 30
                            , Element.Font.hairline
                            ]
                          <|
                            Element.text "Users"
                        ]
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
                                        , Element.Events.onClick <| MessageEffect <| ReportError Mensam.Error.undefined -- TODO
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
                                        , Element.Events.onClick <| MessageEffect <| ReportError Mensam.Error.undefined -- TODO
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
                          , width = Element.px 80
                          , view =
                                \n user ->
                                    Element.el
                                        [ Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
                                        , Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                                        , Element.Events.onClick <| MessageEffect <| ReportError Mensam.Error.undefined -- TODO
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
                                                case List.Extra.find (\role -> role.id == user.role) model.roles of
                                                    Nothing ->
                                                        Element.text <|
                                                            Mensam.Space.Role.identifierToString user.role

                                                    Just role ->
                                                        Element.text <|
                                                            Mensam.Space.Role.nameToString role.name
                          }
                        ]
                    }
                ]
        , popup =
            case model.popup of
                Nothing ->
                    Nothing

                Just (PopupEditUser popupModel) ->
                    Nothing
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
    | SetSelected (Maybe Int)


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

        SetSelected n ->
            { model | selected = n }


type MessageEffect
    = ReportError Mensam.Error.Error
    | Refresh
    | GetProfile Mensam.User.Identifier


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
