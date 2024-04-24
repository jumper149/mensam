module Mensam.Screen.Space.Roles exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Element.Input
import Html.Attributes
import Html.Events
import Mensam.Api.RoleCreate
import Mensam.Api.SpaceView
import Mensam.Auth.Bearer
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
    }


init : { id : Mensam.Space.Identifier } -> Model
init args =
    { spaceId = args.id
    , spaceName = Mensam.Space.MkName ""
    , roles = []
    , selected = Nothing
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
                            Element.text "Edit Roles"
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
                                        , Element.Events.onClick <| MessagePure <| ChooseRole role.id
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
                                        , Element.Events.onClick <| MessagePure <| ChooseRole role.id
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
                                        , Element.Events.onClick <| MessagePure <| ChooseRole role.id
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
                                        , Element.Events.onClick <| MessagePure <| ChooseRole role.id
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
        , popup = Nothing
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
    | ChooseRole Mensam.Space.Role.Identifier


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetSpaceName name ->
            { model | spaceName = name }

        SetRoles roles ->
            { model | roles = roles }

        SetSelected n ->
            { model | selected = n }

        -- TODO
        ChooseRole _ ->
            model


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshRoles


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
                    Messages
                        [ MessagePure <| SetRoles view.roles
                        , MessagePure <| SetSpaceName view.name
                        ]

                Ok Mensam.Api.SpaceView.ErrorInsufficientPermission ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Insufficient permission to view this space" <|
                                Mensam.Error.undefined

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
    , permissions : Mensam.Space.Role.Permissions
    }
    -> Cmd Message
roleCreate requestArgs =
    Mensam.Api.RoleCreate.request requestArgs <|
        \result ->
            case result of
                Ok (Mensam.Api.RoleCreate.Success _) ->
                    MessageEffect RefreshRoles

                Ok Mensam.Api.RoleCreate.ErrorInsufficientPermission ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Insufficient permission to delete this space" <|
                                Mensam.Error.undefined

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
