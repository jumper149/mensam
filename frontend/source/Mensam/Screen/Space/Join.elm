module Mensam.Screen.Space.Join exposing (..)

import Element
import Element.Background
import Element.Events.Pointer
import Element.Font
import Element.Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import List.Extra
import Mensam.Api.SpaceJoin
import Mensam.Api.SpaceView
import Mensam.Auth.Bearer
import Mensam.Element.Button
import Mensam.Element.Color
import Mensam.Element.Screen
import Mensam.Error
import Mensam.NameOrIdentifier
import Mensam.Space
import Mensam.Space.Role
import Mensam.Time
import Mensam.User


type alias Model =
    { spaceId : Mensam.Space.Identifier
    , spaceName : Mensam.Space.Name
    , roles :
        List
            { accessibility : Mensam.Space.Role.Accessibility
            , id : Mensam.Space.Role.Identifier
            , name : Mensam.Space.Role.Name
            , permissions : Mensam.Space.Role.Permissions
            }
    , timezone : Mensam.Time.Timezone
    , visibility : Mensam.Space.Visibility
    , yourRole :
        Maybe
            { accessibility : Mensam.Space.Role.Accessibility
            , id : Mensam.Space.Role.Identifier
            , name : Mensam.Space.Role.Name
            , permissions : Mensam.Space.Role.Permissions
            }
    , roleIdSelected : Maybe Mensam.Space.Role.Identifier
    , password : Maybe String
    }


init : { spaceId : Mensam.Space.Identifier, roleIdSelected : Maybe Mensam.Space.Role.Identifier, password : Maybe String } -> Model
init args =
    { spaceId = args.spaceId
    , spaceName = Mensam.Space.MkName ""
    , roles = []
    , timezone = Mensam.Time.timezoneEtcUtc
    , visibility = Mensam.Space.MkVisibilityHidden
    , yourRole = Nothing
    , roleIdSelected = args.roleIdSelected
    , password = args.password
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
                            Element.text "Join Space"
                        , let
                            roleElement =
                                \role ->
                                    Element.el
                                        [ Element.width Element.fill
                                        , Element.padding 15
                                        , Element.Background.color <|
                                            if Just role.id == model.roleIdSelected then
                                                Element.rgba 0 0 0 0.2

                                            else
                                                Element.rgba 0 0 0 0
                                        , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                        , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                                        , Element.mouseOver
                                            [ Element.Background.color <| Element.rgba 0 0 0 0.3
                                            ]
                                        , Element.Events.Pointer.onClick <| \_ -> MessagePure <| SetRoleToJoin role.id
                                        ]
                                    <|
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            ]
                                        <|
                                            Element.text <|
                                                Mensam.Space.Role.nameToString role.name
                          in
                          Element.column
                            [ Element.width Element.fill
                            ]
                          <|
                            List.map roleElement model.roles
                        , let
                            roleSelectedPasswordRequired =
                                case List.Extra.find (\role -> Just role.id == model.roleIdSelected) model.roles of
                                    Nothing ->
                                        False

                                    Just roleSelected ->
                                        case roleSelected.accessibility of
                                            Mensam.Space.Role.MkAccessibilityJoinable ->
                                                False

                                            Mensam.Space.Role.MkAccessibilityJoinableWithPassword ->
                                                True

                                            Mensam.Space.Role.MkAccessibilityInaccessible ->
                                                False
                          in
                          if roleSelectedPasswordRequired then
                            Element.Input.currentPassword
                                [ onEnter <| MessageEffect <| SubmitJoin
                                , Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                                ]
                                { onChange =
                                    \str ->
                                        MessagePure <|
                                            EnterSpacePasswordToJoin <|
                                                case str of
                                                    "" ->
                                                        Nothing

                                                    actualStr ->
                                                        Just actualStr
                                , text = Maybe.withDefault "" model.password
                                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Password"
                                , label = Element.Input.labelAbove [] <| Element.text "Password"
                                , show = False
                                }

                          else
                            Element.none
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
                                    , label = Element.text "Submit"
                                    , message =
                                        case model.roleIdSelected of
                                            -- TODO: Show a help text in this case.
                                            Nothing ->
                                                Nothing

                                            Just _ ->
                                                Just <| MessageEffect <| SubmitJoin
                                    , size = Mensam.Element.Button.Medium
                                    }
                            ]
                        ]
                    ]
                ]
        , popup = Nothing
        , closePopup = MessagePure ClosePopup
        }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect
    | Messages (List Message)


type MessagePure
    = SetSpaceInfo
        { id : Mensam.Space.Identifier
        , name : Mensam.Space.Name
        , roles :
            List
                { accessibility : Mensam.Space.Role.Accessibility
                , id : Mensam.Space.Role.Identifier
                , name : Mensam.Space.Role.Name
                , permissions : Mensam.Space.Role.Permissions
                }
        , timezone : Mensam.Time.Timezone
        , visibility : Mensam.Space.Visibility
        , yourRole :
            Maybe
                { accessibility : Mensam.Space.Role.Accessibility
                , id : Mensam.Space.Role.Identifier
                , name : Mensam.Space.Role.Name
                , permissions : Mensam.Space.Role.Permissions
                }
        }
    | SetRoleToJoin Mensam.Space.Role.Identifier
    | EnterSpacePasswordToJoin (Maybe String)
    | ClosePopup


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetSpaceInfo view ->
            { model
                | spaceId = view.id
                , spaceName = view.name
                , roles =
                    List.filter
                        (\role ->
                            case role.accessibility of
                                Mensam.Space.Role.MkAccessibilityJoinable ->
                                    True

                                Mensam.Space.Role.MkAccessibilityJoinableWithPassword ->
                                    True

                                Mensam.Space.Role.MkAccessibilityInaccessible ->
                                    False
                        )
                        view.roles
                , timezone = view.timezone
                , visibility = view.visibility
                , yourRole = view.yourRole
            }

        SetRoleToJoin roleId ->
            { model | roleIdSelected = Just roleId, password = Nothing }

        EnterSpacePasswordToJoin password ->
            { model | password = password }

        ClosePopup ->
            model


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshSpace
    | SubmitJoin
    | JoinedSuccessfully


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


spaceView : { jwt : Mensam.Auth.Bearer.Jwt, yourUserId : Mensam.User.Identifier } -> Model -> Cmd Message
spaceView auth model =
    Mensam.Api.SpaceView.request { jwt = auth.jwt, yourUserId = auth.yourUserId, id = model.spaceId } <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceView.Success view) ->
                    case view.yourRole of
                        Nothing ->
                            MessagePure <|
                                SetSpaceInfo
                                    { id = view.id
                                    , name = view.name
                                    , roles = view.roles
                                    , timezone = view.timezone
                                    , visibility = view.visibility
                                    , yourRole = view.yourRole
                                    }

                        Just _ ->
                            MessageEffect JoinedSuccessfully

                Ok (Mensam.Api.SpaceView.Success403Restricted view) ->
                    Messages <|
                        [ MessagePure <|
                            SetSpaceInfo
                                { id = view.id
                                , name = view.name
                                , roles = view.roles
                                , timezone = view.timezone
                                , visibility = view.visibility
                                , yourRole = view.yourRole
                                }
                        ]

                Ok Mensam.Api.SpaceView.ErrorSpaceNotFound ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to get space information" <|
                                Mensam.Error.message "Space not found" <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.SpaceView.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to get space information" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.SpaceView.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to get space information" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to get space information" <|
                                Mensam.Error.http error


spaceJoin : Mensam.Auth.Bearer.Jwt -> Mensam.Space.Identifier -> Mensam.Space.Role.Identifier -> Maybe String -> Cmd Message
spaceJoin jwt spaceId roleId password =
    Mensam.Api.SpaceJoin.request { jwt = jwt, role = Mensam.NameOrIdentifier.Identifier roleId, space = Mensam.NameOrIdentifier.Identifier spaceId, password = password } <|
        \result ->
            case result of
                Ok Mensam.Api.SpaceJoin.Success ->
                    MessageEffect JoinedSuccessfully

                Ok Mensam.Api.SpaceJoin.ErrorInaccessible ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to join" <|
                                Mensam.Error.message "Bad request" <|
                                    Mensam.Error.message "Inaccessible" <|
                                        Mensam.Error.undefined

                Ok Mensam.Api.SpaceJoin.ErrorWrongPassword ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to join" <|
                                Mensam.Error.message "Bad request" <|
                                    Mensam.Error.message "Wrong password" <|
                                        Mensam.Error.undefined

                Ok Mensam.Api.SpaceJoin.ErrorSpaceNotFound ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to join" <|
                                Mensam.Error.message "Space not found" <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.SpaceJoin.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Bad request body" <|
                                Mensam.Error.message error <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.SpaceJoin.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <| ReportError <| Mensam.Error.http error
