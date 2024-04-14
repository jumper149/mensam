module Mensam.Screen.Space.Join exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Element.Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import List.Extra
import Mensam.Api.SpaceJoin
import Mensam.Api.SpaceView
import Mensam.Auth.Bearer
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Element.Screen
import Mensam.Error
import Mensam.NameOrIdentifier
import Mensam.Space
import Mensam.Space.Role
import Mensam.Time


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
    , timezoneIdentifier : Mensam.Time.TimezoneIdentifier
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
    , timezoneIdentifier = Mensam.Time.MkTimezoneIdentifier "Etc/UTC"
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
                                        , Element.Events.onClick <| MessagePure <| SetRoleToJoin role.id
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
                                , Element.Font.color Mensam.Element.Color.dark.black
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
                            [ Element.Input.button
                                ([ Element.width Element.fill
                                 , Element.padding 10
                                 ]
                                    ++ (case model.roleIdSelected of
                                            Nothing ->
                                                [ Element.Background.color Mensam.Element.Color.dark.white
                                                , Element.Font.color Mensam.Element.Color.dark.black
                                                ]

                                            Just _ ->
                                                [ Element.Background.color Mensam.Element.Color.bright.yellow
                                                , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.green ]
                                                , Element.Font.color Mensam.Element.Color.dark.black
                                                ]
                                       )
                                )
                                { onPress =
                                    case model.roleIdSelected of
                                        -- TODO: Show a help text in this case.
                                        Nothing ->
                                            Nothing

                                        Just _ ->
                                            Just <| MessageEffect <| SubmitJoin
                                , label =
                                    Element.el
                                        [ Element.centerX
                                        , Element.centerY
                                        , Element.Font.family [ Mensam.Element.Font.condensed ]
                                        , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                        ]
                                    <|
                                        Element.text "Submit"
                                }
                            ]
                        ]
                    ]
                ]
        , popup = Nothing
        }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect


type MessagePure
    = SetSpaceInfo Mensam.Space.SpaceView
    | SetRoleToJoin Mensam.Space.Role.Identifier
    | EnterSpacePasswordToJoin (Maybe String)


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetSpaceInfo (Mensam.Space.MkSpaceView space) ->
            { model
                | spaceId = space.id
                , spaceName = space.name
                , roles = space.roles
                , timezoneIdentifier = space.timezone
                , visibility = space.visibility
                , yourRole = space.yourRole
            }

        SetRoleToJoin roleId ->
            { model | roleIdSelected = Just roleId, password = Nothing }

        EnterSpacePasswordToJoin password ->
            { model | password = password }


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


spaceView : Mensam.Auth.Bearer.Jwt -> Model -> Cmd Message
spaceView jwt model =
    Mensam.Api.SpaceView.request { jwt = jwt, id = model.spaceId } <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceView.Success value) ->
                    MessagePure <| SetSpaceInfo value.space

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


spaceJoin : Mensam.Auth.Bearer.Jwt -> Mensam.Space.Identifier -> Mensam.Space.Role.Identifier -> Maybe String -> Cmd Message
spaceJoin jwt spaceId roleId password =
    Mensam.Api.SpaceJoin.request { jwt = jwt, role = Mensam.NameOrIdentifier.Identifier roleId, space = Mensam.NameOrIdentifier.Identifier spaceId, password = password } <|
        \result ->
            case result of
                Ok Mensam.Api.SpaceJoin.Success ->
                    MessageEffect JoinedSuccessfully

                Ok Mensam.Api.SpaceJoin.ErrorWrongPassword ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to join" <|
                                Mensam.Error.message "Bad request" <|
                                    Mensam.Error.message "Wrong password" <|
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
