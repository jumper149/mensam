module Mensam.Screen.Space.Settings exposing (..)

import Element
import Element.Background
import Element.Font
import Element.Input
import Html.Events
import Json.Decode as Decode
import Mensam.Api.SpaceDelete
import Mensam.Api.SpaceEdit
import Mensam.Auth.Bearer
import Mensam.Element.Button
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Element.Screen
import Mensam.Error
import Mensam.Space
import Mensam.Space.Role
import Mensam.Time


type alias Model =
    { id : Mensam.Space.Identifier
    , old :
        { name : Mensam.Space.Name
        , timezone : Mensam.Time.TimezoneIdentifier
        , visibility : Mensam.Space.Visibility
        }
    , new :
        { name : Maybe Mensam.Space.Name
        , timezone : Maybe Mensam.Time.TimezoneIdentifier
        , visibility : Maybe Mensam.Space.Visibility
        }
    , popup : Maybe PopupModel
    }


type PopupModel
    = PopupDeleteSpace


init : { id : Mensam.Space.Identifier } -> Model
init args =
    { id = args.id
    , old =
        { name = Mensam.Space.MkName ""
        , timezone = Mensam.Time.MkTimezoneIdentifier "Etc/UTC"
        , visibility = Mensam.Space.MkVisibilityVisible
        }
    , new =
        { name = Nothing
        , timezone = Nothing
        , visibility = Nothing
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
                            , label = Element.text "Delete"
                            , message = Just <| MessagePure OpenDialogToDeleteSpace
                            , size = Mensam.Element.Button.Medium
                            }
                    , Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = [ Element.alignRight, Element.centerY ]
                            , color = Mensam.Element.Button.Yellow
                            , label = Element.text "Roles"
                            , message = Just <| MessageEffect OpenPageToRoles
                            , size = Mensam.Element.Button.Medium
                            }
                    , Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = [ Element.alignRight, Element.centerY ]
                            , color = Mensam.Element.Button.Gray
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
                                        , label = Element.text "Edit Name"
                                        , message = Just <| MessagePure <| EnterName <| Just <| model.old.name
                                        , size = Mensam.Element.Button.Medium
                                        }

                            Just name ->
                                Element.Input.text
                                    [ -- onEnter <| MessageEffect SubmitCreate
                                      Element.Font.color Mensam.Element.Color.dark.black
                                    ]
                                    { onChange = MessagePure << EnterName << Just << Mensam.Space.MkName
                                    , text = Mensam.Space.nameToString name
                                    , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Name"
                                    , label = Element.Input.labelHidden "Name"
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
                        [ Element.el [] <| Element.text "Current Timezone:"
                        , Element.el [] <| Element.text <| Mensam.Time.unTimezoneIdentifier model.old.timezone
                        ]
                    , Element.el
                        [ Element.paddingXY 30 5
                        , Element.width Element.fill
                        , Element.height <| Element.px 115
                        ]
                      <|
                        case model.new.timezone of
                            Nothing ->
                                Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Yellow
                                        , label = Element.text "Edit Timezone"
                                        , message = Just <| MessagePure <| SetTimezone <| Just model.old.timezone
                                        , size = Mensam.Element.Button.Medium
                                        }

                            Just timezone ->
                                Element.Input.text
                                    [ -- onEnter <| MessageEffect SubmitCreate
                                      Element.Font.color Mensam.Element.Color.dark.black
                                    ]
                                    { onChange = MessagePure << SetTimezone << Just << Mensam.Time.MkTimezoneIdentifier
                                    , text = Mensam.Time.unTimezoneIdentifier timezone
                                    , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Timezone"
                                    , label = Element.Input.labelHidden "Timezone"
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
                        [ Element.el [] <| Element.text "Current Visibility:"
                        , Element.el [] <| Element.text <| Mensam.Space.visibilityToString model.old.visibility
                        ]
                    , Element.el
                        [ Element.paddingXY 30 5
                        , Element.width Element.fill
                        , Element.height <| Element.px 115
                        ]
                      <|
                        case model.new.visibility of
                            Nothing ->
                                Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Yellow
                                        , label = Element.text "Edit Visibility"
                                        , message = Just <| MessagePure <| SetVisibility <| Just <| model.old.visibility
                                        , size = Mensam.Element.Button.Medium
                                        }

                            Just visibility ->
                                Element.row
                                    [ Element.spacing 20
                                    , Element.width Element.fill
                                    , Element.height <| Element.px 60
                                    ]
                                    -- TODO: Use `Mensam.Element.Button.button`.
                                    [ Element.Input.button
                                        [ if visibility == Mensam.Space.MkVisibilityVisible then
                                            Element.Background.color Mensam.Element.Color.bright.green

                                          else
                                            Element.Background.color Mensam.Element.Color.bright.white
                                        , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.magenta ]
                                        , Element.Font.color Mensam.Element.Color.dark.black
                                        , Element.width Element.fill
                                        , Element.padding 10
                                        ]
                                        { onPress = Just <| MessagePure <| SetVisibility <| Just <| Mensam.Space.MkVisibilityVisible
                                        , label =
                                            Element.el
                                                [ Element.centerX
                                                , Element.centerY
                                                , Element.Font.family [ Mensam.Element.Font.condensed ]
                                                ]
                                            <|
                                                Element.text "visible"
                                        }
                                    , Element.Input.button
                                        [ if visibility == Mensam.Space.MkVisibilityHidden then
                                            Element.Background.color Mensam.Element.Color.bright.green

                                          else
                                            Element.Background.color Mensam.Element.Color.bright.white
                                        , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.magenta ]
                                        , Element.Font.color Mensam.Element.Color.dark.black
                                        , Element.width Element.fill
                                        , Element.padding 10
                                        ]
                                        { onPress = Just <| MessagePure <| SetVisibility <| Just <| Mensam.Space.MkVisibilityHidden
                                        , label =
                                            Element.el
                                                [ Element.centerX
                                                , Element.centerY
                                                , Element.Font.family [ Mensam.Element.Font.condensed ]
                                                ]
                                            <|
                                                Element.text "hidden"
                                        }
                                    ]
                    ]
                , Element.row
                    [ Element.spacing 20
                    , Element.padding 20
                    , Element.width Element.fill
                    , Element.alignBottom
                    ]
                    [ Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = []
                            , color = Mensam.Element.Button.Gray
                            , label = Element.text "Go back"
                            , message = Just <| MessageEffect ReturnToSpace
                            , size = Mensam.Element.Button.Medium
                            }
                    , Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = [ Element.width Element.fill ]
                            , color = Mensam.Element.Button.Blue
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
                                        , label = Element.text "Abort"
                                        , message = Just <| MessagePure CloseDialogToDeleteSpace
                                        , size = Mensam.Element.Button.Medium
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Red
                                        , label = Element.text "Delete Space permanently"
                                        , message = Just <| MessageEffect SubmitDeleteSpace
                                        , size = Mensam.Element.Button.Medium
                                        }
                                ]
                            ]
        , closePopup = MessagePure CloseDialogToDeleteSpace
        }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect
    | Messages (List Message)


type MessagePure
    = SetOldSettings
        { name : Mensam.Space.Name
        , timezone : Mensam.Time.TimezoneIdentifier
        , visibility : Mensam.Space.Visibility
        }
    | ResetNewSettings
    | EnterName (Maybe Mensam.Space.Name)
    | SetTimezone (Maybe Mensam.Time.TimezoneIdentifier)
    | SetVisibility (Maybe Mensam.Space.Visibility)
    | OpenDialogToDeleteSpace
    | CloseDialogToDeleteSpace


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetOldSettings args ->
            { model | old = args }

        ResetNewSettings ->
            { model | new = { name = Nothing, timezone = Nothing, visibility = Nothing } }

        EnterName name ->
            let
                newSettings =
                    model.new
            in
            { model | new = { newSettings | name = name } }

        SetTimezone timezone ->
            let
                newSettings =
                    model.new
            in
            { model | new = { newSettings | timezone = timezone } }

        SetVisibility visibility ->
            let
                newSettings =
                    model.new
            in
            { model | new = { newSettings | visibility = visibility } }

        OpenDialogToDeleteSpace ->
            { model | popup = Just PopupDeleteSpace }

        CloseDialogToDeleteSpace ->
            { model | popup = Nothing }


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshOldSettings
    | SubmitSettings
    | ReturnToSpace
    | ReturnToSpaces
    | SubmitDeleteSpace
    | OpenPageToRoles


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


spaceEdit :
    { jwt : Mensam.Auth.Bearer.Jwt
    , id : Mensam.Space.Identifier
    , name : Maybe Mensam.Space.Name
    , timezone : Maybe Mensam.Time.TimezoneIdentifier
    , visibility : Maybe Mensam.Space.Visibility
    }
    -> Cmd Message
spaceEdit requestArgs =
    Mensam.Api.SpaceEdit.request requestArgs <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceEdit.Success args) ->
                    Messages
                        [ MessagePure <| SetOldSettings { name = args.name, timezone = args.timezone, visibility = args.visibility }
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
    { jwt : Mensam.Auth.Bearer.Jwt
    , id : Mensam.Space.Identifier
    }
    -> Cmd Message
spaceDelete requestArgs =
    Mensam.Api.SpaceDelete.request requestArgs <|
        \result ->
            case result of
                Ok Mensam.Api.SpaceDelete.Success ->
                    MessageEffect ReturnToSpaces

                Ok (Mensam.Api.SpaceDelete.ErrorInsufficientPermission permission) ->
                    MessageEffect <| ReportError <| Mensam.Space.Role.errorInsufficientPermission permission

                Ok Mensam.Api.SpaceDelete.ErrorSpaceNotFound ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Try a different space identifier" <|
                                Mensam.Error.message "Space not found" <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.SpaceDelete.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Bad request body" <|
                                Mensam.Error.message error <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.SpaceDelete.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <| ReportError <| Mensam.Error.http error
