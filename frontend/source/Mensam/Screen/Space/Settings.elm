module Mensam.Screen.Space.Settings exposing (..)

import Element
import Element.Background
import Element.Events.Pointer
import Element.Font
import Element.Input
import Html.Attributes
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
        , timezone : Mensam.Time.Timezone
        , visibility : Mensam.Space.Visibility
        }
    , new :
        { name : Maybe Mensam.Space.Name
        , timezone :
            Maybe
                { selected : Mensam.Time.Timezone
                , hovering : Maybe Int
                }
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
        , timezone = Mensam.Time.timezoneEtcUtc
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
                                        , enabled = True
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
                                            Element.Background.color <| Mensam.Element.Color.bright.green Mensam.Element.Color.Opaque100

                                          else
                                            Element.Background.color <| Mensam.Element.Color.bright.white Mensam.Element.Color.Opaque100
                                        , Element.mouseOver [ Element.Background.color <| Mensam.Element.Color.bright.magenta Mensam.Element.Color.Opaque100 ]
                                        , Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
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
                                            Element.Background.color <| Mensam.Element.Color.bright.green Mensam.Element.Color.Opaque100

                                          else
                                            Element.Background.color <| Mensam.Element.Color.bright.white Mensam.Element.Color.Opaque100
                                        , Element.mouseOver [ Element.Background.color <| Mensam.Element.Color.bright.magenta Mensam.Element.Color.Opaque100 ]
                                        , Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
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
        , closePopup = MessagePure CloseDialogToDeleteSpace
        }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect
    | Messages (List Message)


type MessagePure
    = SetOldSettings
        { name : Mensam.Space.Name
        , timezone : Mensam.Time.Timezone
        , visibility : Mensam.Space.Visibility
        }
    | ResetNewSettings
    | EnterName (Maybe Mensam.Space.Name)
    | SetTimezone (Maybe Mensam.Time.Timezone)
    | SetHoveringTimezone (Maybe Int)
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


spaceEdit :
    { jwt : Mensam.Auth.Bearer.Jwt
    , id : Mensam.Space.Identifier
    , name : Maybe Mensam.Space.Name
    , timezone : Maybe Mensam.Time.Timezone
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
