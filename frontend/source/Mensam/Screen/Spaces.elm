module Mensam.Screen.Spaces exposing (..)

import Element
import Element.Background
import Element.Events.Pointer
import Element.Font
import Element.Input
import Html.Attributes
import Mensam.Api.SpaceCreate
import Mensam.Api.SpaceList
import Mensam.Auth.Bearer
import Mensam.Element.Button
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Element.Screen
import Mensam.Error
import Mensam.Space
import Mensam.Time
import Mensam.User
import Mensam.Widget.Timezone


type alias Model =
    { spaces :
        List
            { id : Mensam.Space.Identifier
            , name : Mensam.Space.Name
            , timezone : Mensam.Time.Timezone
            , owner : Mensam.User.Identifier
            , users : Int
            }
    , selected : Maybe Int
    , popup : Maybe PopupModel
    }


type PopupModel
    = PopupCreateSpace
        { name : Mensam.Space.Name
        , timezone : Mensam.Time.Timezone
        , visible : Bool
        , timezonePicker : Maybe Mensam.Widget.Timezone.Model
        }


type alias MainModelAccess =
    { timezone : Mensam.Time.Timezone
    }


init : Model
init =
    { spaces = []
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
                    [ Element.el
                        [ Element.alignRight
                        , Element.padding 10
                        , Element.Background.color <| Mensam.Element.Color.bright.yellow Mensam.Element.Color.Opaque100
                        , Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                        , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                        , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                        , Element.mouseOver [ Element.Background.color <| Mensam.Element.Color.bright.green Mensam.Element.Color.Opaque100 ]
                        , Element.Events.Pointer.onClick <| \_ -> MessagePure OpenDialogToCreate
                        ]
                      <|
                        Element.el
                            [ Element.centerX
                            , Element.centerY
                            , Element.Font.family [ Mensam.Element.Font.condensed ]
                            , Element.Font.size 17
                            , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                            ]
                        <|
                            Element.text "Create new Space"
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
                    { data = model.spaces
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
                                \n space ->
                                    Element.el
                                        [ Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelected <| Just n
                                        , Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetSelected Nothing
                                        , Element.Events.Pointer.onClick <| \_ -> MessageEffect <| ChooseSpace space.id
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
                                                    Mensam.Space.identifierToString space.id
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
                                \n space ->
                                    Element.el
                                        [ Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelected <| Just n
                                        , Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetSelected Nothing
                                        , Element.Events.Pointer.onClick <| \_ -> MessageEffect <| ChooseSpace space.id
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
                                                    Mensam.Space.nameToString space.name
                          }
                        ]
                    }
                ]
        , popup =
            case model.popup of
                Nothing ->
                    Nothing

                Just (PopupCreateSpace popupModel) ->
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
                                Element.text "Create space"
                            , Element.Input.text
                                [ Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                                ]
                                { onChange = MessagePure << EnterSpaceName << Mensam.Space.MkName
                                , text = Mensam.Space.nameToString popupModel.name
                                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Name"
                                , label = Element.Input.labelAbove [] <| Element.text "Name"
                                }
                            , Element.column
                                [ Element.width Element.fill
                                ]
                                [ Element.row [ Element.width Element.fill ]
                                    [ Element.el [ Element.alignLeft, Element.alignBottom, Element.paddingXY 0 5 ] <| Element.text "Timezone"
                                    , Mensam.Element.Button.button <|
                                        Mensam.Element.Button.MkButton
                                            { attributes = [ Element.alignRight, Element.alignBottom ]
                                            , color = Mensam.Element.Button.Transparent
                                            , enabled = True
                                            , label = Element.text "Reset"
                                            , message =
                                                Just <|
                                                    Messages
                                                        [ MessagePure TimezonePickerClose
                                                        , MessagePure SetTimezoneToLocalTimezone
                                                        ]
                                            , size = Mensam.Element.Button.Small
                                            }
                                    ]
                                , case popupModel.timezonePicker of
                                    Nothing ->
                                        Mensam.Element.Button.button <|
                                            Mensam.Element.Button.MkButton
                                                { attributes = [ Element.width Element.fill ]
                                                , color = Mensam.Element.Button.Yellow
                                                , enabled = True
                                                , label =
                                                    Element.el
                                                        [ Element.paddingXY 0 4
                                                        ]
                                                    <|
                                                        Element.text <|
                                                            Mensam.Time.timezoneToString popupModel.timezone
                                                , message = Just <| MessagePure <| TimezonePickerOpen
                                                , size = Mensam.Element.Button.Small
                                                }

                                    Just timezonePickerModel ->
                                        Element.el
                                            [ Element.width <| Element.px 250
                                            ]
                                        <|
                                            Element.map (MessagePure << TimezonePickerMessage) <|
                                                Mensam.Widget.Timezone.elementPickTimezone timezonePickerModel
                                ]
                            , Element.row
                                [ Element.width Element.fill
                                , Element.spacing 10
                                , Element.alignBottom
                                ]
                                [ Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Gray
                                        , enabled = True
                                        , label = Element.text "Abort"
                                        , message = Just <| MessagePure <| CloseDialogToCreate
                                        , size = Mensam.Element.Button.Medium
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Blue
                                        , enabled = True
                                        , label = Element.text "Submit"
                                        , message =
                                            Just <|
                                                MessageEffect <|
                                                    SubmitCreate
                                                        { name = popupModel.name
                                                        , timezone = popupModel.timezone
                                                        , visible = popupModel.visible
                                                        }
                                        , size = Mensam.Element.Button.Medium
                                        }
                                ]
                            ]
        , closePopup = MessagePure CloseDialogToCreate
        }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect
    | Messages (List Message)


type MessagePure
    = SetSpaces
        (List
            { id : Mensam.Space.Identifier
            , name : Mensam.Space.Name
            , timezone : Mensam.Time.Timezone
            , owner : Mensam.User.Identifier
            , users : Int
            }
        )
    | SetSelected (Maybe Int)
    | OpenDialogToCreate
    | CloseDialogToCreate
    | EnterSpaceName Mensam.Space.Name
    | SetTimezoneToLocalTimezone
    | TimezonePickerOpen
    | TimezonePickerClose
    | TimezonePickerMessage Mensam.Widget.Timezone.Message


updatePure : MessagePure -> MainModelAccess -> Model -> Model
updatePure message mainModel model =
    case message of
        SetSpaces spaces ->
            { model | spaces = spaces }

        SetSelected selection ->
            { model | selected = selection }

        OpenDialogToCreate ->
            { model
                | popup =
                    Just <|
                        PopupCreateSpace
                            { name = Mensam.Space.MkName ""
                            , timezone = mainModel.timezone
                            , visible = False
                            , timezonePicker = Nothing
                            }
            }

        CloseDialogToCreate ->
            { model | popup = Nothing }

        EnterSpaceName name ->
            case model.popup of
                Nothing ->
                    model

                Just (PopupCreateSpace popupModel) ->
                    { model | popup = Just <| PopupCreateSpace { popupModel | name = name } }

        SetTimezoneToLocalTimezone ->
            case model.popup of
                Nothing ->
                    model

                Just (PopupCreateSpace popupModel) ->
                    { model | popup = Just <| PopupCreateSpace { popupModel | timezone = mainModel.timezone } }

        TimezonePickerOpen ->
            case model.popup of
                Nothing ->
                    model

                Just (PopupCreateSpace popupModel) ->
                    { model | popup = Just <| PopupCreateSpace { popupModel | timezonePicker = Just <| Mensam.Widget.Timezone.init popupModel.timezone } }

        TimezonePickerClose ->
            case model.popup of
                Nothing ->
                    model

                Just (PopupCreateSpace popupModel) ->
                    case popupModel.timezonePicker of
                        Nothing ->
                            model

                        Just (Mensam.Widget.Timezone.MkModel timezonePickerModel) ->
                            { model | popup = Just <| PopupCreateSpace { popupModel | timezone = timezonePickerModel.selected, timezonePicker = Nothing } }

        TimezonePickerMessage msg ->
            case model.popup of
                Nothing ->
                    model

                Just (PopupCreateSpace popupModel) ->
                    case popupModel.timezonePicker of
                        Nothing ->
                            model

                        Just timezonePickerModel ->
                            case msg of
                                Mensam.Widget.Timezone.Select _ ->
                                    updatePure
                                        TimezonePickerClose
                                        mainModel
                                        { model | popup = Just <| PopupCreateSpace { popupModel | timezonePicker = Just <| Mensam.Widget.Timezone.update msg timezonePickerModel } }

                                Mensam.Widget.Timezone.Hover _ ->
                                    { model | popup = Just <| PopupCreateSpace { popupModel | timezonePicker = Just <| Mensam.Widget.Timezone.update msg timezonePickerModel } }


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshSpaces
    | ChooseSpace Mensam.Space.Identifier
    | SubmitCreate
        { name : Mensam.Space.Name
        , timezone : Mensam.Time.Timezone
        , visible : Bool
        }


spaceList : Mensam.Auth.Bearer.Jwt -> Cmd Message
spaceList jwt =
    Mensam.Api.SpaceList.request { jwt = jwt, order = [], member = Nothing } <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceList.Success value) ->
                    MessagePure <| SetSpaces value.spaces

                Ok (Mensam.Api.SpaceList.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting spaces failed" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.SpaceList.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting spaces failed" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting spaces failed" <|
                                Mensam.Error.http error


spaceCreate :
    { jwt : Mensam.Auth.Bearer.Jwt
    , name : Mensam.Space.Name
    , timezone : Mensam.Time.Timezone
    , visibility : Mensam.Space.Visibility
    }
    -> Cmd Message
spaceCreate req =
    Mensam.Api.SpaceCreate.request req <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceCreate.Success _) ->
                    MessagePure CloseDialogToCreate

                Ok (Mensam.Api.SpaceCreate.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Creating space failed" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.SpaceCreate.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Creating space failed" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Creating space failed" <|
                                Mensam.Error.http error
