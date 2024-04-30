module Mensam.Screen.Space exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Element.Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import List.Extra
import Mensam.Api.DeskCreate
import Mensam.Api.DeskList
import Mensam.Api.ReservationCreate
import Mensam.Api.SpaceLeave
import Mensam.Api.SpaceView
import Mensam.Auth.Bearer
import Mensam.Desk
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Element.Screen
import Mensam.Error
import Mensam.NameOrIdentifier
import Mensam.Reservation
import Mensam.Space
import Mensam.Space.Role
import Mensam.Time
import Mensam.Widget.Date
import Mensam.Widget.Time
import Time


type alias Model =
    { space : Mensam.Space.Identifier
    , name : Mensam.Space.Name
    , roles :
        List
            { accessibility : Mensam.Space.Role.Accessibility
            , id : Mensam.Space.Role.Identifier
            , name : Mensam.Space.Role.Name
            , permissions : Mensam.Space.Role.Permissions
            }
    , timezone : Time.Zone
    , timezoneIdentifier : Mensam.Time.TimezoneIdentifier
    , visibility : Mensam.Space.Visibility
    , yourRole :
        Maybe
            { accessibility : Mensam.Space.Role.Accessibility
            , id : Mensam.Space.Role.Identifier
            , name : Mensam.Space.Role.Name
            , permissions : Mensam.Space.Role.Permissions
            }
    , popup : Maybe PopupModel
    , desks :
        List
            { desk :
                { id : Mensam.Desk.Identifier
                , name : Mensam.Desk.Name
                , space : Mensam.Space.Identifier
                }
            , reservations :
                List
                    { desk : Mensam.Desk.Identifier
                    , id : Mensam.Reservation.Identifier
                    , status : Mensam.Reservation.Status
                    , timeBegin : Time.Posix
                    , timeEnd : Time.Posix
                    , user : Int
                    }
            }
    , selected : Maybe Int
    , modelDateBegin : Mensam.Widget.Date.Model
    , modelTimeBegin : Mensam.Widget.Time.Model
    , modelDateEnd : Mensam.Widget.Date.Model
    , modelTimeEnd : Mensam.Widget.Time.Model
    }


type PopupModel
    = PopupCreate
        { name : Mensam.Desk.Name
        }
    | PopupLeave
    | PopupReservation
        { desk :
            { id : Mensam.Desk.Identifier
            , name : Mensam.Desk.Name
            , space : Mensam.Space.Identifier
            }
        , pickerVisibility : PickerVisibility
        }


type PickerVisibility
    = PickerInvisible
    | DateBeginPickerVisible
    | TimeBeginPickerVisible
    | DateEndPickerVisible
    | TimeEndPickerVisible


init : { id : Mensam.Space.Identifier, time : { now : Time.Posix, zone : Time.Zone } } -> Model
init args =
    { space = args.id
    , name = Mensam.Space.MkName ""
    , roles = []
    , timezone = Time.utc
    , timezoneIdentifier = Mensam.Time.MkTimezoneIdentifier "Etc/UTC"
    , visibility = Mensam.Space.MkVisibilityHidden
    , yourRole = Nothing
    , popup = Nothing
    , desks = []
    , selected = Nothing
    , modelDateBegin =
        let
            date =
                (Mensam.Time.unTimestamp <| Mensam.Time.fromPosix args.time.zone args.time.now).date
        in
        Mensam.Widget.Date.MkModel
            { year = (Mensam.Time.unDate date).year
            , month = (Mensam.Time.unDate date).month
            , selected = date
            }
    , modelTimeBegin =
        Mensam.Widget.Time.MkModel
            { selected =
                Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 12
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
            }
    , modelDateEnd =
        let
            date =
                (Mensam.Time.unTimestamp <| Mensam.Time.fromPosix args.time.zone args.time.now).date
        in
        Mensam.Widget.Date.MkModel
            { year = (Mensam.Time.unDate date).year
            , month = (Mensam.Time.unDate date).month
            , selected = date
            }
    , modelTimeEnd =
        Mensam.Widget.Time.MkModel
            { selected =
                Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 13
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
            }
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
                    [ case model.yourRole of
                        Nothing ->
                            Element.el
                                [ Element.alignRight
                                , Element.padding 10
                                , Element.Background.color Mensam.Element.Color.bright.yellow
                                , Element.Font.color Mensam.Element.Color.dark.black
                                , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                                , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.green ]
                                , Element.Events.onClick <| MessageEffect <| OpenPageToJoin
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
                                    Element.text "Join space"

                        Just _ ->
                            Element.el
                                [ Element.alignRight
                                , Element.padding 10
                                , Element.Background.color Mensam.Element.Color.bright.red
                                , Element.Font.color Mensam.Element.Color.dark.black
                                , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                                , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.white ]
                                , Element.Events.onClick <| MessagePure <| OpenDialogToLeave
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
                                    Element.text "Leave"
                    , case model.yourRole of
                        Nothing ->
                            Element.none

                        Just yourRole ->
                            case List.Extra.find (\p -> p == Mensam.Space.Role.MkPermissionEditSpace) <| Mensam.Space.Role.permissionsToList yourRole.permissions of
                                Nothing ->
                                    Element.none

                                Just _ ->
                                    Element.el
                                        [ Element.alignRight
                                        , Element.padding 10
                                        , Element.Background.color Mensam.Element.Color.bright.yellow
                                        , Element.Font.color Mensam.Element.Color.dark.black
                                        , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                        , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                                        , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.green ]
                                        , Element.Events.onClick <| MessageEffect OpenPageToSettings
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
                                            Element.text "Settings"
                    , Element.el
                        [ Element.alignRight
                        , Element.padding 10
                        , Element.Background.color Mensam.Element.Color.bright.yellow
                        , Element.Font.color Mensam.Element.Color.dark.black
                        , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                        , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                        , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.green ]
                        , Element.Events.onClick <| MessageEffect OpenPageToUsers
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
                            Element.text "Users"
                    , case model.yourRole of
                        Nothing ->
                            Element.none

                        Just yourRole ->
                            case List.Extra.find (\p -> p == Mensam.Space.Role.MkPermissionEditDesk) <| Mensam.Space.Role.permissionsToList yourRole.permissions of
                                Nothing ->
                                    Element.none

                                Just _ ->
                                    Element.el
                                        [ Element.alignRight
                                        , Element.padding 10
                                        , Element.Background.color Mensam.Element.Color.bright.yellow
                                        , Element.Font.color Mensam.Element.Color.dark.black
                                        , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                        , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                                        , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.green ]
                                        , Element.Events.onClick <| MessagePure OpenDialogToCreate
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
                                            Element.text "New Desk"
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
                    { data = model.desks
                    , columns =
                        let
                            cell =
                                Element.el
                                    [ Element.height <| Element.px 40
                                    , Element.width Element.fill
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
                                \n x ->
                                    Element.el
                                        [ Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                                        , Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
                                        , Element.Events.onClick <| MessagePure <| ViewDetailed <| Just { desk = x.desk }
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
                                                    Mensam.Desk.identifierToString x.desk.id
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
                                            Element.text "Reservations"
                          , width = Element.px 120
                          , view =
                                \n x ->
                                    Element.el
                                        [ Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                                        , Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
                                        , Element.Events.onClick <| MessagePure <| ViewDetailed <| Just { desk = x.desk }
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
                                                [ Element.height Element.fill
                                                , Element.width <| Element.maximum 100 <| Element.fill
                                                ]
                                            <|
                                                let
                                                    date =
                                                        case model.modelDateBegin of
                                                            Mensam.Widget.Date.MkModel m ->
                                                                m.selected
                                                in
                                                visualizeReservations model.timezone date x.reservations
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
                                \n x ->
                                    Element.el
                                        [ Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                                        , Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
                                        , Element.Events.onClick <| MessagePure <| ViewDetailed <| Just { desk = x.desk }
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
                                                    Mensam.Desk.nameToString x.desk.name
                          }
                        ]
                    }
                ]
        , popup =
            case model.popup of
                Nothing ->
                    Nothing

                Just (PopupReservation reservation) ->
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
                                Element.text "Create reservation"
                            , Element.row
                                [ Element.width Element.fill
                                , Element.spacing 10
                                ]
                                [ Element.Input.button
                                    [ Element.Background.color Mensam.Element.Color.bright.blue
                                    , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.green ]
                                    , Element.Font.color Mensam.Element.Color.dark.black
                                    , Element.width Element.fill
                                    , Element.padding 10
                                    ]
                                    { onPress = Just <| MessagePure <| ViewDateBeginPicker
                                    , label =
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            , Element.Font.family [ Mensam.Element.Font.condensed ]
                                            , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                            ]
                                        <|
                                            Element.text <|
                                                let
                                                    date =
                                                        case model.modelDateBegin of
                                                            Mensam.Widget.Date.MkModel { selected } ->
                                                                Mensam.Time.unDate selected
                                                in
                                                String.concat
                                                    [ Mensam.Time.yearToString date.year
                                                    , ", "
                                                    , Mensam.Time.monthToString date.month
                                                    , " "
                                                    , Mensam.Time.dayToString date.day
                                                    ]
                                    }
                                , Element.Input.button
                                    [ Element.Background.color Mensam.Element.Color.bright.blue
                                    , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.green ]
                                    , Element.Font.color Mensam.Element.Color.dark.black
                                    , Element.width Element.fill
                                    , Element.padding 10
                                    ]
                                    { onPress = Just <| MessagePure <| ViewTimeBeginPicker
                                    , label =
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            , Element.Font.family [ Mensam.Element.Font.condensed ]
                                            , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                            ]
                                        <|
                                            Element.text <|
                                                let
                                                    time =
                                                        case model.modelTimeBegin of
                                                            Mensam.Widget.Time.MkModel { selected } ->
                                                                selected
                                                in
                                                Mensam.Time.timeToString time
                                    }
                                ]
                            , Element.el
                                [ Element.width Element.fill
                                , Element.height Element.fill
                                ]
                              <|
                                case reservation.pickerVisibility of
                                    DateBeginPickerVisible ->
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            ]
                                        <|
                                            Element.map (MessagePure << MessageDateBegin) <|
                                                Mensam.Widget.Date.elementPickDate model.modelDateBegin

                                    TimeBeginPickerVisible ->
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            ]
                                        <|
                                            Element.map (MessagePure << MessageTimeBegin) <|
                                                Mensam.Widget.Time.elementPickTime model.modelTimeBegin

                                    DateEndPickerVisible ->
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            ]
                                        <|
                                            Element.map (MessagePure << MessageDateEnd) <|
                                                Mensam.Widget.Date.elementPickDate model.modelDateEnd

                                    TimeEndPickerVisible ->
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            ]
                                        <|
                                            Element.map (MessagePure << MessageTimeEnd) <|
                                                Mensam.Widget.Time.elementPickTime model.modelTimeEnd

                                    PickerInvisible ->
                                        Element.none
                            , Element.row
                                [ Element.width Element.fill
                                , Element.spacing 10
                                ]
                                [ Element.Input.button
                                    [ Element.Background.color Mensam.Element.Color.bright.blue
                                    , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.green ]
                                    , Element.Font.color Mensam.Element.Color.dark.black
                                    , Element.width Element.fill
                                    , Element.padding 10
                                    ]
                                    { onPress = Just <| MessagePure <| ViewDateEndPicker
                                    , label =
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            , Element.Font.family [ Mensam.Element.Font.condensed ]
                                            , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                            ]
                                        <|
                                            Element.text <|
                                                let
                                                    date =
                                                        case model.modelDateEnd of
                                                            Mensam.Widget.Date.MkModel { selected } ->
                                                                Mensam.Time.unDate selected
                                                in
                                                String.concat
                                                    [ Mensam.Time.yearToString date.year
                                                    , ", "
                                                    , Mensam.Time.monthToString date.month
                                                    , " "
                                                    , Mensam.Time.dayToString date.day
                                                    ]
                                    }
                                , Element.Input.button
                                    [ Element.Background.color Mensam.Element.Color.bright.blue
                                    , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.green ]
                                    , Element.Font.color Mensam.Element.Color.dark.black
                                    , Element.width Element.fill
                                    , Element.padding 10
                                    ]
                                    { onPress = Just <| MessagePure <| ViewTimeEndPicker
                                    , label =
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            , Element.Font.family [ Mensam.Element.Font.condensed ]
                                            , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                            ]
                                        <|
                                            Element.text <|
                                                let
                                                    time =
                                                        case model.modelTimeEnd of
                                                            Mensam.Widget.Time.MkModel { selected } ->
                                                                selected
                                                in
                                                Mensam.Time.timeToString time
                                    }
                                ]
                            , Element.row
                                [ Element.width Element.fill
                                , Element.spacing 10
                                , Element.alignBottom
                                ]
                                [ Element.Input.button
                                    [ Element.Background.color Mensam.Element.Color.bright.yellow
                                    , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.green ]
                                    , Element.Font.color Mensam.Element.Color.dark.black
                                    , Element.width Element.fill
                                    , Element.padding 10
                                    ]
                                    { onPress = Just <| MessagePure <| ViewDetailed Nothing
                                    , label =
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            , Element.Font.family [ Mensam.Element.Font.condensed ]
                                            , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                            ]
                                        <|
                                            Element.text "Abort"
                                    }
                                , Element.Input.button
                                    [ Element.Background.color Mensam.Element.Color.bright.yellow
                                    , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.green ]
                                    , Element.Font.color Mensam.Element.Color.dark.black
                                    , Element.width Element.fill
                                    , Element.padding 10
                                    ]
                                    { onPress = Just <| MessageEffect <| SubmitReservation
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

                Just PopupLeave ->
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
                                Element.text "Leave Space"
                            , Element.row
                                [ Element.width Element.fill
                                , Element.spacing 10
                                , Element.alignBottom
                                ]
                                [ Element.Input.button
                                    [ Element.Background.color Mensam.Element.Color.bright.yellow
                                    , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.green ]
                                    , Element.Font.color Mensam.Element.Color.dark.black
                                    , Element.width Element.fill
                                    , Element.padding 10
                                    ]
                                    { onPress = Just <| MessagePure <| CloseDialogToLeave
                                    , label =
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            , Element.Font.family [ Mensam.Element.Font.condensed ]
                                            , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                            ]
                                        <|
                                            Element.text "Abort"
                                    }
                                , Element.Input.button
                                    [ Element.Background.color Mensam.Element.Color.bright.red
                                    , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.white ]
                                    , Element.Font.color Mensam.Element.Color.dark.black
                                    , Element.width Element.fill
                                    , Element.padding 10
                                    ]
                                    { onPress = Just <| MessageEffect <| SubmitLeave
                                    , label =
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            , Element.Font.family [ Mensam.Element.Font.condensed ]
                                            , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                            ]
                                        <|
                                            Element.text "Abandon space"
                                    }
                                ]
                            ]

                Just (PopupCreate create) ->
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
                                Element.text "Create Desk"
                            , Element.Input.text
                                [ onEnter <| MessageEffect SubmitCreate
                                , Element.Font.color Mensam.Element.Color.dark.black
                                ]
                                { onChange = MessagePure << EnterDeskName << Mensam.Desk.MkName
                                , text = Mensam.Desk.nameToString create.name
                                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Name"
                                , label = Element.Input.labelAbove [] <| Element.text "Name"
                                }
                            , Element.row
                                [ Element.width Element.fill
                                , Element.spacing 10
                                , Element.alignBottom
                                ]
                                [ Element.Input.button
                                    [ Element.Background.color Mensam.Element.Color.bright.yellow
                                    , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.green ]
                                    , Element.Font.color Mensam.Element.Color.dark.black
                                    , Element.width Element.fill
                                    , Element.padding 10
                                    ]
                                    { onPress = Just <| MessagePure <| CloseDialogToCreate
                                    , label =
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            , Element.Font.family [ Mensam.Element.Font.condensed ]
                                            , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                            ]
                                        <|
                                            Element.text "Abort"
                                    }
                                , Element.Input.button
                                    [ Element.Background.color Mensam.Element.Color.bright.yellow
                                    , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.green ]
                                    , Element.Font.color Mensam.Element.Color.dark.black
                                    , Element.width Element.fill
                                    , Element.padding 10
                                    ]
                                    { onPress = Just <| MessageEffect <| SubmitCreate
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
        , closePopup = MessagePure ClosePopup
        }


visualizeReservations :
    Time.Zone
    -> Mensam.Time.Date
    ->
        List
            { desk : Mensam.Desk.Identifier
            , id : Mensam.Reservation.Identifier
            , status : Mensam.Reservation.Status
            , timeBegin : Time.Posix
            , timeEnd : Time.Posix
            , user : Int
            }
    -> Element.Element a
visualizeReservations timezone date reservations =
    -- TODO: This function is using a conversion to Time.Posix to check for overlapping time. Maybe this is actually fine, but I'm not quite sure at the time of writing.
    let
        timePeriods =
            [ ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 0
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 0
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 1
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 1
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 2
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 2
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 3
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 3
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 4
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 4
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 5
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 5
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 6
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 6
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 7
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 7
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 8
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 8
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 9
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 9
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 10
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 10
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 11
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 11
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 12
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 12
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 13
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 13
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 14
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 14
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 15
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 15
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 16
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 16
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 17
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 17
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 18
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 18
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 19
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 19
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 20
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 20
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 21
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 21
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 22
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 22
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            , ( Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 23
                    , minute = Mensam.Time.MkMinute 0
                    , second = Mensam.Time.MkSecond 0
                    }
              , Mensam.Time.MkTime
                    { hour = Mensam.Time.MkHour 23
                    , minute = Mensam.Time.MkMinute 59
                    , second = Mensam.Time.MkSecond 59
                    }
              )
            ]

        timeToPosix time =
            Mensam.Time.toPosix timezone <| Mensam.Time.MkTimestamp { date = date, time = time }

        timeToPosixPeriod =
            \( t1, t2 ) -> ( timeToPosix t1, timeToPosix t2 )

        posixPeriods =
            List.map timeToPosixPeriod timePeriods

        -- TODO: Are these checks fine with "strictly smaller/greater" (`<` and `>`)?
        checkPeriodIsFree =
            \( t1, t2 ) ->
                List.all
                    (\reservation ->
                        (Time.posixToMillis t1
                            < Time.posixToMillis reservation.timeEnd
                            && Time.posixToMillis t2
                            < Time.posixToMillis reservation.timeBegin
                        )
                            || (Time.posixToMillis t1
                                    > Time.posixToMillis reservation.timeEnd
                                    && Time.posixToMillis t2
                                    > Time.posixToMillis reservation.timeBegin
                               )
                    )
                    reservations

        freePeriods =
            List.map checkPeriodIsFree posixPeriods

        freeToElement =
            \x ->
                Element.el
                    [ Element.height Element.fill
                    , Element.width Element.fill
                    , Element.Background.color <|
                        if x then
                            Mensam.Element.Color.bright.green

                        else
                            Mensam.Element.Color.bright.red
                    ]
                <|
                    Element.none
    in
    Element.row
        [ Element.height Element.fill
        , Element.width Element.fill
        , Element.paddingXY 0 6
        ]
    <|
        List.map freeToElement freePeriods


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect
    | Messages (List Message) -- TODO: Maybe this recursion should be in a separate data type.


type MessagePure
    = SetSpaceInfo Mensam.Space.SpaceView
    | SetDesks
        (List
            { desk :
                { id : Mensam.Desk.Identifier
                , name : Mensam.Desk.Name
                , space : Mensam.Space.Identifier
                }
            , reservations :
                List
                    { desk : Mensam.Desk.Identifier
                    , id : Mensam.Reservation.Identifier
                    , status : Mensam.Reservation.Status
                    , timeBegin : Time.Posix
                    , timeEnd : Time.Posix
                    , user : Int
                    }
            }
        )
    | SetSelected (Maybe Int)
    | ClosePopup
    | OpenDialogToLeave
    | CloseDialogToLeave
    | OpenDialogToCreate
    | CloseDialogToCreate
    | EnterDeskName Mensam.Desk.Name
    | ViewDetailed
        (Maybe
            { desk :
                { id : Mensam.Desk.Identifier
                , name : Mensam.Desk.Name
                , space : Mensam.Space.Identifier
                }
            }
        )
    | ViewDateBeginPicker
    | ViewTimeBeginPicker
    | MessageDateBegin Mensam.Widget.Date.Message
    | MessageTimeBegin Mensam.Widget.Time.Message
    | ViewDateEndPicker
    | ViewTimeEndPicker
    | MessageDateEnd Mensam.Widget.Date.Message
    | MessageTimeEnd Mensam.Widget.Time.Message


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetSpaceInfo (Mensam.Space.MkSpaceView space) ->
            { model
                | space = space.id
                , name = space.name
                , roles = space.roles
                , timezone = Mensam.Time.timezone space.timezone
                , timezoneIdentifier = space.timezone
                , visibility = space.visibility
                , yourRole = space.yourRole
            }

        SetDesks desks ->
            { model | desks = desks }

        SetSelected selection ->
            { model | selected = selection }

        ClosePopup ->
            { model | popup = Nothing }

        OpenDialogToLeave ->
            { model | popup = Just <| PopupLeave }

        CloseDialogToLeave ->
            { model | popup = Nothing }

        OpenDialogToCreate ->
            { model | popup = Just <| PopupCreate { name = Mensam.Desk.MkName "" } }

        CloseDialogToCreate ->
            { model | popup = Nothing }

        EnterDeskName name ->
            { model
                | popup =
                    case model.popup of
                        Just (PopupCreate create) ->
                            Just <|
                                PopupCreate
                                    { create
                                        | name = name
                                    }

                        _ ->
                            model.popup
            }

        ViewDetailed Nothing ->
            { model | popup = Nothing }

        ViewDetailed (Just data) ->
            { model | popup = Just <| PopupReservation { desk = data.desk, pickerVisibility = PickerInvisible } }

        ViewDateBeginPicker ->
            { model
                | popup =
                    case model.popup of
                        Just (PopupReservation reservation) ->
                            Just <|
                                PopupReservation
                                    { reservation
                                        | pickerVisibility = DateBeginPickerVisible
                                    }

                        _ ->
                            model.popup
            }

        ViewTimeBeginPicker ->
            { model
                | popup =
                    case model.popup of
                        Just (PopupReservation reservation) ->
                            Just <|
                                PopupReservation
                                    { reservation
                                        | pickerVisibility = TimeBeginPickerVisible
                                    }

                        _ ->
                            model.popup
            }

        MessageDateBegin Mensam.Widget.Date.NextMonth ->
            { model | modelDateBegin = Mensam.Widget.Date.updateDateNextMonth model.modelDateBegin }

        MessageDateBegin Mensam.Widget.Date.PreviousMonth ->
            { model | modelDateBegin = Mensam.Widget.Date.updateDatePreviousMonth model.modelDateBegin }

        MessageDateBegin (Mensam.Widget.Date.ClickDay day) ->
            let
                (Mensam.Widget.Date.MkModel modelDate) =
                    model.modelDateBegin
            in
            { model
                | modelDateBegin =
                    Mensam.Widget.Date.MkModel
                        { modelDate
                            | selected =
                                Mensam.Time.MkDate
                                    { year = modelDate.year
                                    , month = modelDate.month
                                    , day = day
                                    }
                        }
            }

        MessageTimeBegin (Mensam.Widget.Time.SetHour hour) ->
            let
                (Mensam.Widget.Time.MkModel modelTime) =
                    model.modelTimeBegin
            in
            { model
                | modelTimeBegin =
                    Mensam.Widget.Time.MkModel
                        { modelTime
                            | selected =
                                Mensam.Time.MkTime
                                    { hour = hour
                                    , minute = (Mensam.Time.unTime modelTime.selected).minute
                                    , second = (Mensam.Time.unTime modelTime.selected).second
                                    }
                        }
            }

        MessageTimeBegin (Mensam.Widget.Time.SetMinute minute) ->
            let
                (Mensam.Widget.Time.MkModel modelTime) =
                    model.modelTimeBegin
            in
            { model
                | modelTimeBegin =
                    Mensam.Widget.Time.MkModel
                        { modelTime
                            | selected =
                                Mensam.Time.MkTime
                                    { hour = (Mensam.Time.unTime modelTime.selected).hour
                                    , minute = minute
                                    , second = (Mensam.Time.unTime modelTime.selected).second
                                    }
                        }
            }

        ViewDateEndPicker ->
            { model
                | popup =
                    case model.popup of
                        Just (PopupReservation reservation) ->
                            Just <|
                                PopupReservation
                                    { reservation
                                        | pickerVisibility = DateEndPickerVisible
                                    }

                        _ ->
                            model.popup
            }

        ViewTimeEndPicker ->
            { model
                | popup =
                    case model.popup of
                        Just (PopupReservation reservation) ->
                            Just <|
                                PopupReservation
                                    { reservation
                                        | pickerVisibility = TimeEndPickerVisible
                                    }

                        _ ->
                            model.popup
            }

        MessageDateEnd Mensam.Widget.Date.NextMonth ->
            { model | modelDateEnd = Mensam.Widget.Date.updateDateNextMonth model.modelDateEnd }

        MessageDateEnd Mensam.Widget.Date.PreviousMonth ->
            { model | modelDateEnd = Mensam.Widget.Date.updateDatePreviousMonth model.modelDateEnd }

        MessageDateEnd (Mensam.Widget.Date.ClickDay day) ->
            let
                (Mensam.Widget.Date.MkModel modelDate) =
                    model.modelDateEnd
            in
            { model
                | modelDateEnd =
                    Mensam.Widget.Date.MkModel
                        { modelDate
                            | selected =
                                Mensam.Time.MkDate
                                    { year = modelDate.year
                                    , month = modelDate.month
                                    , day = day
                                    }
                        }
            }

        MessageTimeEnd (Mensam.Widget.Time.SetHour hour) ->
            let
                (Mensam.Widget.Time.MkModel modelTime) =
                    model.modelTimeEnd
            in
            { model
                | modelTimeEnd =
                    Mensam.Widget.Time.MkModel
                        { modelTime
                            | selected =
                                Mensam.Time.MkTime
                                    { hour = hour
                                    , minute = (Mensam.Time.unTime modelTime.selected).minute
                                    , second = (Mensam.Time.unTime modelTime.selected).second
                                    }
                        }
            }

        MessageTimeEnd (Mensam.Widget.Time.SetMinute minute) ->
            let
                (Mensam.Widget.Time.MkModel modelTime) =
                    model.modelTimeEnd
            in
            { model
                | modelTimeEnd =
                    Mensam.Widget.Time.MkModel
                        { modelTime
                            | selected =
                                Mensam.Time.MkTime
                                    { hour = (Mensam.Time.unTime modelTime.selected).hour
                                    , minute = minute
                                    , second = (Mensam.Time.unTime modelTime.selected).second
                                    }
                        }
            }


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshSpace
    | RefreshDesks
    | OpenPageToJoin
    | OpenPageToUsers
    | OpenPageToSettings
    | SubmitLeave
    | SubmitCreate
    | SubmitReservation


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
    Mensam.Api.SpaceView.request { jwt = jwt, id = model.space } <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceView.Success value) ->
                    case
                        let
                            (Mensam.Space.MkSpaceView spaceview) =
                                value.space
                        in
                        spaceview.yourRole
                    of
                        Nothing ->
                            MessageEffect OpenPageToJoin

                        Just _ ->
                            Messages
                                [ MessagePure <| SetSpaceInfo value.space
                                , MessageEffect
                                    RefreshDesks
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


spaceLeave : Mensam.Auth.Bearer.Jwt -> Mensam.Space.Identifier -> Cmd Message
spaceLeave jwt spaceId =
    Mensam.Api.SpaceLeave.request { jwt = jwt, space = Mensam.NameOrIdentifier.Identifier spaceId } <|
        \result ->
            case result of
                Ok Mensam.Api.SpaceLeave.Success ->
                    MessagePure CloseDialogToLeave

                Ok Mensam.Api.SpaceLeave.ErrorOwnerCantLeave ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Owners cannot leave their space." <|
                                Mensam.Error.message "You could try deleting the space." <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.SpaceLeave.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Bad request body" <|
                                Mensam.Error.message error <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.SpaceLeave.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <| ReportError <| Mensam.Error.http error


deskList : Mensam.Auth.Bearer.Jwt -> Model -> Cmd Message
deskList jwt model =
    Mensam.Api.DeskList.request { jwt = jwt, space = model.space } <|
        \result ->
            case result of
                Ok (Mensam.Api.DeskList.Success value) ->
                    MessagePure <| SetDesks value.desks

                Ok (Mensam.Api.DeskList.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Bad request body" <|
                                Mensam.Error.message error <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.DeskList.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <| ReportError <| Mensam.Error.http error


deskCreate :
    { jwt : Mensam.Auth.Bearer.Jwt
    , space : Mensam.Space.Identifier
    , name : Mensam.Desk.Name
    }
    -> Cmd Message
deskCreate req =
    Mensam.Api.DeskCreate.request req <|
        \result ->
            case result of
                Ok (Mensam.Api.DeskCreate.Success _) ->
                    MessagePure CloseDialogToCreate

                Ok (Mensam.Api.DeskCreate.ErrorInsufficientPermission permission) ->
                    MessageEffect <| ReportError <| Mensam.Space.Role.errorInsufficientPermission permission

                Ok Mensam.Api.DeskCreate.ErrorSpaceNotFound ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Creating desk failed" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message "Space not found" <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.DeskCreate.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Creating desk failed" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.DeskCreate.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Creating desk failed" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Creating desk failed" <|
                                Mensam.Error.http error


reservationCreate : Mensam.Auth.Bearer.Jwt -> Model -> { desk : { id : Mensam.Desk.Identifier } } -> Cmd Message
reservationCreate jwt model { desk } =
    Mensam.Api.ReservationCreate.request
        { jwt = jwt
        , desk = desk
        , timeWindow =
            { start =
                Mensam.Time.toPosix model.timezone <|
                    Mensam.Time.MkTimestamp
                        { date =
                            case model.modelDateBegin of
                                Mensam.Widget.Date.MkModel modelDate ->
                                    modelDate.selected
                        , time =
                            case model.modelTimeBegin of
                                Mensam.Widget.Time.MkModel modelTime ->
                                    modelTime.selected
                        }
            , end =
                Mensam.Time.toPosix model.timezone <|
                    Mensam.Time.MkTimestamp
                        { date =
                            case model.modelDateEnd of
                                Mensam.Widget.Date.MkModel modelDate ->
                                    modelDate.selected
                        , time =
                            case model.modelTimeEnd of
                                Mensam.Widget.Time.MkModel modelTime ->
                                    modelTime.selected
                        }
            }
        }
    <|
        \result ->
            case result of
                Ok (Mensam.Api.ReservationCreate.Success _) ->
                    Messages
                        [ MessagePure <| ViewDetailed Nothing
                        , MessageEffect RefreshDesks
                        ]

                Ok (Mensam.Api.ReservationCreate.ErrorInsufficientPermission permission) ->
                    MessageEffect <| ReportError <| Mensam.Space.Role.errorInsufficientPermission permission

                Ok Mensam.Api.ReservationCreate.ErrorTimeUnavailable ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Reservation request failed" <|
                                Mensam.Error.message "Bad request" <|
                                    Mensam.Error.message "Requested time unavailable" <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.ReservationCreate.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Reservation request failed" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.ReservationCreate.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Reservation request failed" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Reservation request failed" <|
                                Mensam.Error.http error
