module Mensam.Screen.Space exposing (..)

import Browser.Dom
import Element
import Element.Background
import Element.Border
import Element.Events.Pointer
import Element.Font
import Element.Window
import Html.Attributes
import List.Extra
import Mensam.Api.DeskList
import Mensam.Api.ReservationCreate
import Mensam.Api.SpaceLeave
import Mensam.Api.SpaceView
import Mensam.Auth.Bearer
import Mensam.Desk
import Mensam.Element.Button
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Element.Screen
import Mensam.Error
import Mensam.NameOrIdentifier
import Mensam.Reservation
import Mensam.Room
import Mensam.Space
import Mensam.Space.Role
import Mensam.Svg.Color
import Mensam.Time
import Mensam.User
import Mensam.Widget.Date
import Mensam.Widget.Time
import Svg
import Svg.Attributes
import Task
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
    , timezone : Mensam.Time.Timezone
    , visibility : Mensam.Space.Visibility
    , yourRole :
        Maybe
            { accessibility : Mensam.Space.Role.Accessibility
            , id : Mensam.Space.Role.Identifier
            , name : Mensam.Space.Role.Name
            , permissions : Mensam.Space.Role.Permissions
            }
    , desks :
        List
            { desk :
                { id : Mensam.Desk.Identifier
                , name : Mensam.Desk.Name
                , space : Mensam.Space.Identifier
                , location : Maybe Mensam.Desk.Location
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
    , selectionDragging :
        Maybe
            { start : Mensam.Time.Hour
            , end : Mensam.Time.Hour
            }
    , modelDateBegin : Mensam.Widget.Date.Model
    , modelTimeBegin : Mensam.Widget.Time.Model
    , modelDateEnd : Mensam.Widget.Date.Model
    , modelTimeEnd : Mensam.Widget.Time.Model
    , globalDatePickerVisible : Bool
    , tabView : TabView
    , window : Element.Window.Model
    , timetablePointer : Maybe Element.Events.Pointer.Event
    , timetablePointerRegionDimensions : Maybe { width : Float, height : Float }
    , popup : Maybe PopupModel
    }


type TabView
    = TabTimetable
    | TabRoom


type PopupModel
    = PopupLeave
    | PopupReservation
        { desk :
            { id : Mensam.Desk.Identifier
            , name : Mensam.Desk.Name
            , space : Mensam.Space.Identifier
            , location : Maybe Mensam.Desk.Location
            }
        , pickerVisibility : PickerVisibility
        }


type PickerVisibility
    = PickerInvisible
    | DateBeginPickerVisible
    | TimeBeginPickerVisible
    | DateEndPickerVisible
    | TimeEndPickerVisible


init : { id : Mensam.Space.Identifier, time : { now : Time.Posix, zone : Mensam.Time.Timezone } } -> Model
init args =
    { space = args.id
    , name = Mensam.Space.MkName ""
    , roles = []
    , timezone = Mensam.Time.timezoneEtcUtc
    , visibility = Mensam.Space.MkVisibilityHidden
    , yourRole = Nothing
    , desks = []
    , selected = Nothing
    , selectionDragging = Nothing
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
    , globalDatePickerVisible = False
    , tabView = TabTimetable
    , window = Element.Window.init Element.Window.defaultConfig { position = { x = 183.5, y = 250 } }
    , timetablePointer = Nothing
    , timetablePointerRegionDimensions = Nothing
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
                    , Element.spacing 20
                    ]
                    [ case model.yourRole of
                        Nothing ->
                            Mensam.Element.Button.button <|
                                Mensam.Element.Button.MkButton
                                    { attributes = [ Element.alignRight, Element.centerY ]
                                    , color = Mensam.Element.Button.Yellow
                                    , enabled = True
                                    , label = Element.text "Join"
                                    , message = Just <| MessageEffect OpenPageToJoin
                                    , size = Mensam.Element.Button.Medium
                                    }

                        Just _ ->
                            Mensam.Element.Button.button <|
                                Mensam.Element.Button.MkButton
                                    { attributes = [ Element.alignRight, Element.centerY ]
                                    , color = Mensam.Element.Button.Red
                                    , enabled = True
                                    , label = Element.text "Leave"
                                    , message = Just <| MessagePure OpenDialogToLeave
                                    , size = Mensam.Element.Button.Medium
                                    }
                    , case model.yourRole of
                        Nothing ->
                            Element.none

                        Just yourRole ->
                            case List.Extra.find (\p -> p == Mensam.Space.Role.MkPermissionEditSpace) <| Mensam.Space.Role.permissionsToList yourRole.permissions of
                                Nothing ->
                                    Element.none

                                Just _ ->
                                    Mensam.Element.Button.button <|
                                        Mensam.Element.Button.MkButton
                                            { attributes = [ Element.alignRight, Element.centerY ]
                                            , color = Mensam.Element.Button.Yellow
                                            , enabled = True
                                            , label = Element.text "Settings"
                                            , message = Just <| MessageEffect OpenPageToSettings
                                            , size = Mensam.Element.Button.Medium
                                            }
                    , Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = [ Element.alignRight, Element.centerY ]
                            , color = Mensam.Element.Button.Yellow
                            , enabled = True
                            , label = Element.text "Users"
                            , message = Just <| MessageEffect OpenPageToUsers
                            , size = Mensam.Element.Button.Medium
                            }
                    , case model.yourRole of
                        Nothing ->
                            Element.none

                        Just yourRole ->
                            case List.Extra.find (\p -> p == Mensam.Space.Role.MkPermissionEditDesk) <| Mensam.Space.Role.permissionsToList yourRole.permissions of
                                Nothing ->
                                    Element.none

                                Just _ ->
                                    Mensam.Element.Button.button <|
                                        Mensam.Element.Button.MkButton
                                            { attributes = [ Element.alignRight, Element.centerY ]
                                            , color = Mensam.Element.Button.Yellow
                                            , enabled = True
                                            , label = Element.text "Desks"
                                            , message = Just <| MessageEffect OpenPageToDesks
                                            , size = Mensam.Element.Button.Medium
                                            }
                    ]
                , Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    [ elementTabs model.tabView
                    , case model.tabView of
                        TabTimetable ->
                            deskTimetable model

                        TabRoom ->
                            deskRoom model
                    ]
                , Element.row
                    [ Element.width Element.fill
                    , Element.padding 10
                    , Element.spacing 10
                    , Element.alignBottom
                    ]
                    [ Element.column
                        [ Element.width Element.fill
                        , Element.alignBottom
                        ]
                        [ if model.globalDatePickerVisible then
                            Element.el
                                [ Element.centerX
                                , Element.alignBottom
                                ]
                            <|
                                Element.map
                                    (\m ->
                                        Messages
                                            [ MessagePure <| MessageDateBegin m
                                            , MessagePure SetDateEndToDateBegin
                                            , case m of
                                                Mensam.Widget.Date.PreviousMonth ->
                                                    Messages []

                                                Mensam.Widget.Date.NextMonth ->
                                                    Messages []

                                                Mensam.Widget.Date.ClickDay _ ->
                                                    MessagePure <| ViewDateGlobalPicker False
                                            ]
                                    )
                                <|
                                    Mensam.Widget.Date.elementPickDate model.modelDateBegin

                          else
                            Element.none
                        , Element.row
                            [ Element.width <| Element.maximum 320 <| Element.fill
                            , Element.height Element.shrink
                            , Element.centerX
                            , Element.spacing 10
                            ]
                            [ Mensam.Element.Button.button <|
                                Mensam.Element.Button.MkButton
                                    { attributes = []
                                    , color = Mensam.Element.Button.Blue
                                    , enabled = True
                                    , label = Element.text "<"
                                    , message =
                                        Just <|
                                            Messages
                                                [ MessagePure PreviousDay
                                                , MessagePure ResetDateBeginToSelection
                                                , MessagePure ResetDateBeginToSelection
                                                ]
                                    , size = Mensam.Element.Button.Medium
                                    }
                            , Mensam.Element.Button.button <|
                                Mensam.Element.Button.MkButton
                                    { attributes = [ Element.width Element.fill ]
                                    , color = Mensam.Element.Button.Blue
                                    , enabled = True
                                    , label =
                                        let
                                            date =
                                                case model.modelDateBegin of
                                                    Mensam.Widget.Date.MkModel { selected } ->
                                                        Mensam.Time.unDate selected
                                        in
                                        Element.text <|
                                            String.concat
                                                [ Mensam.Time.yearToString date.year
                                                , ", "
                                                , Mensam.Time.monthToString date.month
                                                , " "
                                                , Mensam.Time.dayToString date.day
                                                ]
                                    , message = Just <| MessagePure <| ViewDateGlobalPicker <| not model.globalDatePickerVisible
                                    , size = Mensam.Element.Button.Medium
                                    }
                            , Mensam.Element.Button.button <|
                                Mensam.Element.Button.MkButton
                                    { attributes = []
                                    , color = Mensam.Element.Button.Blue
                                    , enabled = True
                                    , label = Element.text ">"
                                    , message =
                                        Just <|
                                            Messages
                                                [ MessagePure NextDay
                                                , MessagePure ResetDateBeginToSelection
                                                , MessagePure ResetDateBeginToSelection
                                                ]
                                    , size = Mensam.Element.Button.Medium
                                    }
                            ]
                        ]
                    ]
                ]
        , popup =
            case model.popup of
                Nothing ->
                    Nothing

                Just (PopupReservation reservation) ->
                    let
                        clashes =
                            reservationClashes
                                { timezone = model.timezone
                                , begin =
                                    Mensam.Time.MkTimestamp
                                        { date = (Mensam.Widget.Date.unModel model.modelDateBegin).selected
                                        , time = (Mensam.Widget.Time.unModel model.modelTimeBegin).selected
                                        }
                                , end =
                                    Mensam.Time.MkTimestamp
                                        { date = (Mensam.Widget.Date.unModel model.modelDateEnd).selected
                                        , time = (Mensam.Widget.Time.unModel model.modelTimeEnd).selected
                                        }
                                , reservations =
                                    List.concat <|
                                        List.filterMap
                                            (\d ->
                                                if d.desk.id == reservation.desk.id then
                                                    Just d.reservations

                                                else
                                                    Nothing
                                            )
                                            model.desks
                                }
                    in
                    Just <|
                        Element.column
                            [ Element.spacing 17
                            , Element.width Element.fill
                            , Element.height Element.fill
                            ]
                            [ Element.el
                                [ Element.Font.size 30
                                , Element.Font.hairline
                                , Element.alignTop
                                ]
                              <|
                                Element.text "Create reservation"
                            , Element.column
                                [ Element.width Element.fill
                                , Element.spacing 3
                                , Element.alignTop
                                ]
                                [ Element.el
                                    [ Element.Font.size 10
                                    , Element.alignBottom
                                    , Element.padding 1
                                    , Element.centerX
                                    , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                                    , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                    , Element.Font.family [ Mensam.Element.Font.condensed ]
                                    , Element.Font.color <| Mensam.Element.Color.bright.cyan Mensam.Element.Color.Opaque100
                                    ]
                                  <|
                                    Element.text "from"
                                , Element.row
                                    [ Element.width Element.fill
                                    , Element.spacing 10
                                    ]
                                    [ Mensam.Element.Button.button <|
                                        Mensam.Element.Button.MkButton
                                            { attributes = [ Element.width Element.fill ]
                                            , color = Mensam.Element.Button.Blue
                                            , enabled = True
                                            , label =
                                                let
                                                    date =
                                                        case model.modelDateBegin of
                                                            Mensam.Widget.Date.MkModel { selected } ->
                                                                Mensam.Time.unDate selected
                                                in
                                                Element.text <|
                                                    String.concat
                                                        [ Mensam.Time.yearToString date.year
                                                        , ", "
                                                        , Mensam.Time.monthToString date.month
                                                        , " "
                                                        , Mensam.Time.dayToString date.day
                                                        ]
                                            , message = Just <| MessagePure ViewDateBeginPicker
                                            , size = Mensam.Element.Button.Medium
                                            }
                                    , Mensam.Element.Button.button <|
                                        Mensam.Element.Button.MkButton
                                            { attributes = [ Element.width Element.fill ]
                                            , color =
                                                if clashes.beginClashes then
                                                    Mensam.Element.Button.Red

                                                else
                                                    Mensam.Element.Button.Blue
                                            , enabled = True
                                            , label =
                                                let
                                                    time =
                                                        case model.modelTimeBegin of
                                                            Mensam.Widget.Time.MkModel { selected } ->
                                                                Mensam.Time.unTime selected
                                                in
                                                Element.text <|
                                                    String.concat
                                                        [ Mensam.Time.hourToString time.hour
                                                        , ":"
                                                        , Mensam.Time.minuteToString time.minute
                                                        ]
                                            , message = Just <| MessagePure ViewTimeBeginPicker
                                            , size = Mensam.Element.Button.Medium
                                            }
                                    ]
                                ]
                            , Element.el
                                [ Element.width Element.fill
                                , Element.centerY
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
                            , Element.column
                                [ Element.width Element.fill
                                , Element.spacing 3
                                , Element.alignBottom
                                ]
                                [ Element.el
                                    [ Element.Font.size 10
                                    , Element.alignBottom
                                    , Element.padding 1
                                    , Element.centerX
                                    , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                                    , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                    , Element.Font.family [ Mensam.Element.Font.condensed ]
                                    , Element.Font.color <| Mensam.Element.Color.bright.cyan Mensam.Element.Color.Opaque100
                                    ]
                                  <|
                                    Element.text "to"
                                , Element.row
                                    [ Element.width Element.fill
                                    , Element.spacing 10
                                    , Element.alignBottom
                                    ]
                                    [ Mensam.Element.Button.button <|
                                        Mensam.Element.Button.MkButton
                                            { attributes = [ Element.width Element.fill ]
                                            , color = Mensam.Element.Button.Blue
                                            , enabled = True
                                            , label =
                                                let
                                                    date =
                                                        case model.modelDateEnd of
                                                            Mensam.Widget.Date.MkModel { selected } ->
                                                                Mensam.Time.unDate selected
                                                in
                                                Element.text <|
                                                    String.concat
                                                        [ Mensam.Time.yearToString date.year
                                                        , ", "
                                                        , Mensam.Time.monthToString date.month
                                                        , " "
                                                        , Mensam.Time.dayToString date.day
                                                        ]
                                            , message = Just <| MessagePure ViewDateEndPicker
                                            , size = Mensam.Element.Button.Medium
                                            }
                                    , Mensam.Element.Button.button <|
                                        Mensam.Element.Button.MkButton
                                            { attributes = [ Element.width Element.fill ]
                                            , color =
                                                if clashes.endClashes then
                                                    Mensam.Element.Button.Red

                                                else
                                                    Mensam.Element.Button.Blue
                                            , enabled = True
                                            , label =
                                                let
                                                    time =
                                                        case model.modelTimeEnd of
                                                            Mensam.Widget.Time.MkModel { selected } ->
                                                                Mensam.Time.unTime selected
                                                in
                                                Element.text <|
                                                    String.concat
                                                        [ Mensam.Time.hourToString time.hour
                                                        , ":"
                                                        , Mensam.Time.minuteToString time.minute
                                                        ]
                                            , message = Just <| MessagePure ViewTimeEndPicker
                                            , size = Mensam.Element.Button.Medium
                                            }
                                    ]
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
                                        , message = Just <| MessagePure <| ViewDetailed Nothing
                                        , size = Mensam.Element.Button.Medium
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Yellow
                                        , enabled = not <| clashes.beginClashes || clashes.endClashes
                                        , label = Element.text "Submit"
                                        , message = Just <| MessageEffect <| SubmitReservation
                                        , size = Mensam.Element.Button.Medium
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
                            , Element.paragraph
                                [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Light300
                                ]
                                [ Element.text "When leaving a space you lose all permissions you had for this space."
                                ]
                            , Element.paragraph
                                [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Light300
                                ]
                                [ Element.text "Your reservations will remain untouched."
                                ]
                            , Element.paragraph
                                [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Light300
                                , Element.alignBottom
                                ]
                                [ Element.text "Are you sure you want to leave this space?"
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
                                        , message = Just <| MessagePure CloseDialogToLeave
                                        , size = Mensam.Element.Button.Medium
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Red
                                        , enabled = True
                                        , label = Element.text "Abandon space"
                                        , message = Just <| MessageEffect SubmitLeave
                                        , size = Mensam.Element.Button.Medium
                                        }
                                ]
                            ]
        , closePopup = MessagePure ClosePopup
        }


elementTabs : TabView -> Element.Element Message
elementTabs tabSelected =
    Element.row
        [ Element.paddingEach
            { bottom = 0
            , left = 8
            , top = 10
            , right = 8
            }
        , Element.width Element.fill
        , Element.height <| Element.px 42
        ]
        [ Element.row
            [ Element.spacing 8
            , Element.height Element.fill
            , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
            , Element.alignRight
            ]
          <|
            let
                elementTab tab title =
                    Element.el
                        [ Element.height <| Element.fill
                        , Element.mouseOver
                            [ Element.Background.color <| Element.rgba 1 1 1 0.78
                            , Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                            ]
                        , Element.Background.color
                            (if tab == tabSelected then
                                Element.rgba 1 1 1 0.63

                             else
                                Element.rgba 1 1 1 0.39
                            )
                        , Element.Font.color
                            (if tab == tabSelected then
                                Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100

                             else
                                Mensam.Element.Color.bright.black Mensam.Element.Color.Opaque100
                            )
                        , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                        , Element.Events.Pointer.onClick <| \_ -> MessagePure <| SetTabView tab
                        ]
                    <|
                        Element.el
                            [ Element.centerY
                            , Element.padding 5
                            ]
                        <|
                            Element.text title
            in
            [ elementTab TabTimetable "Timetable"
            , elementTab TabRoom "Room"
            ]
        ]


deskTimetable : Model -> Element.Element Message
deskTimetable model =
    Element.indexedTable
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Background.color (Element.rgba 0 0 0 0.1)
        , Element.Font.family [ Mensam.Element.Font.condensed ]
        , Element.Font.size 16
        , Element.clipY
        , Element.scrollbarY
        , Element.htmlAttribute <| Html.Attributes.style "contain" "size"
        , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
        ]
        { data = model.desks
        , columns =
            [ { header = Element.none
              , width = Element.px 100
              , view =
                    \n x ->
                        Element.el
                            [ Element.width Element.fill
                            , Element.height <| Element.px 60
                            , Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelected <| Just n
                            , Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetSelected Nothing
                            , Element.Events.Pointer.onClick <| \_ -> MessagePure <| ViewDetailed <| Just { desk = x.desk, dontViewUnlessMouseIsStillDragging = False }
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
                            , Element.clip
                            ]
                        <|
                            Element.column
                                [ Element.width Element.fill
                                , Element.height Element.fill
                                , Element.Border.widthEach
                                    { bottom = 0
                                    , left = 0
                                    , right = 0
                                    , top = 1
                                    }
                                , Element.padding 10
                                ]
                                [ Element.paragraph
                                    (Element.Font.size 12 :: Mensam.Element.Font.font (Mensam.Element.Font.Condensed { weight = Mensam.Element.Font.Regular400, italic = True }))
                                    [ Element.text "#"
                                    , Element.text <| Mensam.Desk.identifierToString x.desk.id
                                    ]
                                , Element.paragraph
                                    [ Element.Font.size 16
                                    ]
                                    [ Element.text <| Mensam.Desk.nameToString x.desk.name
                                    ]
                                ]
              }
            , { header = Element.none
              , width = Element.fill
              , view =
                    \n x ->
                        Element.el
                            [ Element.width Element.fill
                            , Element.height <| Element.px 60
                            , Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelected <| Just n
                            , Element.Events.Pointer.onLeave <|
                                \_ ->
                                    Messages
                                        [ MessagePure <| SetSelected Nothing
                                        , MessagePure AbortSelectionDragging
                                        ]
                            , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                            , Element.htmlAttribute <| Html.Attributes.style "touch-action" "pan-y pinch-zoom"
                            , Element.inFront <|
                                let
                                    elementId =
                                        "timetableRegion" ++ String.fromInt n
                                in
                                Element.el
                                    [ Element.width Element.fill
                                    , Element.height Element.fill
                                    , Element.Background.color Mensam.Element.Color.transparent
                                    , Element.Events.Pointer.onEnter <| \_ -> MessageEffect <| GetSelectorRegionWidth elementId
                                    , Element.Events.Pointer.onMove <|
                                        \e ->
                                            Messages
                                                [ MessagePure <| SetTimetablePointer e
                                                , MessagePure <| SetSelectionDraggingKeepFromPointer
                                                ]
                                    , Element.Events.Pointer.onDown <|
                                        \e ->
                                            Messages
                                                [ MessagePure <| SetTimetablePointer e
                                                , MessagePure <| SetSelectionDraggingStartFromPointer
                                                ]
                                    , Element.Events.Pointer.onUp <|
                                        \e ->
                                            Messages
                                                [ MessagePure <| SetTimetablePointer e
                                                , MessagePure <| SetSelectionDraggingKeepFromPointer
                                                , MessagePure SetTimeFromSelectionDragging
                                                , MessagePure <| ViewDetailed <| Just { desk = x.desk, dontViewUnlessMouseIsStillDragging = True }
                                                , MessagePure AbortSelectionDragging
                                                ]
                                    , Element.htmlAttribute <| Html.Attributes.id <| elementId
                                    ]
                                <|
                                    Element.none
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
                            Element.row
                                [ Element.width Element.fill
                                , Element.height Element.fill
                                , Element.alignTop
                                , Element.Border.widthEach
                                    { bottom = 0
                                    , left = 0
                                    , right = 0
                                    , top = 1
                                    }
                                , Element.behindContent <|
                                    Element.el
                                        [ Element.width Element.fill
                                        , Element.height Element.fill
                                        ]
                                    <|
                                        visualizeReservations
                                            model.timezone
                                            (Mensam.Widget.Date.unModel model.modelDateBegin).selected
                                            x.reservations
                                ]
                            <|
                                let
                                    rowPiece piece =
                                        Element.el
                                            ([ Element.width Element.fill
                                             , Element.height Element.fill
                                             ]
                                                ++ (case model.selectionDragging of
                                                        Nothing ->
                                                            case ( model.timetablePointer, model.timetablePointerRegionDimensions ) of
                                                                ( Just ptr, Just dim ) ->
                                                                    if calculateHour ptr dim == Mensam.Time.MkHour piece.hour && model.selected == Just n then
                                                                        [ Element.Background.color <| Mensam.Element.Color.bright.green Mensam.Element.Color.Opaque50
                                                                        ]

                                                                    else
                                                                        []

                                                                _ ->
                                                                    []

                                                        Just interval ->
                                                            let
                                                                orderedInterval =
                                                                    if Mensam.Time.unHour interval.start <= Mensam.Time.unHour interval.end then
                                                                        { begin = interval.start, end = interval.end }

                                                                    else
                                                                        { begin = interval.end, end = interval.start }
                                                            in
                                                            if
                                                                piece.hour
                                                                    >= Mensam.Time.unHour orderedInterval.begin
                                                                    && piece.hour
                                                                    <= Mensam.Time.unHour orderedInterval.end
                                                                    && (case model.selected of
                                                                            Nothing ->
                                                                                False

                                                                            Just m ->
                                                                                n == m
                                                                       )
                                                            then
                                                                [ Element.Background.color <| Mensam.Element.Color.bright.green Mensam.Element.Color.Opaque50
                                                                ]

                                                            else
                                                                []
                                                   )
                                            )
                                        <|
                                            Element.el
                                                [ Element.width Element.fill
                                                , Element.height <| Element.px piece.indicatorHeight
                                                , Element.alignTop
                                                , Element.Font.size 8
                                                , Element.paddingEach
                                                    { bottom = 0
                                                    , left = 1
                                                    , right = 0
                                                    , top = 0
                                                    }
                                                , Element.Border.widthEach
                                                    { bottom = 0
                                                    , left = 1
                                                    , right = 0
                                                    , top = 0
                                                    }
                                                ]
                                            <|
                                                Element.paragraph
                                                    [ Element.width <| Element.px 8
                                                    , Element.height <| Element.px 15
                                                    ]
                                                    [ Element.text <| String.fromInt piece.hour
                                                    ]
                                in
                                List.map rowPiece
                                    [ { hour = 0, indicatorHeight = 40 }
                                    , { hour = 1, indicatorHeight = 12 }
                                    , { hour = 2, indicatorHeight = 12 }
                                    , { hour = 3, indicatorHeight = 18 }
                                    , { hour = 4, indicatorHeight = 12 }
                                    , { hour = 5, indicatorHeight = 12 }
                                    , { hour = 6, indicatorHeight = 25 }
                                    , { hour = 7, indicatorHeight = 12 }
                                    , { hour = 8, indicatorHeight = 12 }
                                    , { hour = 9, indicatorHeight = 18 }
                                    , { hour = 10, indicatorHeight = 12 }
                                    , { hour = 11, indicatorHeight = 12 }
                                    , { hour = 12, indicatorHeight = 40 }
                                    , { hour = 13, indicatorHeight = 12 }
                                    , { hour = 14, indicatorHeight = 12 }
                                    , { hour = 15, indicatorHeight = 18 }
                                    , { hour = 16, indicatorHeight = 12 }
                                    , { hour = 17, indicatorHeight = 12 }
                                    , { hour = 18, indicatorHeight = 25 }
                                    , { hour = 19, indicatorHeight = 12 }
                                    , { hour = 20, indicatorHeight = 12 }
                                    , { hour = 21, indicatorHeight = 18 }
                                    , { hour = 22, indicatorHeight = 12 }
                                    , { hour = 23, indicatorHeight = 12 }
                                    ]
              }
            ]
        }


visualizeReservations :
    Mensam.Time.Timezone
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
    Element.html <|
        Svg.svg
            [ Svg.Attributes.width "100%"
            , Svg.Attributes.height "100%"
            , Svg.Attributes.preserveAspectRatio "none"
            , Svg.Attributes.viewBox "0 0 86400 100"
            ]
        <|
            List.filterMap (visualizeReservation timezone date) reservations


visualizeReservation :
    Mensam.Time.Timezone
    -> Mensam.Time.Date
    ->
        { desk : Mensam.Desk.Identifier
        , id : Mensam.Reservation.Identifier
        , status : Mensam.Reservation.Status
        , timeBegin : Time.Posix
        , timeEnd : Time.Posix
        , user : Int
        }
    -> Maybe (Svg.Svg a)
visualizeReservation timezone date reservation =
    case reservation.status of
        Mensam.Reservation.MkStatusPlanned ->
            let
                timestamp =
                    { begin = Mensam.Time.fromPosix timezone reservation.timeBegin
                    , end = Mensam.Time.fromPosix timezone reservation.timeEnd
                    }

                isToday =
                    { begin =
                        case Mensam.Time.compareDate (Mensam.Time.unTimestamp timestamp.begin).date date of
                            LT ->
                                Just False

                            EQ ->
                                Just True

                            GT ->
                                Nothing
                    , end =
                        case Mensam.Time.compareDate (Mensam.Time.unTimestamp timestamp.end).date date of
                            LT ->
                                Nothing

                            EQ ->
                                Just True

                            GT ->
                                Just False
                    }

                time =
                    { begin =
                        case isToday.begin of
                            Nothing ->
                                Nothing

                            Just True ->
                                Just (Mensam.Time.unTimestamp timestamp.begin).time

                            Just False ->
                                Just <|
                                    Mensam.Time.MkTime
                                        { hour = Mensam.Time.MkHour 0
                                        , minute = Mensam.Time.MkMinute 0
                                        , second = Mensam.Time.MkSecond 0
                                        }
                    , end =
                        case isToday.end of
                            Nothing ->
                                Nothing

                            Just True ->
                                Just (Mensam.Time.unTimestamp timestamp.end).time

                            Just False ->
                                Just <|
                                    Mensam.Time.MkTime
                                        { hour = Mensam.Time.MkHour 23
                                        , minute = Mensam.Time.MkMinute 59
                                        , second = Mensam.Time.MkSecond 59
                                        }
                    }

                secondsMaybe =
                    { begin = Maybe.map timeToSeconds time.begin
                    , end = Maybe.map timeToSeconds time.end
                    }

                maybeSeconds =
                    case secondsMaybe.begin of
                        Nothing ->
                            Nothing

                        Just begin ->
                            case secondsMaybe.end of
                                Nothing ->
                                    Nothing

                                Just end ->
                                    Just { begin = begin, end = end }
            in
            case maybeSeconds of
                Nothing ->
                    Nothing

                Just seconds ->
                    Just <|
                        Svg.rect
                            [ Svg.Attributes.x <| String.fromInt seconds.begin
                            , Svg.Attributes.y "0"
                            , Svg.Attributes.width <| String.fromInt <| seconds.end - seconds.begin
                            , Svg.Attributes.height "100%"
                            , Svg.Attributes.rx "0"
                            , Svg.Attributes.ry "0"
                            , Svg.Attributes.fill Mensam.Svg.Color.dark.red
                            , Svg.Attributes.opacity "0.4"
                            ]
                            []

        Mensam.Reservation.MkStatusCancelled ->
            Nothing


timeToSeconds : Mensam.Time.Time -> Int
timeToSeconds time =
    Mensam.Time.unSecond (Mensam.Time.unTime time).second
        + (60
            * (Mensam.Time.unMinute (Mensam.Time.unTime time).minute
                + (60
                    * Mensam.Time.unHour (Mensam.Time.unTime time).hour
                  )
              )
          )


deskRoom : Model -> Element.Element Message
deskRoom model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.clip
        , Element.Border.width 1
        , Element.Background.color <| Mensam.Element.Color.dark.white Mensam.Element.Color.Opaque100
        , Element.htmlAttribute <| Html.Attributes.style "contain" "size"
        ]
    <|
        Element.Window.view model.window
            { viewportAttributes = []
            , contentAttributes =
                [ Element.width <| Element.px 10000
                , Element.height <| Element.px 10000
                , Element.Window.onDown <| MessagePure << MessageWindow
                , Element.Window.onMove <| MessagePure << MessageWindow
                , Element.Window.onUp <| MessagePure << MessageWindow
                , Element.Border.width 2
                , Element.htmlAttribute <| Html.Attributes.style "touch-action" "pinch-zoom"
                ]
            }
        <|
            Element.el
                [ Element.centerX
                , Element.centerY
                ]
            <|
                Mensam.Room.drawRoom <|
                    Mensam.Room.MkRoom
                        { dimensions = { minX = -5000, minY = -5000, maxX = 5000, maxY = 5000 }
                        , drawingInstructions =
                            let
                                deskInstructions =
                                    List.map (Mensam.Room.instructDesk << .desk) model.desks
                            in
                            List.concat
                                [ [ Mensam.Room.instructGrid
                                  ]
                                , deskInstructions
                                ]
                        , messages =
                            -- TODO: Add useful messages.
                            { onClickTable =
                                \desk ->
                                    MessagePure <|
                                        ViewDetailed <|
                                            Just
                                                { desk = desk
                                                , dontViewUnlessMouseIsStillDragging = False
                                                }
                            , onEnterTable = Messages []
                            , onLeaveTable = Messages []
                            }
                        }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect
    | Messages (List Message) -- TODO: Maybe this recursion should be in a separate data type.


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
    | SetDesks
        (List
            { desk :
                { id : Mensam.Desk.Identifier
                , name : Mensam.Desk.Name
                , space : Mensam.Space.Identifier
                , location : Maybe Mensam.Desk.Location
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
    | AbortSelectionDragging
    | SetTimeFromSelectionDragging
    | ClosePopup
    | OpenDialogToLeave
    | CloseDialogToLeave
    | ViewDetailed
        (Maybe
            { desk :
                { id : Mensam.Desk.Identifier
                , name : Mensam.Desk.Name
                , space : Mensam.Space.Identifier
                , location : Maybe Mensam.Desk.Location
                }
            , dontViewUnlessMouseIsStillDragging : Bool
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
    | ViewDateGlobalPicker Bool
    | SetDateEndToDateBegin
    | SetDateEndOneAfterDateBegin
    | PreviousDay
    | NextDay
    | ResetDateBeginToSelection
    | ResetDateEndToSelection
    | SetTabView TabView
    | MessageWindow Element.Window.Message
    | SetTimetablePointer Element.Events.Pointer.Event
    | SetSelectorRegionDimensions { width : Float, height : Float }
    | SetSelectionDraggingStartFromPointer
    | SetSelectionDraggingKeepFromPointer


applyPureMessages : List MessagePure -> Model -> Model
applyPureMessages messages model =
    case messages of
        [] ->
            model

        m :: ms ->
            applyPureMessages ms <| updatePure m model


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetSpaceInfo space ->
            { model
                | space = space.id
                , name = space.name
                , roles = space.roles
                , timezone = space.timezone
                , visibility = space.visibility
                , yourRole = space.yourRole
            }

        SetDesks desks ->
            { model | desks = desks }

        SetSelected selection ->
            { model | selected = selection }

        AbortSelectionDragging ->
            { model | selectionDragging = Nothing }

        SetTimeFromSelectionDragging ->
            case model.selectionDragging of
                Nothing ->
                    model

                Just interval ->
                    let
                        orderedInterval =
                            if Mensam.Time.unHour interval.start <= Mensam.Time.unHour interval.end then
                                { begin = interval.start, end = interval.end }

                            else
                                { begin = interval.end, end = interval.start }

                        fixedBoundsInterval =
                            { begin = orderedInterval.begin
                            , end = Mensam.Time.MkHour <| modBy 24 <| Mensam.Time.unHour orderedInterval.end + 1
                            }

                        messages =
                            [ MessageTimeBegin <| Mensam.Widget.Time.SetHour fixedBoundsInterval.begin
                            , MessageTimeBegin <| Mensam.Widget.Time.SetMinute <| Mensam.Time.MkMinute 0
                            , MessageTimeEnd <| Mensam.Widget.Time.SetHour <| fixedBoundsInterval.end
                            , MessageTimeEnd <| Mensam.Widget.Time.SetMinute <| Mensam.Time.MkMinute 0
                            ]
                                ++ (if Mensam.Time.unHour fixedBoundsInterval.end < Mensam.Time.unHour orderedInterval.end then
                                        [ SetDateEndOneAfterDateBegin ]

                                    else
                                        []
                                   )
                                ++ [ ResetDateBeginToSelection, ResetDateEndToSelection ]
                    in
                    applyPureMessages messages model

        ClosePopup ->
            { model | popup = Nothing }

        OpenDialogToLeave ->
            { model | popup = Just <| PopupLeave }

        CloseDialogToLeave ->
            { model | popup = Nothing }

        ViewDetailed Nothing ->
            { model | popup = Nothing }

        ViewDetailed (Just data) ->
            { model
                | popup =
                    if data.dontViewUnlessMouseIsStillDragging then
                        case model.selectionDragging of
                            Nothing ->
                                Nothing

                            Just _ ->
                                Just <| PopupReservation { desk = data.desk, pickerVisibility = PickerInvisible }

                    else
                        Just <| PopupReservation { desk = data.desk, pickerVisibility = PickerInvisible }
            }

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

        ViewDateGlobalPicker visible ->
            { model | globalDatePickerVisible = visible }

        SetDateEndToDateBegin ->
            { model | modelDateEnd = model.modelDateBegin }

        SetDateEndOneAfterDateBegin ->
            { model
                | modelDateEnd =
                    let
                        oldModel =
                            Mensam.Widget.Date.unModel model.modelDateEnd
                    in
                    Mensam.Widget.Date.MkModel
                        { oldModel
                            | selected = Mensam.Time.nextDay (Mensam.Widget.Date.unModel model.modelDateBegin).selected
                        }
            }

        PreviousDay ->
            { model
                | modelDateBegin =
                    case model.modelDateBegin of
                        Mensam.Widget.Date.MkModel widgetModel ->
                            Mensam.Widget.Date.MkModel
                                { widgetModel
                                    | selected = Mensam.Time.previousDay widgetModel.selected
                                }
                , modelDateEnd =
                    case model.modelDateBegin of
                        Mensam.Widget.Date.MkModel widgetModel ->
                            Mensam.Widget.Date.MkModel
                                { widgetModel
                                    | selected = Mensam.Time.previousDay widgetModel.selected
                                }
            }

        NextDay ->
            { model
                | modelDateBegin =
                    case model.modelDateBegin of
                        Mensam.Widget.Date.MkModel widgetModel ->
                            Mensam.Widget.Date.MkModel
                                { widgetModel
                                    | selected = Mensam.Time.nextDay widgetModel.selected
                                }
                , modelDateEnd =
                    case model.modelDateBegin of
                        Mensam.Widget.Date.MkModel widgetModel ->
                            Mensam.Widget.Date.MkModel
                                { widgetModel
                                    | selected = Mensam.Time.nextDay widgetModel.selected
                                }
            }

        ResetDateBeginToSelection ->
            { model | modelDateBegin = Mensam.Widget.Date.resetDateToSelection model.modelDateBegin }

        ResetDateEndToSelection ->
            { model | modelDateEnd = Mensam.Widget.Date.resetDateToSelection model.modelDateEnd }

        SetTabView tab ->
            { model | tabView = tab }

        MessageWindow messageWindow ->
            { model | window = Element.Window.update messageWindow model.window }

        SetTimetablePointer event ->
            { model | timetablePointer = Just event }

        SetSelectorRegionDimensions dimensions ->
            { model | timetablePointerRegionDimensions = Just dimensions }

        SetSelectionDraggingStartFromPointer ->
            { model
                | selectionDragging =
                    case model.selectionDragging of
                        Just x ->
                            Just x

                        Nothing ->
                            case ( model.timetablePointer, model.timetablePointerRegionDimensions ) of
                                ( Just pointer, Just dimensions ) ->
                                    Just <|
                                        let
                                            hour =
                                                calculateHour pointer dimensions
                                        in
                                        { start = hour, end = hour }

                                _ ->
                                    Nothing
            }

        SetSelectionDraggingKeepFromPointer ->
            { model
                | selectionDragging =
                    case model.selectionDragging of
                        Just x ->
                            case ( model.timetablePointer, model.timetablePointerRegionDimensions ) of
                                ( Just pointer, Just dimensions ) ->
                                    Just <|
                                        let
                                            hour =
                                                calculateHour pointer dimensions
                                        in
                                        { start = x.start, end = hour }

                                _ ->
                                    Just x

                        Nothing ->
                            Nothing
            }


type MessageEffect
    = ReportError Mensam.Error.Error
    | GetSelectorRegionWidth String
    | RefreshSpace
    | RefreshDesks
    | OpenPageToJoin
    | OpenPageToUsers
    | OpenPageToSettings
    | OpenPageToDesks
    | SubmitLeave
    | SubmitReservation


spaceView : { jwt : Mensam.Auth.Bearer.Jwt, yourUserId : Mensam.User.Identifier } -> Model -> Cmd Message
spaceView auth model =
    Mensam.Api.SpaceView.request { jwt = auth.jwt, yourUserId = auth.yourUserId, id = model.space } <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceView.Success view) ->
                    case view.yourRole of
                        Nothing ->
                            MessageEffect OpenPageToJoin

                        Just _ ->
                            Messages
                                [ MessagePure <|
                                    SetSpaceInfo
                                        { id = view.id
                                        , name = view.name
                                        , roles = view.roles
                                        , timezone = view.timezone
                                        , visibility = view.visibility
                                        , yourRole = view.yourRole
                                        }
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
    Mensam.Api.DeskList.request { jwt = jwt, space = model.space, timeWindow = { start = Nothing, end = Nothing } } <|
        \result ->
            case result of
                Ok (Mensam.Api.DeskList.Success value) ->
                    Messages
                        [ MessagePure <| SetDesks value.desks
                        , case value.desks of
                            [] ->
                                Messages []

                            _ ->
                                -- TODO: Hardcoded HTML id.
                                MessageEffect <| GetSelectorRegionWidth "timetableRegion0"
                        ]

                Ok (Mensam.Api.DeskList.ErrorInsufficientPermission permission) ->
                    MessageEffect <| ReportError <| Mensam.Space.Role.errorInsufficientPermission permission

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


getSelectorRegionDimensionsTask : String -> Task.Task Browser.Dom.Error Message
getSelectorRegionDimensionsTask elementId =
    Task.map (\e -> MessagePure <| SetSelectorRegionDimensions { width = e.element.width, height = e.element.height }) <|
        Browser.Dom.getElement elementId


getSelectorRegionDimensionsCmd : String -> Cmd.Cmd Message
getSelectorRegionDimensionsCmd elementId =
    Task.attempt
        (\result ->
            case result of
                Ok m ->
                    m

                Err (Browser.Dom.NotFound _) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to get dimensions of timetable in HTML" <|
                                Mensam.Error.undefined
        )
    <|
        getSelectorRegionDimensionsTask elementId


calculateHour : Element.Events.Pointer.Event -> { width : Float, height : Float } -> Mensam.Time.Hour
calculateHour event dimensions =
    Mensam.Time.MkHour <| floor <| 24 * event.offsetPos.x / dimensions.width


reservationClashes :
    { timezone : Mensam.Time.Timezone
    , begin : Mensam.Time.Timestamp
    , end : Mensam.Time.Timestamp
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
    ->
        { beginClashes : Bool
        , endClashes : Bool
        }
reservationClashes input =
    let
        beginPosix =
            Mensam.Time.toPosix input.timezone input.begin

        endPosix =
            Mensam.Time.toPosix input.timezone input.end

        activeReservations =
            List.filter
                (\reservation ->
                    case reservation.status of
                        Mensam.Reservation.MkStatusPlanned ->
                            True

                        Mensam.Reservation.MkStatusCancelled ->
                            False
                )
                input.reservations

        isClashingAny =
            List.any
                (\reservation ->
                    Time.posixToMillis reservation.timeBegin < Time.posixToMillis endPosix && Time.posixToMillis reservation.timeEnd > Time.posixToMillis beginPosix
                )
                activeReservations

        isObviousClashingBegin =
            List.any
                (\reservation ->
                    Time.posixToMillis reservation.timeBegin <= Time.posixToMillis beginPosix && Time.posixToMillis reservation.timeEnd > Time.posixToMillis beginPosix
                )
                activeReservations

        isObviousClashingEnd =
            List.any
                (\reservation ->
                    Time.posixToMillis reservation.timeBegin < Time.posixToMillis endPosix && Time.posixToMillis reservation.timeEnd >= Time.posixToMillis endPosix
                )
                activeReservations
    in
    case ( isClashingAny, isObviousClashingBegin, isObviousClashingEnd ) of
        ( False, _, _ ) ->
            { beginClashes = False, endClashes = False }

        ( True, True, True ) ->
            { beginClashes = True, endClashes = True }

        ( True, True, False ) ->
            { beginClashes = True, endClashes = False }

        ( True, False, True ) ->
            { beginClashes = False, endClashes = True }

        ( True, False, False ) ->
            { beginClashes = True, endClashes = True }
