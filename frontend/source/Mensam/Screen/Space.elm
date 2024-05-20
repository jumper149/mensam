module Mensam.Screen.Space exposing (..)

import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import List.Extra
import Mensam.Api.DeskList
import Mensam.Api.ReservationCreate
import Mensam.Api.SpaceLeave
import Mensam.Api.SpaceView
import Mensam.Auth.Bearer
import Mensam.Desk
import Mensam.Element.Button
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
import Svg
import Svg.Attributes
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
    , globalDatePickerVisible : Bool
    }


type PopupModel
    = PopupLeave
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
    , globalDatePickerVisible = False
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
                                    , label = Element.text "Join"
                                    , message = Just <| MessageEffect OpenPageToJoin
                                    }

                        Just _ ->
                            Mensam.Element.Button.button <|
                                Mensam.Element.Button.MkButton
                                    { attributes = [ Element.alignRight, Element.centerY ]
                                    , color = Mensam.Element.Button.Red
                                    , label = Element.text "Leave"
                                    , message = Just <| MessagePure OpenDialogToLeave
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
                                            , label = Element.text "Settings"
                                            , message = Just <| MessageEffect OpenPageToSettings
                                            }
                    , Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = [ Element.alignRight, Element.centerY ]
                            , color = Mensam.Element.Button.Yellow
                            , label = Element.text "Users"
                            , message = Just <| MessageEffect OpenPageToUsers
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
                                            , label = Element.text "Desks"
                                            , message = Just <| MessageEffect OpenPageToDesks
                                            }
                    ]
                , deskTimetable model
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
                                            ]
                                    )
                                <|
                                    Mensam.Widget.Date.elementPickDate model.modelDateBegin

                          else
                            Element.none
                        , Mensam.Element.Button.button <|
                            Mensam.Element.Button.MkButton
                                { attributes = [ Element.width Element.fill ]
                                , color = Mensam.Element.Button.Blue
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
                                }
                        ]
                    ]
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
                                [ Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Blue
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
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Blue
                                        , label =
                                            let
                                                time =
                                                    case model.modelTimeBegin of
                                                        Mensam.Widget.Time.MkModel { selected } ->
                                                            selected
                                            in
                                            Element.text <| Mensam.Time.timeToString time
                                        , message = Just <| MessagePure ViewTimeBeginPicker
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
                                [ Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Blue
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
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Blue
                                        , label =
                                            let
                                                time =
                                                    case model.modelTimeEnd of
                                                        Mensam.Widget.Time.MkModel { selected } ->
                                                            selected
                                            in
                                            Element.text <| Mensam.Time.timeToString time
                                        , message = Just <| MessagePure ViewTimeEndPicker
                                        }
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
                                        , message = Just <| MessagePure <| ViewDetailed Nothing
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Yellow
                                        , label = Element.text "Submit"
                                        , message = Just <| MessageEffect <| SubmitReservation
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
                                []
                                [ Element.text "Are you sure you want to leave this space?"
                                ]
                            , Element.paragraph
                                []
                                [ Element.text "When leaving a space you lose all permissions you had for this space."
                                ]
                            , Element.paragraph
                                []
                                [ Element.text "Your reservations will remain untouched."
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
                                        , message = Just <| MessagePure CloseDialogToLeave
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Red
                                        , label = Element.text "Abandon space"
                                        , message = Just <| MessageEffect SubmitLeave
                                        }
                                ]
                            ]
        , closePopup = MessagePure ClosePopup
        }


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
                            , Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
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
                                    [ Element.Font.size 12
                                    ]
                                    [ Element.text <| Mensam.Desk.identifierToString x.desk.id
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
                            , Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                            , Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
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
                                            [ Element.width Element.fill
                                            , Element.height Element.fill
                                            , Element.Events.onClick <|
                                                Messages
                                                    [ MessagePure <| MessageTimeBegin <| Mensam.Widget.Time.SetHour <| Mensam.Time.MkHour piece.hour
                                                    , MessagePure <| MessageTimeBegin <| Mensam.Widget.Time.SetMinute <| Mensam.Time.MkMinute 0
                                                    , MessagePure <| ViewDetailed <| Just { desk = x.desk }
                                                    ]
                                            , Element.mouseOver
                                                [ Element.Background.color (Element.rgba 0 1 0 0.1)
                                                ]
                                            ]
                                        <|
                                            Element.el
                                                [ Element.width Element.fill
                                                , Element.height <| Element.px piece.height
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
                                    [ { hour = 0, height = 40 }
                                    , { hour = 1, height = 12 }
                                    , { hour = 2, height = 12 }
                                    , { hour = 3, height = 18 }
                                    , { hour = 4, height = 12 }
                                    , { hour = 5, height = 12 }
                                    , { hour = 6, height = 25 }
                                    , { hour = 7, height = 12 }
                                    , { hour = 8, height = 12 }
                                    , { hour = 9, height = 18 }
                                    , { hour = 10, height = 12 }
                                    , { hour = 11, height = 12 }
                                    , { hour = 12, height = 40 }
                                    , { hour = 13, height = 12 }
                                    , { hour = 14, height = 12 }
                                    , { hour = 15, height = 18 }
                                    , { hour = 16, height = 12 }
                                    , { hour = 17, height = 12 }
                                    , { hour = 18, height = 25 }
                                    , { hour = 19, height = 12 }
                                    , { hour = 20, height = 12 }
                                    , { hour = 21, height = 18 }
                                    , { hour = 22, height = 12 }
                                    , { hour = 23, height = 12 }
                                    ]
              }
            ]
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
    Time.Zone
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
                    , Svg.Attributes.fill "red"
                    , Svg.Attributes.opacity "0.2"
                    ]
                    []


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
        , timezone : Mensam.Time.TimezoneIdentifier
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
    | ViewDateGlobalPicker Bool
    | SetDateEndToDateBegin


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetSpaceInfo space ->
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

        ViewDateGlobalPicker visible ->
            { model | globalDatePickerVisible = visible }

        SetDateEndToDateBegin ->
            { model | modelDateEnd = model.modelDateBegin }


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshSpace
    | RefreshDesks
    | OpenPageToJoin
    | OpenPageToUsers
    | OpenPageToSettings
    | OpenPageToDesks
    | SubmitLeave
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
    Mensam.Api.DeskList.request { jwt = jwt, space = model.space } <|
        \result ->
            case result of
                Ok (Mensam.Api.DeskList.Success value) ->
                    MessagePure <| SetDesks value.desks

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
