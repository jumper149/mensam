module Mensam.Screen.Reservations exposing (..)

import Element
import Element.Background
import Element.Events.Pointer
import Element.Font
import Html.Attributes
import List.Extra
import Mensam.Api.ReservationCancel
import Mensam.Api.ReservationList
import Mensam.Auth.Bearer
import Mensam.Desk
import Mensam.Element.Button
import Mensam.Element.Font
import Mensam.Element.Screen
import Mensam.Error
import Mensam.Reservation
import Mensam.Space
import Mensam.Space.Role
import Mensam.Time
import Mensam.Http.Tracker
import Mensam.Url
import Mensam.User
import Mensam.Widget.Date
import Time


type alias Model =
    { reservations :
        List
            { desk :
                { id : Mensam.Desk.Identifier
                , name : Mensam.Desk.Name
                , location : Maybe Mensam.Desk.Location
                }
            , reservation :
                { id : Mensam.Reservation.Identifier
                , status : Mensam.Reservation.Status
                , timeBegin : Time.Posix
                , timeEnd : Time.Posix
                }
            , space :
                { id : Mensam.Space.Identifier
                , name : Mensam.Space.Name
                , timezone : Mensam.Time.Timezone
                , owner : Mensam.User.Identifier
                }
            , user :
                { id : Mensam.User.Identifier
                }
            }
    , selected : Maybe Int
    , timezone : Mensam.Time.Timezone
    , modelDateBegin : Mensam.Widget.Date.Model
    , modelDateEnd : Mensam.Widget.Date.Model
    , popup : Maybe PopupModel
    }


type PopupModel
    = PopupDate DateBeginEnd
    | PopupViewReservation Mensam.Reservation.Identifier


type DateBeginEnd
    = DateBegin
    | DateEnd


init : { time : { now : Time.Posix, zone : Mensam.Time.Timezone } } -> Model
init value =
    let
        initialDateModel =
            let
                timestampNow =
                    Mensam.Time.fromPosix value.time.zone value.time.now

                timestampOneWeekFromNow =
                    let
                        weekInMillis =
                            1000 * 60 * 60 * 24 * 7
                    in
                    Mensam.Time.fromPosix value.time.zone <| Time.millisToPosix <| Time.posixToMillis value.time.now + weekInMillis

                timestampToDateModel =
                    \timestamp ->
                        Mensam.Widget.Date.MkModel
                            { year = (Mensam.Time.unDate (Mensam.Time.unTimestamp timestamp).date).year
                            , month = (Mensam.Time.unDate (Mensam.Time.unTimestamp timestamp).date).month
                            , selected = (Mensam.Time.unTimestamp timestamp).date
                            }
            in
            { begin = timestampToDateModel timestampNow
            , end = timestampToDateModel timestampOneWeekFromNow
            }
    in
    { reservations = []
    , selected = Nothing
    , timezone = value.time.zone
    , modelDateBegin = initialDateModel.begin
    , modelDateEnd = initialDateModel.end
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
                    , Element.paddingXY 0 6
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
                                Element.row [ Element.spacing 4 ]
                                    [ Element.el
                                        [ Element.Font.size 12
                                        , Element.alignLeft
                                        , Element.alignTop
                                        ]
                                      <|
                                        Element.text "from"
                                    , Element.el
                                        [ Element.Font.size 16
                                        , Element.alignRight
                                        , Element.centerY
                                        ]
                                      <|
                                        Element.text <|
                                            String.concat
                                                [ Mensam.Time.yearToString date.year
                                                , ", "
                                                , Mensam.Time.monthToString date.month
                                                , " "
                                                , Mensam.Time.dayToString date.day
                                                ]
                                    ]
                            , message = Just <| MessagePure ViewDateBeginPicker
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
                                        case model.modelDateEnd of
                                            Mensam.Widget.Date.MkModel { selected } ->
                                                Mensam.Time.unDate selected
                                in
                                Element.row [ Element.spacing 4 ]
                                    [ Element.el
                                        [ Element.Font.size 12
                                        , Element.alignLeft
                                        , Element.alignTop
                                        ]
                                      <|
                                        Element.text "to"
                                    , Element.el
                                        [ Element.Font.size 16
                                        , Element.alignRight
                                        , Element.centerY
                                        ]
                                      <|
                                        Element.text <|
                                            String.concat
                                                [ Mensam.Time.yearToString date.year
                                                , ", "
                                                , Mensam.Time.monthToString date.month
                                                , " "
                                                , Mensam.Time.dayToString date.day
                                                ]
                                    ]
                            , message = Just <| MessagePure ViewDateEndPicker
                            , size = Mensam.Element.Button.Medium
                            }
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
                    { data = model.reservations
                    , columns =
                        let
                            cell =
                                Element.el
                                    [ Element.height <| Element.px 50
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
                                            Element.text "Time"
                          , width = Element.px 160
                          , view =
                                \n entry ->
                                    Element.el
                                        (case entry.reservation.status of
                                            Mensam.Reservation.MkStatusPlanned ->
                                                [ Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelected <| Just n
                                                , Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetSelected Nothing
                                                , Element.Events.Pointer.onClick <| \_ -> MessagePure <| ChooseReservation entry.reservation.id
                                                , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                                , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
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

                                            Mensam.Reservation.MkStatusCancelled ->
                                                [ Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelected <| Just n
                                                , Element.Background.color (Element.rgba 1 0 0 0.2)
                                                , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                                                ]
                                        )
                                    <|
                                        cell <|
                                            Element.column
                                                [ Element.width <| Element.maximum 160 <| Element.fill ]
                                                [ Element.row [ Element.alignRight, Element.spacing 3 ]
                                                    [ Element.el
                                                        [ Element.Font.size 10
                                                        , Element.alignBottom
                                                        , Element.padding 1
                                                        ]
                                                      <|
                                                        Element.text "from"
                                                    , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Light300 ] <|
                                                        Element.text <|
                                                            Mensam.Time.timestampToString <|
                                                                Mensam.Time.fromPosix entry.space.timezone entry.reservation.timeBegin
                                                    ]
                                                , Element.row [ Element.alignRight, Element.spacing 3 ]
                                                    [ Element.el
                                                        [ Element.Font.size 10
                                                        , Element.alignBottom
                                                        , Element.padding 1
                                                        ]
                                                      <|
                                                        Element.text "to"
                                                    , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Light300 ] <|
                                                        Element.text <|
                                                            Mensam.Time.timestampToString <|
                                                                Mensam.Time.fromPosix entry.space.timezone entry.reservation.timeEnd
                                                    ]
                                                ]
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
                                            Element.text "Desk"
                          , width = Element.fill
                          , view =
                                \n entry ->
                                    Element.el
                                        (case entry.reservation.status of
                                            Mensam.Reservation.MkStatusPlanned ->
                                                [ Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelected <| Just n
                                                , Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetSelected Nothing
                                                , Element.Events.Pointer.onClick <| \_ -> MessagePure <| ChooseReservation entry.reservation.id
                                                , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                                , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
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

                                            Mensam.Reservation.MkStatusCancelled ->
                                                [ Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelected <| Just n
                                                , Element.Background.color (Element.rgba 1 0 0 0.2)
                                                , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                                                ]
                                        )
                                    <|
                                        cell <|
                                            Element.column
                                                [ Element.width <| Element.maximum 150 <| Element.fill ]
                                                [ Element.row [ Element.alignLeft, Element.spacing 3 ]
                                                    [ Element.el
                                                        [ Element.Font.size 10
                                                        , Element.alignBottom
                                                        , Element.padding 1
                                                        ]
                                                      <|
                                                        Element.text "Space"
                                                    , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Light300 ] <|
                                                        Element.text <|
                                                            Mensam.Space.nameToString entry.space.name
                                                    ]
                                                , Element.row [ Element.alignLeft, Element.spacing 3 ]
                                                    [ Element.el
                                                        [ Element.Font.size 10
                                                        , Element.alignBottom
                                                        , Element.padding 1
                                                        ]
                                                      <|
                                                        Element.text "Desk"
                                                    , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Light300 ] <|
                                                        Element.text <|
                                                            Mensam.Desk.nameToString entry.desk.name
                                                    ]
                                                ]
                          }
                        ]
                    }
                ]
        , popup =
            case model.popup of
                Nothing ->
                    Nothing

                Just (PopupDate dateBeginEnd) ->
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
                                Element.text <|
                                    case dateBeginEnd of
                                        DateBegin ->
                                            "Earliest Date"

                                        DateEnd ->
                                            "Latest Date"
                            , Element.el
                                [ Element.width Element.fill
                                , Element.height Element.fill
                                ]
                              <|
                                case dateBeginEnd of
                                    DateBegin ->
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            ]
                                        <|
                                            Element.map (MessagePure << MessageDateBegin) <|
                                                Mensam.Widget.Date.elementPickDate model.modelDateBegin

                                    DateEnd ->
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            ]
                                        <|
                                            Element.map (MessagePure << MessageDateEnd) <|
                                                Mensam.Widget.Date.elementPickDate model.modelDateEnd
                            , Mensam.Element.Button.button <|
                                Mensam.Element.Button.MkButton
                                    { attributes = [ Element.width Element.fill ]
                                    , color = Mensam.Element.Button.Yellow
                                    , enabled = True
                                    , label = Element.text "Set date boundary"
                                    , message = Just <| MessageEffect SetDateRange
                                    , size = Mensam.Element.Button.Medium
                                    }
                            ]

                Just (PopupViewReservation reservationId) ->
                    case List.Extra.find (\entry -> entry.reservation.id == reservationId) <| model.reservations of
                        -- This case should never occur. The user cannot click on a reservation that doesn't exist.
                        Nothing ->
                            Nothing

                        Just entry ->
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
                                        Element.text "Reservation"
                                    , Element.column
                                        [ Element.centerX
                                        , Element.spacing 10
                                        ]
                                        [ Element.row [ Element.alignRight, Element.spacing 3 ]
                                            [ Element.el
                                                [ Element.Font.size 12
                                                , Element.alignBottom
                                                , Element.padding 1
                                                ]
                                              <|
                                                Element.text "from"
                                            , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Regular400 ] <|
                                                Element.text <|
                                                    Mensam.Time.timestampToString <|
                                                        Mensam.Time.fromPosix entry.space.timezone entry.reservation.timeBegin
                                            ]
                                        , Element.row [ Element.alignRight, Element.spacing 3 ]
                                            [ Element.el
                                                [ Element.Font.size 12
                                                , Element.alignBottom
                                                , Element.padding 1
                                                ]
                                              <|
                                                Element.text "to"
                                            , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Regular400 ] <|
                                                Element.text <|
                                                    Mensam.Time.timestampToString <|
                                                        Mensam.Time.fromPosix entry.space.timezone entry.reservation.timeEnd
                                            ]
                                        , Element.row [ Element.alignLeft, Element.spacing 3 ]
                                            [ Element.el
                                                [ Element.Font.size 12
                                                , Element.alignBottom
                                                , Element.padding 1
                                                ]
                                              <|
                                                Element.text "Timezone"
                                            , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Regular400 ] <|
                                                Element.text <|
                                                    Mensam.Time.timezoneToString <|
                                                        entry.space.timezone
                                            ]
                                        , Element.row [ Element.alignLeft, Element.spacing 3 ]
                                            [ Element.el
                                                [ Element.Font.size 12
                                                , Element.alignBottom
                                                , Element.padding 1
                                                ]
                                              <|
                                                Element.text "Status"
                                            , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Regular400 ] <|
                                                Element.text <|
                                                    Mensam.Reservation.statusToString <|
                                                        entry.reservation.status
                                            ]
                                        ]
                                    , Element.column
                                        [ Element.centerX
                                        , Element.spacing 10
                                        ]
                                        [ Element.row [ Element.alignLeft, Element.spacing 3 ]
                                            [ Element.el
                                                [ Element.Font.size 12
                                                , Element.alignBottom
                                                , Element.padding 1
                                                ]
                                              <|
                                                Element.text "Space"
                                            , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Regular400 ] <|
                                                Element.text <|
                                                    Mensam.Space.nameToString entry.space.name
                                            ]
                                        , Element.row [ Element.alignLeft, Element.spacing 3 ]
                                            [ Element.el
                                                [ Element.Font.size 12
                                                , Element.alignBottom
                                                , Element.padding 1
                                                ]
                                              <|
                                                Element.text "Desk"
                                            , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Regular400 ] <|
                                                Element.text <|
                                                    Mensam.Desk.nameToString <|
                                                        entry.desk.name
                                            ]
                                        ]
                                    , Element.paragraph
                                        [ Element.alignBottom
                                        , Element.centerX
                                        , Mensam.Element.Font.fontWeight Mensam.Element.Font.Light300
                                        ]
                                        [ Element.text "Do you want to cancel the reservation?"
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
                                                , label = Element.text "Go back"
                                                , message = Just <| MessagePure ClosePopup
                                                , size = Mensam.Element.Button.Medium
                                                }
                                        , Mensam.Element.Button.button <|
                                            Mensam.Element.Button.MkButton
                                                { attributes = [ Element.width Element.fill ]
                                                , color = Mensam.Element.Button.Red
                                                , enabled = True
                                                , label = Element.text "Cancel reservation"
                                                , message = Just <| MessageEffect <| CancelReservation reservationId
                                                , size = Mensam.Element.Button.Medium
                                                }
                                        ]
                                    ]
        , closePopup = MessagePure ClosePopup
        }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect
    | Messages (List Message)


type MessagePure
    = SetReservations
        (List
            { desk :
                { id : Mensam.Desk.Identifier
                , name : Mensam.Desk.Name
                , location : Maybe Mensam.Desk.Location
                }
            , reservation :
                { id : Mensam.Reservation.Identifier
                , status : Mensam.Reservation.Status
                , timeBegin : Time.Posix
                , timeEnd : Time.Posix
                }
            , space :
                { id : Mensam.Space.Identifier
                , name : Mensam.Space.Name
                , timezone : Mensam.Time.Timezone
                , owner : Mensam.User.Identifier
                }
            , user :
                { id : Mensam.User.Identifier
                }
            }
        )
    | SetSelected (Maybe Int)
    | ChooseReservation Mensam.Reservation.Identifier
    | ClosePopup
    | ViewDateBeginPicker
    | ViewDateEndPicker
    | MessageDateBegin Mensam.Widget.Date.Message
    | MessageDateEnd Mensam.Widget.Date.Message
    | FixLowDateBoundDegenerate
    | FixHighDateBoundDegenerate


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetReservations reservations ->
            { model | reservations = reservations }

        SetSelected selection ->
            { model | selected = selection }

        ChooseReservation id ->
            { model | popup = Just <| PopupViewReservation id }

        ClosePopup ->
            { model | popup = Nothing }

        ViewDateBeginPicker ->
            { model | popup = Just <| PopupDate DateBegin }

        ViewDateEndPicker ->
            { model | popup = Just <| PopupDate DateEnd }

        MessageDateBegin Mensam.Widget.Date.NextMonth ->
            { model | modelDateBegin = Mensam.Widget.Date.updateDateNextMonth model.modelDateBegin }

        MessageDateBegin Mensam.Widget.Date.PreviousMonth ->
            { model | modelDateBegin = Mensam.Widget.Date.updateDatePreviousMonth model.modelDateBegin }

        MessageDateBegin (Mensam.Widget.Date.ClickDay day) ->
            let
                (Mensam.Widget.Date.MkModel modelDate) =
                    model.modelDateBegin
            in
            updatePure FixHighDateBoundDegenerate <|
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

        MessageDateEnd Mensam.Widget.Date.NextMonth ->
            { model | modelDateEnd = Mensam.Widget.Date.updateDateNextMonth model.modelDateEnd }

        MessageDateEnd Mensam.Widget.Date.PreviousMonth ->
            { model | modelDateEnd = Mensam.Widget.Date.updateDatePreviousMonth model.modelDateEnd }

        MessageDateEnd (Mensam.Widget.Date.ClickDay day) ->
            let
                (Mensam.Widget.Date.MkModel modelDate) =
                    model.modelDateEnd
            in
            updatePure FixLowDateBoundDegenerate <|
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

        FixLowDateBoundDegenerate ->
            case Mensam.Time.compareDate (Mensam.Widget.Date.unModel model.modelDateBegin).selected (Mensam.Widget.Date.unModel model.modelDateEnd).selected of
                LT ->
                    model

                EQ ->
                    model

                GT ->
                    let
                        (Mensam.Widget.Date.MkModel modelDateBegin) =
                            model.modelDateBegin

                        (Mensam.Widget.Date.MkModel modelDateEnd) =
                            model.modelDateEnd
                    in
                    { model
                        | modelDateBegin =
                            Mensam.Widget.Date.MkModel
                                { modelDateBegin
                                    | year = (Mensam.Time.unDate modelDateEnd.selected).year
                                    , month = (Mensam.Time.unDate modelDateEnd.selected).month
                                    , selected = modelDateEnd.selected
                                }
                    }

        FixHighDateBoundDegenerate ->
            case Mensam.Time.compareDate (Mensam.Widget.Date.unModel model.modelDateBegin).selected (Mensam.Widget.Date.unModel model.modelDateEnd).selected of
                LT ->
                    model

                EQ ->
                    model

                GT ->
                    let
                        (Mensam.Widget.Date.MkModel modelDateBegin) =
                            model.modelDateBegin

                        (Mensam.Widget.Date.MkModel modelDateEnd) =
                            model.modelDateEnd
                    in
                    { model
                        | modelDateEnd =
                            Mensam.Widget.Date.MkModel
                                { modelDateEnd
                                    | year = (Mensam.Time.unDate modelDateBegin.selected).year
                                    , month = (Mensam.Time.unDate modelDateBegin.selected).month
                                    , selected = modelDateBegin.selected
                                }
                    }


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshReservations
    | SetDateRange
    | CancelReservation Mensam.Reservation.Identifier


reservationList : Maybe Mensam.Http.Tracker.Tracker -> Mensam.Url.BaseUrl -> { jwt : Mensam.Auth.Bearer.Jwt, model : Model } -> Cmd Message
reservationList tracker baseUrl argument =
    Mensam.Api.ReservationList.request tracker
        baseUrl
        { jwt = argument.jwt
        , timeWindow =
            { start =
                Just <|
                    Mensam.Time.toPosix argument.model.timezone <|
                        Mensam.Time.MkTimestamp
                            { date = (Mensam.Widget.Date.unModel argument.model.modelDateBegin).selected
                            , time =
                                Mensam.Time.MkTime
                                    { hour = Mensam.Time.MkHour 0
                                    , minute = Mensam.Time.MkMinute 0
                                    , second = Mensam.Time.MkSecond 0
                                    }
                            }
            , end =
                Just <|
                    Mensam.Time.toPosix argument.model.timezone <|
                        Mensam.Time.MkTimestamp
                            { date = (Mensam.Widget.Date.unModel argument.model.modelDateEnd).selected
                            , time =
                                Mensam.Time.MkTime
                                    { hour = Mensam.Time.MkHour 23
                                    , minute = Mensam.Time.MkMinute 59
                                    , second = Mensam.Time.MkSecond 59
                                    }
                            }
            }
        }
    <|
        \result ->
            case result of
                Ok (Mensam.Api.ReservationList.Success value) ->
                    MessagePure <| SetReservations value.reservations

                Ok (Mensam.Api.ReservationList.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting reservations failed" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.ReservationList.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting reservations failed" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting reservations failed" <|
                                Mensam.Error.http error


reservationCancel : Maybe Mensam.Http.Tracker.Tracker -> Mensam.Url.BaseUrl -> { jwt : Mensam.Auth.Bearer.Jwt, id : Mensam.Reservation.Identifier } -> Cmd Message
reservationCancel tracker baseUrl argument =
    Mensam.Api.ReservationCancel.request tracker
        baseUrl
        { jwt = argument.jwt
        , id = argument.id
        }
    <|
        \result ->
            case result of
                Ok Mensam.Api.ReservationCancel.Success ->
                    Messages
                        [ MessagePure ClosePopup
                        , MessageEffect RefreshReservations
                        ]

                Ok (Mensam.Api.ReservationCancel.ErrorInsufficientPermission permission) ->
                    MessageEffect <| ReportError <| Mensam.Space.Role.errorInsufficientPermission permission

                Ok Mensam.Api.ReservationCancel.ErrorAlreadyCancelled ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Cancelling reservation failed" <|
                                Mensam.Error.message "Reservation is already cancelled" <|
                                    Mensam.Error.undefined

                Ok Mensam.Api.ReservationCancel.ErrorAlreadyHappened ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Cancelling reservation failed" <|
                                Mensam.Error.message "Reservation already happened in the past" <|
                                    Mensam.Error.message "It is not possible to cancel fulfilled reservations" <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.ReservationCancel.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Cancelling reservation failed" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.ReservationCancel.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting reservations failed" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting reservations failed" <|
                                Mensam.Error.http error
