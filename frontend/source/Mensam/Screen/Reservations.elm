module Mensam.Screen.Reservations exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Element.Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Mensam.Api.ReservationCancel
import Mensam.Api.ReservationList
import Mensam.Auth.Bearer
import Mensam.Desk
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Element.Screen
import Mensam.Error
import Mensam.Reservation
import Mensam.Space
import Mensam.Time
import Mensam.User
import Mensam.Widget.Date
import Time


type alias Model =
    { reservations :
        List
            { desk :
                { id : Mensam.Desk.Identifier
                , name : Mensam.Desk.Name
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
                , timezone : Mensam.Time.TimezoneIdentifier
                , owner : Mensam.User.Identifier
                }
            , user :
                { id : Mensam.User.Identifier
                }
            }
    , selected : Maybe Int
    , timezone : Time.Zone
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


init : { time : { now : Time.Posix, zone : Time.Zone } } -> Model
init value =
    let
        initialDateModel =
            let
                timestamp =
                    Mensam.Time.fromPosix value.time.zone value.time.now
            in
            Mensam.Widget.Date.MkModel
                { year = (Mensam.Time.unDate (Mensam.Time.unTimestamp timestamp).date).year
                , month = (Mensam.Time.unDate (Mensam.Time.unTimestamp timestamp).date).month
                , selected = (Mensam.Time.unTimestamp timestamp).date
                }
    in
    { reservations = []
    , selected = Nothing
    , timezone = value.time.zone
    , modelDateBegin = initialDateModel
    , modelDateEnd = initialDateModel
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
                    ]
                , Element.indexedTable
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.Background.color (Element.rgba 0 0 0 0.1)
                    , Element.Font.family [ Mensam.Element.Font.condensed ]
                    , Element.Font.size 16
                    , Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
                    , Element.clipY
                    , Element.scrollbarY
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
                                                [ Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                                                , Element.Events.onClick <| MessagePure <| ChooseReservation entry.reservation.id
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

                                            Mensam.Reservation.MkStatusCancelled ->
                                                [ Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                                                , Element.Background.color (Element.rgba 1 0 0 0.2)
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
                                                                Mensam.Time.fromPosix (Mensam.Time.timezone entry.space.timezone) entry.reservation.timeBegin
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
                                                                Mensam.Time.fromPosix (Mensam.Time.timezone entry.space.timezone) entry.reservation.timeEnd
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
                                                [ Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                                                , Element.Events.onClick <| MessagePure <| ChooseReservation entry.reservation.id
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

                                            Mensam.Reservation.MkStatusCancelled ->
                                                [ Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                                                , Element.Background.color (Element.rgba 1 0 0 0.2)
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
                            , Element.Input.button
                                [ Element.Background.color Mensam.Element.Color.bright.yellow
                                , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.green ]
                                , Element.Font.color Mensam.Element.Color.dark.black
                                , Element.width Element.fill
                                , Element.padding 10
                                ]
                                { onPress = Just <| MessageEffect <| SetDateRange
                                , label =
                                    Element.el
                                        [ Element.centerX
                                        , Element.centerY
                                        , Element.Font.family [ Mensam.Element.Font.condensed ]
                                        , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                        ]
                                    <|
                                        Element.text "Set date boundary"
                                }
                            ]

                Just (PopupViewReservation reservationId) ->
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
                                    { onPress = Just <| MessagePure <| ClosePopup
                                    , label =
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            , Element.Font.family [ Mensam.Element.Font.condensed ]
                                            , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                            ]
                                        <|
                                            Element.text "Go back"
                                    }
                                , Element.Input.button
                                    [ Element.Background.color Mensam.Element.Color.bright.red
                                    , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.white ]
                                    , Element.Font.color Mensam.Element.Color.dark.black
                                    , Element.width Element.fill
                                    , Element.padding 10
                                    ]
                                    { onPress = Just <| MessageEffect <| CancelReservation reservationId
                                    , label =
                                        Element.el
                                            [ Element.centerX
                                            , Element.centerY
                                            , Element.Font.family [ Mensam.Element.Font.condensed ]
                                            , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                            ]
                                        <|
                                            Element.text "Cancel reservation"
                                    }
                                ]
                            ]
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
                , timezone : Mensam.Time.TimezoneIdentifier
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


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshReservations
    | SetDateRange
    | CancelReservation Mensam.Reservation.Identifier


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


reservationList : { jwt : Mensam.Auth.Bearer.Jwt, model : Model } -> Cmd Message
reservationList argument =
    Mensam.Api.ReservationList.request
        { jwt = argument.jwt
        , timeBegin =
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
        , timeEnd =
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


reservationCancel : { jwt : Mensam.Auth.Bearer.Jwt, id : Mensam.Reservation.Identifier } -> Cmd Message
reservationCancel argument =
    Mensam.Api.ReservationCancel.request
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
