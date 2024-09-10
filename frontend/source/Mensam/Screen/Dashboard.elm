module Mensam.Screen.Dashboard exposing (..)

import Element
import Element.Background
import Element.Events.Pointer
import Element.Font
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import List.Extra
import Mensam.Api.ReservationCancel
import Mensam.Api.ReservationList
import Mensam.Api.SpaceList
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
import Mensam.User
import Time


type alias Model =
    { spaces : List Mensam.Space.Space
    , selectedSpace : Maybe Int
    , reservations :
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
    , selectedReservation : Maybe Int
    , timezone : Time.Zone
    , modelDateBegin : Mensam.Time.Date
    , modelDateEnd : Mensam.Time.Date
    , popup : Maybe PopupModel
    }


type PopupModel
    = PopupViewReservation Mensam.Reservation.Identifier


init : { time : { now : Time.Posix, zone : Time.Zone } } -> Model
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
            in
            { begin = (Mensam.Time.unTimestamp timestampNow).date
            , end = (Mensam.Time.unTimestamp timestampOneWeekFromNow).date
            }
    in
    { spaces = []
    , selectedSpace = Nothing
    , reservations = []
    , selectedReservation = Nothing
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
                , Element.spacing 10
                ]
                [ Element.column
                    [ Element.width Element.fill
                    , Element.height <| Element.fillPortion 1
                    ]
                    [ Element.row
                        [ Element.width Element.fill
                        , Element.height <| Element.px 45
                        , Element.padding 10
                        , Element.spacing 30
                        ]
                        [ Element.el
                            [ Element.Font.size 22
                            , Element.Font.hairline
                            , Element.alignBottom
                            , Element.alignLeft
                            ]
                          <|
                            Element.text "Your Spaces"
                        , Mensam.Element.Button.button <|
                            Mensam.Element.Button.MkButton
                                { attributes = [ Element.alignRight ]
                                , color = Mensam.Element.Button.Yellow
                                , label = Element.text "Browse"
                                , message = Just <| MessageEffect OpenPageToBrowseSpaces
                                , size = Mensam.Element.Button.Small
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
                            [ { header = Element.none
                              , width = Element.fill
                              , view =
                                    \n (Mensam.Space.MkSpace space) ->
                                        Element.el
                                            [ Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelectedSpace <| Just n
                                            , Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetSelectedSpace Nothing
                                            , Element.Events.Pointer.onClick <| \_ -> MessageEffect <| ChooseSpace space.id
                                            , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                            , let
                                                alpha =
                                                    case model.selectedSpace of
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
                , Element.column
                    [ Element.width Element.fill
                    , Element.height <| Element.fillPortion 2
                    ]
                    [ Element.row
                        [ Element.width Element.fill
                        , Element.height <| Element.px 45
                        , Element.padding 10
                        , Element.spacing 30
                        ]
                        [ Element.el
                            [ Element.Font.size 22
                            , Element.Font.hairline
                            , Element.alignBottom
                            , Element.alignLeft
                            ]
                          <|
                            Element.text "Upcoming Reservations"
                        , Mensam.Element.Button.button <|
                            Mensam.Element.Button.MkButton
                                { attributes = [ Element.alignRight ]
                                , color = Mensam.Element.Button.Yellow
                                , label = Element.text "More"
                                , message = Just <| MessageEffect OpenPageToViewReservations
                                , size = Mensam.Element.Button.Small
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
                            [ { header = Element.none
                              , width = Element.px 160
                              , view =
                                    \n entry ->
                                        Element.el
                                            (case entry.reservation.status of
                                                Mensam.Reservation.MkStatusPlanned ->
                                                    [ Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelectedReservation <| Just n
                                                    , Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetSelectedReservation Nothing
                                                    , Element.Events.Pointer.onClick <| \_ -> MessagePure <| ChooseReservation entry.reservation.id
                                                    , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                                    , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                                                    , let
                                                        alpha =
                                                            case model.selectedReservation of
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
                                                    [ Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelectedReservation <| Just n
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
                            , { header = Element.none
                              , width = Element.fill
                              , view =
                                    \n entry ->
                                        Element.el
                                            (case entry.reservation.status of
                                                Mensam.Reservation.MkStatusPlanned ->
                                                    [ Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelectedReservation <| Just n
                                                    , Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetSelectedReservation Nothing
                                                    , Element.Events.Pointer.onClick <| \_ -> MessagePure <| ChooseReservation entry.reservation.id
                                                    , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                                    , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                                                    , let
                                                        alpha =
                                                            case model.selectedReservation of
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
                                                    [ Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelectedReservation <| Just n
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
                ]
        , popup =
            case model.popup of
                Nothing ->
                    Nothing

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
                                                        Mensam.Time.fromPosix (Mensam.Time.timezone entry.space.timezone) entry.reservation.timeBegin
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
                                                        Mensam.Time.fromPosix (Mensam.Time.timezone entry.space.timezone) entry.reservation.timeEnd
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
                                                    Mensam.Time.unTimezoneIdentifier <|
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
                                                , label = Element.text "Go back"
                                                , message = Just <| MessagePure <| ClosePopup
                                                , size = Mensam.Element.Button.Medium
                                                }
                                        , Mensam.Element.Button.button <|
                                            Mensam.Element.Button.MkButton
                                                { attributes = [ Element.width Element.fill ]
                                                , color = Mensam.Element.Button.Red
                                                , label = Element.text "Cancel Reservation"
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
    = SetSpaces (List Mensam.Space.Space)
    | SetSelectedSpace (Maybe Int)
    | SetReservations
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
    | SetSelectedReservation (Maybe Int)
    | ChooseReservation Mensam.Reservation.Identifier
    | ClosePopup


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetSpaces spaces ->
            { model | spaces = spaces }

        SetSelectedSpace selection ->
            { model | selectedSpace = selection }

        SetReservations reservations ->
            { model | reservations = reservations }

        SetSelectedReservation selection ->
            { model | selectedReservation = selection }

        ChooseReservation id ->
            { model | popup = Just <| PopupViewReservation id }

        ClosePopup ->
            { model | popup = Nothing }


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshSpaces
    | ChooseSpace Mensam.Space.Identifier
    | RefreshReservations
    | CancelReservation Mensam.Reservation.Identifier
    | OpenPageToBrowseSpaces
    | OpenPageToViewReservations


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


spaceList : Mensam.Auth.Bearer.Jwt -> Cmd Message
spaceList jwt =
    Mensam.Api.SpaceList.request { jwt = jwt, order = [], member = Just True } <|
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


reservationList : { jwt : Mensam.Auth.Bearer.Jwt, model : Model } -> Cmd Message
reservationList argument =
    Mensam.Api.ReservationList.request
        { jwt = argument.jwt
        , timeWindow =
            { start =
                Just <|
                    Mensam.Time.toPosix argument.model.timezone <|
                        Mensam.Time.MkTimestamp
                            { date = argument.model.modelDateBegin
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
                            { date = argument.model.modelDateEnd
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
