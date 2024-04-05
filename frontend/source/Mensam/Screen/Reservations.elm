module Mensam.Screen.Reservations exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Mensam.Api.ReservationList
import Mensam.Auth.Bearer
import Mensam.Desk
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
                , status : String -- TODO
                , timeBegin : Time.Posix
                , timeEnd : Time.Posix
                }
            , space :
                { id : Mensam.Space.Identifier
                , name : Mensam.Space.Name
                }
            , user :
                { id : Mensam.User.Identifier
                }
            }
    , selected : Maybe Int
    , timezone : Time.Zone
    , modelDateBegin : Mensam.Widget.Date.Model
    , modelDateEnd : Mensam.Widget.Date.Model
    }


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
                    []
                , Element.indexedTable
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.Background.color (Element.rgba 0 0 0 0.1)
                    , Element.Font.family [ Mensam.Element.Font.condensed ]
                    , Element.Font.size 16
                    , Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
                    ]
                    { data = model.reservations
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
                                            Element.text "Start"
                          , width = Element.px 100
                          , view =
                                \n entry ->
                                    Element.el
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
                                    <|
                                        cell <|
                                            Element.el
                                                [ Element.width <| Element.maximum 100 <| Element.fill ]
                                            <|
                                                Element.text <|
                                                    Mensam.Time.timestampToString <|
                                                        -- TODO: Use space's timezone!
                                                        Mensam.Time.fromPosix model.timezone entry.reservation.timeBegin
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
                                            Element.text "Space"
                          , width = Element.fill
                          , view =
                                \n entry ->
                                    Element.el
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
                                    <|
                                        cell <|
                                            Element.el
                                                [ Element.width <| Element.maximum 100 <| Element.fill ]
                                            <|
                                                Element.text <|
                                                    Mensam.Space.nameToString entry.space.name
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
                                    <|
                                        cell <|
                                            Element.el
                                                [ Element.width <| Element.maximum 100 <| Element.fill ]
                                            <|
                                                Element.text <|
                                                    Mensam.Desk.nameToString entry.desk.name
                          }
                        ]
                    }
                ]
        , popup = Nothing
        }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect


type MessagePure
    = SetReservations
        (List
            { desk :
                { id : Mensam.Desk.Identifier
                , name : Mensam.Desk.Name
                }
            , reservation :
                { id : Mensam.Reservation.Identifier
                , status : String -- TODO
                , timeBegin : Time.Posix
                , timeEnd : Time.Posix
                }
            , space :
                { id : Mensam.Space.Identifier
                , name : Mensam.Space.Name
                }
            , user :
                { id : Mensam.User.Identifier
                }
            }
        )
    | SetSelected (Maybe Int)
    | ChooseReservation Mensam.Reservation.Identifier


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetReservations reservations ->
            { model | reservations = reservations }

        SetSelected selection ->
            { model | selected = selection }

        ChooseReservation id ->
            -- TODO
            model


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshReservations


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
