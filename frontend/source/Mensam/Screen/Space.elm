module Mensam.Screen.Space exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Element.Input
import Html.Attributes
import Mensam.Api.DeskList
import Mensam.Api.ReservationCreate
import Mensam.Auth.Bearer
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Error
import Mensam.Time
import Time


type alias Model =
    { space : { id : Int }
    , desks :
        List
            { desk :
                { id : Int
                , name : String
                , space : Int
                }
            , reservations :
                List
                    { desk : Int
                    , id : Int
                    , status : String
                    , timeBegin : Time.Posix
                    , timeEnd : Time.Posix
                    , user : Int
                    }
            }
    , selected : Maybe Int
    , viewDetailed :
        Maybe
            { desk :
                { id : Int
                , name : String
                , space : Int
                }
            }
    , time :
        { now : Time.Posix
        , zone : Time.Zone
        }
    , pickerVisibility : PickerVisibility
    , modelDate : Mensam.Time.ModelDate
    , dateSelected : Mensam.Time.Date
    , timeSelected : Mensam.Time.Time
    }


type PickerVisibility
    = PickerInvisible
    | DatePickerVisible
    | TimePickerVisible


init : { id : Int, time : { now : Time.Posix, zone : Time.Zone } } -> Model
init args =
    { space = { id = args.id }
    , desks = []
    , selected = Nothing
    , viewDetailed = Nothing
    , time =
        { now = args.time.now
        , zone = args.time.zone
        }
    , modelDate =
        let
            date =
                (Mensam.Time.unTimestamp <| Mensam.Time.fromPosix args.time.zone args.time.now).date
        in
        Mensam.Time.MkModelDate
            { year = (Mensam.Time.unDate date).year
            , month = (Mensam.Time.unDate date).month
            , selected = [ (Mensam.Time.unDate date).day ]
            }
    , pickerVisibility = PickerInvisible
    , dateSelected = (Mensam.Time.unTimestamp <| Mensam.Time.fromPosix args.time.zone args.time.now).date
    , timeSelected =
        Mensam.Time.MkTime
            { hour = Mensam.Time.MkHour 12
            , minute = Mensam.Time.MkMinute 0
            , second = Mensam.Time.MkSecond 0
            }
    }


element : Model -> Element.Element Message
element model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 10
        , Element.Background.color (Element.rgba 0 0 0 0.1)
        , Element.Font.size 16
        , Element.Font.family [ Mensam.Element.Font.condensed ]
        , Element.inFront <|
            case model.viewDetailed of
                Nothing ->
                    Element.none

                Just _ ->
                    Element.el
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.paddingXY 30 30
                        ]
                    <|
                        Element.el
                            [ Element.Background.color Mensam.Element.Color.bright.black
                            , Element.centerX
                            , Element.width <| Element.maximum 500 <| Element.fill
                            , Element.height <| Element.px 465
                            , Element.paddingXY 30 30
                            ]
                        <|
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
                                        { onPress = Just <| MessagePure <| ViewDatePicker
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
                                                            Mensam.Time.unDate model.dateSelected
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
                                        { onPress = Just <| MessagePure <| ViewTimePicker
                                        , label =
                                            Element.el
                                                [ Element.centerX
                                                , Element.centerY
                                                , Element.Font.family [ Mensam.Element.Font.condensed ]
                                                , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                                ]
                                            <|
                                                Element.text <|
                                                    Mensam.Time.timeToString model.timeSelected
                                        }
                                    ]
                                , Element.el
                                    [ Element.width Element.fill
                                    ]
                                  <|
                                    case model.pickerVisibility of
                                        DatePickerVisible ->
                                            Element.el
                                                [ Element.centerX
                                                ]
                                            <|
                                                Element.map (MessagePure << PickDate) <|
                                                    Mensam.Time.elementPickDate model.modelDate

                                        TimePickerVisible ->
                                            Element.el
                                                [ Element.centerX
                                                ]
                                            <|
                                                Element.map (MessagePure << PickTime) <|
                                                    Mensam.Time.elementPickTime model.timeSelected

                                        PickerInvisible ->
                                            Element.none
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
        ]
    <|
        Element.indexedTable
            [ Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
            ]
            { data = model.desks
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
                        \n x ->
                            Element.el
                                [ Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
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
                                            String.fromInt x.desk.id
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
                                            x.desk.name
                  }
                ]
            }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect


type MessagePure
    = SetDesks
        (List
            { desk :
                { id : Int
                , name : String
                , space : Int
                }
            , reservations :
                List
                    { desk : Int
                    , id : Int
                    , status : String
                    , timeBegin : Time.Posix
                    , timeEnd : Time.Posix
                    , user : Int
                    }
            }
        )
    | SetSelected (Maybe Int)
    | ViewDetailed
        (Maybe
            { desk :
                { id : Int
                , name : String
                , space : Int
                }
            }
        )
    | ViewDatePicker
    | ViewTimePicker
    | PickDate Mensam.Time.MessageDate
    | PickTime Mensam.Time.MessageTime


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetDesks desks ->
            { model | desks = desks }

        SetSelected selection ->
            { model | selected = selection }

        ViewDetailed data ->
            { model | viewDetailed = data }

        ViewDatePicker ->
            { model | pickerVisibility = DatePickerVisible }

        ViewTimePicker ->
            { model | pickerVisibility = TimePickerVisible }

        PickDate (Mensam.Time.MessageMonth Mensam.Time.MonthNext) ->
            { model | modelDate = Mensam.Time.updateDateNextMonth model.modelDate }

        PickDate (Mensam.Time.MessageMonth Mensam.Time.MonthPrevious) ->
            { model | modelDate = Mensam.Time.updateDatePreviousMonth model.modelDate }

        PickDate (Mensam.Time.MessageDay (Mensam.Time.ClickDay day)) ->
            let
                (Mensam.Time.MkModelDate modelDate) =
                    model.modelDate
            in
            { model
                | modelDate = Mensam.Time.MkModelDate { modelDate | selected = [ day ] }
                , dateSelected =
                    Mensam.Time.MkDate
                        { year = modelDate.year
                        , month = modelDate.month
                        , day = day
                        }
            }

        PickTime (Mensam.Time.SetHour hour) ->
            { model
                | timeSelected =
                    let
                        (Mensam.Time.MkTime timeSelected) =
                            model.timeSelected
                    in
                    Mensam.Time.MkTime { timeSelected | hour = hour }
            }

        PickTime (Mensam.Time.SetMinute minute) ->
            { model
                | timeSelected =
                    let
                        (Mensam.Time.MkTime timeSelected) =
                            model.timeSelected
                    in
                    Mensam.Time.MkTime { timeSelected | minute = minute }
            }


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshDesks
    | SubmitReservation


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


reservationCreate : Mensam.Auth.Bearer.Jwt -> Model -> { desk : { id : Int } } -> Cmd Message
reservationCreate jwt model { desk } =
    Mensam.Api.ReservationCreate.request
        { jwt = jwt
        , desk = desk
        , timeWindow =
            { start =
                Mensam.Time.toPosix model.time.zone <|
                    Mensam.Time.MkTimestamp
                        { date = model.dateSelected
                        , time = model.timeSelected
                        }
            , end =
                Mensam.Time.toPosix model.time.zone <|
                    Mensam.Time.MkTimestamp
                        { date = model.dateSelected
                        , time = model.timeSelected
                        }
            }
        }
    <|
        \result ->
            case result of
                Ok (Mensam.Api.ReservationCreate.Success _) ->
                    MessagePure <| ViewDetailed Nothing

                Ok Mensam.Api.ReservationCreate.ErrorTimeUnavailable ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Bad request" <|
                                Mensam.Error.message "Requested time unavailable" <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.ReservationCreate.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Bad request body" <|
                                Mensam.Error.message error <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.ReservationCreate.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <| ReportError <| Mensam.Error.http error
