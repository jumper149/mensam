module Mensam.Screen.Space exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Element.Input
import Html.Attributes
import Mensam.Api.DeskList
import Mensam.Api.ReservationCreate
import Mensam.Api.SpaceView
import Mensam.Auth.Bearer
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Element.Screen
import Mensam.Error
import Mensam.Space
import Mensam.Time
import Set
import Time


type alias Model =
    { space : Mensam.Space.Identifier
    , name : Mensam.Space.Name
    , timezone : Time.Zone
    , visibility : Mensam.Space.Visibility
    , accessibility : Mensam.Space.Accessibility
    , permissions : Set.Set String
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
    , pickerVisibility : PickerVisibility
    , modelDate : Mensam.Time.ModelDate
    , dateSelected : Mensam.Time.Date
    , timeSelected : Mensam.Time.Time
    }


type PickerVisibility
    = PickerInvisible
    | DatePickerVisible
    | TimePickerVisible


init : { id : Mensam.Space.Identifier, time : { now : Time.Posix, zone : Time.Zone } } -> Model
init args =
    { space = args.id
    , name = Mensam.Space.MkName ""
    , timezone = Time.utc
    , visibility = Mensam.Space.MkVisibilityHidden
    , accessibility = Mensam.Space.MkAccessibilityInaccessible
    , permissions = Set.empty
    , desks = []
    , selected = Nothing
    , viewDetailed = Nothing
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
    Mensam.Element.Screen.element
        { main =
            Element.indexedTable
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Background.color (Element.rgba 0 0 0 0.1)
                , Element.Font.family [ Mensam.Element.Font.condensed ]
                , Element.Font.size 16
                , Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
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
        , popup =
            model.viewDetailed
                |> (Maybe.map <|
                        \_ ->
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
                   )
        }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect


type MessagePure
    = SetSpaceInfo Mensam.Space.SpaceView
    | SetDesks
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
        SetSpaceInfo (Mensam.Space.MkSpaceView space) ->
            { model
                | name = space.name
                , visibility = space.visibility
                , accessibility = space.accessibility
                , permissions = space.permissions
            }

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
    | RefreshSpace
    | RefreshDesks
    | SubmitReservation


spaceView : Mensam.Auth.Bearer.Jwt -> Model -> Cmd Message
spaceView jwt model =
    Mensam.Api.SpaceView.request { jwt = jwt, id = model.space } <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceView.Success value) ->
                    MessagePure <| SetSpaceInfo value.space

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
                Mensam.Time.toPosix model.timezone <|
                    Mensam.Time.MkTimestamp
                        { date = model.dateSelected
                        , time = model.timeSelected
                        }
            , end =
                Mensam.Time.toPosix model.timezone <|
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
