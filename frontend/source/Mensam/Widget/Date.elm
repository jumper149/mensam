module Mensam.Widget.Date exposing (..)

import Element
import Element.Background
import Element.Events
import Html.Attributes
import Mensam.Time
import Mensam.Widget.Month
import Time


type Model
    = MkModel
        { year : Mensam.Time.Year
        , month : Mensam.Time.Month
        , selected : Mensam.Time.Date
        }


unModel : Model -> { year : Mensam.Time.Year, month : Mensam.Time.Month, selected : Mensam.Time.Date }
unModel (MkModel value) =
    value


type Message
    = PreviousMonth
    | NextMonth
    | ClickDay Mensam.Time.Day


messageMonthWidget : Mensam.Widget.Month.Message -> Message
messageMonthWidget message =
    case message of
        Mensam.Widget.Month.PreviousMonth ->
            PreviousMonth

        Mensam.Widget.Month.NextMonth ->
            NextMonth


elementPickDay : Model -> Element.Element Message
elementPickDay (MkModel model) =
    let
        daysPre =
            let
                weekdayOfFirst =
                    Time.toWeekday Time.utc <|
                        Mensam.Time.toPosix Time.utc <|
                            Mensam.Time.MkTimestamp
                                { date =
                                    Mensam.Time.MkDate
                                        { year = model.year
                                        , month = model.month
                                        , day = Mensam.Time.MkDay 1
                                        }
                                , time =
                                    Mensam.Time.MkTime
                                        { hour = Mensam.Time.MkHour 12
                                        , minute = Mensam.Time.MkMinute 0
                                        , second = Mensam.Time.MkSecond 0
                                        }
                                }

                count =
                    case weekdayOfFirst of
                        Time.Mon ->
                            7

                        Time.Tue ->
                            1

                        Time.Wed ->
                            2

                        Time.Thu ->
                            3

                        Time.Fri ->
                            4

                        Time.Sat ->
                            5

                        Time.Sun ->
                            6
            in
            List.repeat count Nothing

        daysMiddle =
            List.map (Just << Mensam.Time.MkDay) <| List.range 1 (Mensam.Time.daysInMonth model.year model.month)

        daysPost =
            List.repeat 14 Nothing

        days =
            List.take (7 * 6) <| daysPre ++ daysMiddle ++ daysPost

        toListsOfSeven : List a -> List (List a)
        toListsOfSeven xs =
            if List.length xs > 7 then
                List.take 7 xs :: toListsOfSeven (List.drop 7 xs)

            else
                [ xs ]

        dayMatrix =
            toListsOfSeven days

        elementDay : Maybe Mensam.Time.Day -> Element.Element Message
        elementDay maybeDay =
            Element.el
                [ Element.width <| Element.px 33
                , Element.height <| Element.px 33
                , Element.padding 2
                ]
            <|
                case maybeDay of
                    Nothing ->
                        Element.el
                            [ Element.width Element.fill
                            , Element.height Element.fill
                            ]
                        <|
                            Element.el
                                [ Element.centerX
                                , Element.centerY
                                ]
                            <|
                                Element.none

                    Just day ->
                        Element.el
                            [ Element.width Element.fill
                            , Element.height Element.fill
                            , Element.Background.color <|
                                case model.selected of
                                    Mensam.Time.MkDate date ->
                                        if
                                            date.year
                                                == model.year
                                                && date.month
                                                == model.month
                                                && date.day
                                                == day
                                        then
                                            Element.rgba 1 1 1 0.05

                                        else
                                            Element.rgba 1 1 1 0
                            , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                            , Element.mouseOver
                                [ Element.Background.color <| Element.rgba 1 1 1 0.1
                                ]
                            , Element.Events.onClick <| ClickDay day
                            ]
                        <|
                            Element.el
                                [ Element.centerX
                                , Element.centerY
                                ]
                            <|
                                Element.text <|
                                    Mensam.Time.dayToString day
    in
    Element.el
        [ Element.width <| Element.px 231
        , Element.height <| Element.px 198
        , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
        ]
    <|
        Element.column
            []
        <|
            List.map
                (Element.row
                    []
                    << List.map elementDay
                )
                dayMatrix


updateDateNextMonth : Model -> Model
updateDateNextMonth (MkModel model) =
    case Mensam.Time.unMonth model.month of
        Time.Jan ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Feb }

        Time.Feb ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Mar }

        Time.Mar ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Apr }

        Time.Apr ->
            MkModel { model | month = Mensam.Time.MkMonth Time.May }

        Time.May ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Jun }

        Time.Jun ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Jul }

        Time.Jul ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Aug }

        Time.Aug ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Sep }

        Time.Sep ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Oct }

        Time.Oct ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Nov }

        Time.Nov ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Dec }

        Time.Dec ->
            MkModel
                { model
                    | year = Mensam.Time.MkYear <| Mensam.Time.unYear model.year + 1
                    , month = Mensam.Time.MkMonth Time.Jan
                }


updateDatePreviousMonth : Model -> Model
updateDatePreviousMonth (MkModel model) =
    case Mensam.Time.unMonth model.month of
        Time.Jan ->
            MkModel
                { model
                    | year = Mensam.Time.MkYear <| Mensam.Time.unYear model.year - 1
                    , month = Mensam.Time.MkMonth Time.Dec
                }

        Time.Feb ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Jan }

        Time.Mar ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Feb }

        Time.Apr ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Mar }

        Time.May ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Apr }

        Time.Jun ->
            MkModel { model | month = Mensam.Time.MkMonth Time.May }

        Time.Jul ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Jun }

        Time.Aug ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Jul }

        Time.Sep ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Aug }

        Time.Oct ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Sep }

        Time.Nov ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Oct }

        Time.Dec ->
            MkModel { model | month = Mensam.Time.MkMonth Time.Nov }


elementPickDate : Model -> Element.Element Message
elementPickDate (MkModel model) =
    Element.column
        [ Element.spacing 5
        , Element.width <| Element.px 231
        ]
        [ Element.el
            [ Element.width Element.fill
            ]
          <|
            Element.el
                [ Element.centerX
                ]
            <|
                Element.map messageMonthWidget <|
                    Mensam.Widget.Month.elementPickMonth <|
                        Mensam.Widget.Month.MkModel
                            { year = model.year
                            , month = model.month
                            }
        , Element.el
            [ Element.width Element.fill
            ]
          <|
            Element.el
                [ Element.centerX
                ]
            <|
                elementPickDay <|
                    MkModel model
        ]
