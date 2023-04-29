module Mensam.Time exposing (..)

import Element
import Element.Events
import Html.Attributes
import Time
import Time.Extra


type Day
    = MkDay Int


unDay : Day -> Int
unDay (MkDay n) =
    n - 1


type Month
    = MkMonth Time.Month


unMonth : Month -> Time.Month
unMonth (MkMonth n) =
    n


type Year
    = MkYear Int


unYear : Year -> Int
unYear (MkYear n) =
    n


type Date
    = MkDate { year : Year, month : Month, day : Day }


unDate : Date -> { year : Year, month : Month, day : Day }
unDate (MkDate x) =
    x


type Hour
    = MkHour Int


unHour : Hour -> Int
unHour (MkHour n) =
    n


type Minute
    = MkMinute Int


unMinute : Minute -> Int
unMinute (MkMinute n) =
    n


type Second
    = MkSecond Int


unSecond : Second -> Int
unSecond (MkSecond n) =
    n


type Time
    = MkTime { hour : Hour, minute : Minute, second : Second }


unTime : Time -> { hour : Hour, minute : Minute, second : Second }
unTime (MkTime x) =
    x


type Timestamp
    = MkTimestamp { date : Date, time : Time }


unTimestamp : Timestamp -> { date : Date, time : Time }
unTimestamp (MkTimestamp x) =
    x


fromPosix : Time.Zone -> Time.Posix -> Timestamp
fromPosix zone posix =
    let
        parts =
            Time.Extra.posixToParts zone posix
    in
    MkTimestamp
        { date =
            MkDate
                { year = MkYear parts.year
                , month = MkMonth parts.month
                , day = MkDay parts.day
                }
        , time =
            MkTime
                { hour = MkHour parts.hour
                , minute = MkMinute parts.minute
                , second = MkSecond parts.second
                }
        }


toPosix : Time.Zone -> Timestamp -> Time.Posix
toPosix zone timestamp =
    let
        parts =
            { year = unYear (unDate (unTimestamp timestamp).date).year
            , month = unMonth (unDate (unTimestamp timestamp).date).month
            , day = unDay (unDate (unTimestamp timestamp).date).day
            , hour = unHour (unTime (unTimestamp timestamp).time).hour
            , minute = unMinute (unTime (unTimestamp timestamp).time).minute
            , second = unSecond (unTime (unTimestamp timestamp).time).second
            , millisecond = 0
            }
    in
    Time.Extra.partsToPosix zone parts


isLeapYear : Year -> Bool
isLeapYear (MkYear year) =
    (modBy 4 year == 0) && ((modBy 100 year /= 0) || (modBy 400 year == 0))


daysInMonth : Year -> Month -> Int
daysInMonth year (MkMonth month) =
    case month of
        Time.Jan ->
            31

        Time.Feb ->
            if isLeapYear year then
                29

            else
                28

        Time.Mar ->
            31

        Time.Apr ->
            30

        Time.May ->
            31

        Time.Jun ->
            30

        Time.Jul ->
            31

        Time.Aug ->
            31

        Time.Sep ->
            30

        Time.Oct ->
            31

        Time.Nov ->
            30

        Time.Dec ->
            31


type MessageMonth
    = MonthNext
    | MonthPrevious


elementPickMonth : Year -> Month -> Element.Element MessageMonth
elementPickMonth year month =
    Element.el
        [ Element.width <| Element.px 230
        , Element.height <| Element.px 40
        , Element.spaceEvenly
        ]
    <|
        Element.row
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ Element.el
                [ Element.width <| Element.px 40
                , Element.height <| Element.px 40
                , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                , Element.Events.onClick MonthPrevious
                ]
              <|
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                <|
                    Element.text "<"
            , Element.el
                [ Element.width <| Element.fill
                , Element.height <| Element.fill
                ]
              <|
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                <|
                    Element.text <|
                        yearToString year
                            ++ ", "
                            ++ monthToString month
            , Element.el
                [ Element.width <| Element.px 40
                , Element.height <| Element.px 40
                , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                , Element.Events.onClick MonthNext
                ]
              <|
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                <|
                    Element.text ">"
            ]


type MessageDay
    = ClickDay Day


elementPickDay : Year -> Month -> Element.Element MessageDay
elementPickDay year month =
    let
        weekdayOfFirst =
            Time.toWeekday Time.utc <|
                toPosix Time.utc <|
                    MkTimestamp
                        { date =
                            MkDate
                                { year = year
                                , month = month
                                , day = MkDay 1
                                }
                        , time =
                            MkTime
                                { hour = MkHour 12
                                , minute = MkMinute 0
                                , second = MkSecond 0
                                }
                        }

        daysPre =
            let
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
            List.map (Just << MkDay << (\n -> n + 1)) <| List.range 0 (daysInMonth year month)

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

        elementDay : Maybe Day -> Element.Element MessageDay
        elementDay maybeDay =
            Element.el
                [ Element.width <| Element.px 33
                , Element.height <| Element.px 33
                , Element.padding 5
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
                            , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                            , Element.Events.onClick <| ClickDay day
                            ]
                        <|
                            Element.el
                                [ Element.centerX
                                , Element.centerY
                                ]
                            <|
                                Element.text <|
                                    String.fromInt <|
                                        unDay day
    in
    Element.column
        []
    <|
        List.map
            (Element.row
                []
                << List.map elementDay
            )
            dayMatrix


yearToString : Year -> String
yearToString =
    String.fromInt << unYear


monthToString : Month -> String
monthToString (MkMonth month) =
    case month of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"
