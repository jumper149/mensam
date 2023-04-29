module Mensam.Time exposing (..)

import Time


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


fromPosix : Time.Zone -> Time.Posix -> Date
fromPosix zone posix =
    MkDate
        { year = MkYear <| Time.toYear zone posix
        , month = MkMonth <| Time.toMonth zone posix
        , day = MkDay <| Time.toDay zone posix
        }


toPosixUtc : Time.Zone -> Timestamp -> Time.Posix
toPosixUtc zone timestamp =
    let
        secondsToMillis seconds =
            1000 * seconds

        minutesToMillis minutes =
            secondsToMillis 60 * minutes

        hoursToMillis hours =
            minutesToMillis 60 * hours

        daysToMillis days =
            hoursToMillis 24 * days

        isLeapYear (MkYear n) =
            (modBy 4 n == 0) && ((modBy 100 n /= 0) || (modBy 400 n == 0))

        daysUntilMonth year (MkMonth n) =
            case n of
                Time.Jan ->
                    0

                Time.Feb ->
                    31 + daysUntilMonth year (MkMonth Time.Jan)

                Time.Mar ->
                    (if isLeapYear year then
                        28

                     else
                        29
                    )
                        + daysUntilMonth year (MkMonth Time.Feb)

                Time.Apr ->
                    31 + daysUntilMonth year (MkMonth Time.Mar)

                Time.May ->
                    30 + daysUntilMonth year (MkMonth Time.Apr)

                Time.Jun ->
                    31 + daysUntilMonth year (MkMonth Time.May)

                Time.Jul ->
                    30 + daysUntilMonth year (MkMonth Time.Jun)

                Time.Aug ->
                    31 + daysUntilMonth year (MkMonth Time.Jul)

                Time.Sep ->
                    31 + daysUntilMonth year (MkMonth Time.Aug)

                Time.Oct ->
                    30 + daysUntilMonth year (MkMonth Time.Sep)

                Time.Nov ->
                    31 + daysUntilMonth year (MkMonth Time.Oct)

                Time.Dec ->
                    30 + daysUntilMonth year (MkMonth Time.Nov)

        daysUntilYear year =
            case year of
                MkYear 1970 ->
                    0

                MkYear n ->
                    (if isLeapYear year then
                        365

                     else
                        366
                    )
                        + daysUntilYear (MkYear <| n - 1)
    in
    Time.millisToPosix <|
        List.sum
            [ secondsToMillis <| unSecond (unTime (unTimestamp timestamp).time).second
            , minutesToMillis <| unMinute (unTime (unTimestamp timestamp).time).minute
            , hoursToMillis <| unHour (unTime (unTimestamp timestamp).time).hour
            , daysToMillis <| unDay (unDate (unTimestamp timestamp).date).day
            , daysToMillis <|
                daysUntilMonth
                    (unDate (unTimestamp timestamp).date).year
                    (unDate (unTimestamp timestamp).date).month
            , daysToMillis <| daysUntilYear <| (unDate (unTimestamp timestamp).date).year
            ]
