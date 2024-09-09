module Mensam.Time exposing (..)

import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Time
import Time.Extra
import TimeZone.Extra


type TimezoneIdentifier
    = MkTimezoneIdentifier String


timezone : TimezoneIdentifier -> Time.Zone
timezone (MkTimezoneIdentifier name) =
    case Dict.get name TimeZone.Extra.zones of
        Nothing ->
            Time.utc

        Just toZone ->
            toZone ()


mkTimezone : String -> Maybe ( TimezoneIdentifier, Time.Zone )
mkTimezone name =
    Maybe.map (\toZone -> ( MkTimezoneIdentifier name, toZone () )) <| Dict.get name TimeZone.Extra.zones


unTimezoneIdentifier : TimezoneIdentifier -> String
unTimezoneIdentifier (MkTimezoneIdentifier name) =
    name


timezoneIdentifierEncode : TimezoneIdentifier -> Encode.Value
timezoneIdentifierEncode =
    Encode.string << unTimezoneIdentifier


timezoneIdentifierDecoder : Decode.Decoder TimezoneIdentifier
timezoneIdentifierDecoder =
    Decode.andThen
        (\string ->
            case mkTimezone string of
                Nothing ->
                    Decode.fail <| "Trying to decode time zone database identifier, but can't recognize: " ++ string

                Just ( timezoneIdentifier, _ ) ->
                    Decode.succeed timezoneIdentifier
        )
        Decode.string


type Day
    = MkDay Int


unDay : Day -> Int
unDay (MkDay n) =
    n


compareDay : Day -> Day -> Order
compareDay (MkDay x) (MkDay y) =
    compare x y


type Month
    = MkMonth Time.Month


unMonth : Month -> Time.Month
unMonth (MkMonth n) =
    n


compareMonth : Month -> Month -> Order
compareMonth x y =
    compare (monthToNumber x) (monthToNumber y)


type Year
    = MkYear Int


unYear : Year -> Int
unYear (MkYear n) =
    n


compareYear : Year -> Year -> Order
compareYear (MkYear x) (MkYear y) =
    compare x y


type Date
    = MkDate { year : Year, month : Month, day : Day }


unDate : Date -> { year : Year, month : Month, day : Day }
unDate (MkDate x) =
    x


compareDate : Date -> Date -> Order
compareDate (MkDate x) (MkDate y) =
    case compareYear x.year y.year of
        LT ->
            LT

        EQ ->
            case compareMonth x.month y.month of
                LT ->
                    LT

                EQ ->
                    case compareDay x.day y.day of
                        LT ->
                            LT

                        EQ ->
                            EQ

                        GT ->
                            GT

                GT ->
                    GT

        GT ->
            GT


type Hour
    = MkHour Int


unHour : Hour -> Int
unHour (MkHour n) =
    n


compareHour : Hour -> Hour -> Order
compareHour (MkHour x) (MkHour y) =
    compare x y


type Minute
    = MkMinute Int


unMinute : Minute -> Int
unMinute (MkMinute n) =
    n


compareMinute : Minute -> Minute -> Order
compareMinute (MkMinute x) (MkMinute y) =
    compare x y


type Second
    = MkSecond Int


unSecond : Second -> Int
unSecond (MkSecond n) =
    n


compareSecond : Second -> Second -> Order
compareSecond (MkSecond x) (MkSecond y) =
    compare x y


type Time
    = MkTime { hour : Hour, minute : Minute, second : Second }


unTime : Time -> { hour : Hour, minute : Minute, second : Second }
unTime (MkTime x) =
    x


compareTime : Time -> Time -> Order
compareTime (MkTime x) (MkTime y) =
    case compareHour x.hour y.hour of
        LT ->
            LT

        EQ ->
            case compareMinute x.minute x.minute of
                LT ->
                    LT

                EQ ->
                    case compareSecond x.second x.second of
                        LT ->
                            LT

                        EQ ->
                            EQ

                        GT ->
                            GT

                GT ->
                    GT

        GT ->
            GT


type Timestamp
    = MkTimestamp { date : Date, time : Time }


unTimestamp : Timestamp -> { date : Date, time : Time }
unTimestamp (MkTimestamp x) =
    x


compareTimestamp : Timestamp -> Timestamp -> Order
compareTimestamp (MkTimestamp x) (MkTimestamp y) =
    case compareDate x.date y.date of
        LT ->
            LT

        EQ ->
            case compareTime x.time x.time of
                LT ->
                    LT

                EQ ->
                    EQ

                GT ->
                    GT

        GT ->
            GT


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


yearToString : Year -> String
yearToString =
    String.fromInt << unYear


{-| The number of a month in a year indexed starting at `1`.
-}
monthToNumber : Month -> Int
monthToNumber (MkMonth month) =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


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


dayToString : Day -> String
dayToString (MkDay n) =
    String.fromInt n


dateToString : Date -> String
dateToString (MkDate date) =
    let
        addPadding2 =
            (\x -> "00" ++ x) >> String.reverse >> String.toList >> List.take 2 >> String.fromList >> String.reverse

        addPadding4 =
            (\x -> "0000" ++ x) >> String.reverse >> String.toList >> List.take 4 >> String.fromList >> String.reverse
    in
    String.concat
        [ addPadding4 <| yearToString date.year
        , "-"
        , addPadding2 <| String.fromInt <| monthToNumber date.month
        , "-"
        , addPadding2 <| dayToString date.day
        ]


timeToString : Time -> String
timeToString (MkTime time) =
    let
        addPadding =
            (\x -> "00" ++ x) >> String.reverse >> String.toList >> List.take 2 >> String.fromList >> String.reverse
    in
    String.concat
        [ addPadding <| String.fromInt <| unHour time.hour
        , ":"
        , addPadding <| String.fromInt <| unMinute time.minute
        , ":"
        , addPadding <| String.fromInt <| unSecond time.second
        ]


timestampToString : Timestamp -> String
timestampToString (MkTimestamp timestamp) =
    dateToString timestamp.date ++ " " ++ timeToString timestamp.time


previousMonth : Month -> Month
previousMonth (MkMonth month) =
    MkMonth <|
        case month of
            Time.Jan ->
                Time.Dec

            Time.Feb ->
                Time.Jan

            Time.Mar ->
                Time.Feb

            Time.Apr ->
                Time.Mar

            Time.May ->
                Time.Apr

            Time.Jun ->
                Time.May

            Time.Jul ->
                Time.Jun

            Time.Aug ->
                Time.Jul

            Time.Sep ->
                Time.Aug

            Time.Oct ->
                Time.Sep

            Time.Nov ->
                Time.Oct

            Time.Dec ->
                Time.Nov


nextMonth : Month -> Month
nextMonth (MkMonth month) =
    MkMonth <|
        case month of
            Time.Jan ->
                Time.Feb

            Time.Feb ->
                Time.Mar

            Time.Mar ->
                Time.Apr

            Time.Apr ->
                Time.May

            Time.May ->
                Time.Jun

            Time.Jun ->
                Time.Jul

            Time.Jul ->
                Time.Aug

            Time.Aug ->
                Time.Sep

            Time.Sep ->
                Time.Oct

            Time.Oct ->
                Time.Nov

            Time.Nov ->
                Time.Dec

            Time.Dec ->
                Time.Jan


previousDay : Date -> Date
previousDay (MkDate date) =
    let
        isFirstDayOfMonth =
            unDay date.day == 1

        isFirstDayOfYear =
            isFirstDayOfMonth && unMonth date.month == Time.Jan

        newYear =
            if isFirstDayOfYear then
                MkYear <| unYear date.year - 1

            else
                date.year

        newMonth =
            if isFirstDayOfMonth then
                previousMonth date.month

            else
                date.month

        newDay =
            if isFirstDayOfMonth then
                MkDay <| daysInMonth newYear newMonth

            else
                MkDay <| unDay date.day - 1
    in
    MkDate
        { year = newYear
        , month = newMonth
        , day = newDay
        }


nextDay : Date -> Date
nextDay (MkDate date) =
    let
        lastDayOfMonth =
            daysInMonth date.year date.month

        isLastDayOfMonth =
            unDay date.day == lastDayOfMonth

        isLastDayOfYear =
            isLastDayOfMonth && unMonth date.month == Time.Dec
    in
    MkDate
        { year =
            if isLastDayOfYear then
                MkYear <| unYear date.year + 1

            else
                date.year
        , month =
            if isLastDayOfMonth then
                nextMonth date.month

            else
                date.month
        , day =
            if isLastDayOfMonth then
                MkDay 1

            else
                MkDay <| unDay date.day + 1
        }
