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


dayToString : Day -> String
dayToString (MkDay n) =
    String.fromInt n


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
