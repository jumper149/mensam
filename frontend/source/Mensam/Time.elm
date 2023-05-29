module Mensam.Time exposing (..)

import Dict
import Element
import Json.Decode as Decode
import Json.Encode as Encode
import Svg
import Svg.Attributes
import Svg.Events
import Time
import Time.Extra
import TimeZone


type TimezoneIdentifier
    = MkTimezoneIdentifier String


timezone : TimezoneIdentifier -> Time.Zone
timezone (MkTimezoneIdentifier name) =
    case Dict.get name TimeZone.zones of
        Nothing ->
            Time.utc

        Just toZone ->
            toZone ()


mkTimezone : String -> Maybe ( TimezoneIdentifier, Time.Zone )
mkTimezone name =
    Maybe.map (\toZone -> ( MkTimezoneIdentifier name, toZone () )) <| Dict.get name TimeZone.zones


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


type MessageTime
    = SetHour Hour
    | SetMinute Minute


elementPickTime : Time -> Element.Element MessageTime
elementPickTime (MkTime _) =
    Element.el
        [ Element.width <| Element.px 200 ]
    <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "-120 -120 240 240"
                , Svg.Attributes.width "100%"
                ]
            <|
                Svg.circle
                    [ Svg.Attributes.cx "0"
                    , Svg.Attributes.cy "0"
                    , Svg.Attributes.r "120"
                    , Svg.Attributes.fill "rgba(255,255,255,0.1)"
                    ]
                    []
                    :: List.concat
                        [ svgOnCircle 100 "00" (SetMinute <| MkMinute 0) 0 -1
                        , svgOnCircle 100 "05" (SetMinute <| MkMinute 5) (1 / 2) -(sqrt 3 / 2)
                        , svgOnCircle 100 "10" (SetMinute <| MkMinute 10) (sqrt 3 / 2) -(1 / 2)
                        , svgOnCircle 100 "15" (SetMinute <| MkMinute 15) 1 0
                        , svgOnCircle 100 "20" (SetMinute <| MkMinute 20) (sqrt 3 / 2) (1 / 2)
                        , svgOnCircle 100 "25" (SetMinute <| MkMinute 25) (1 / 2) (sqrt 3 / 2)
                        , svgOnCircle 100 "30" (SetMinute <| MkMinute 30) 0 1
                        , svgOnCircle 100 "35" (SetMinute <| MkMinute 35) -(1 / 2) (sqrt 3 / 2)
                        , svgOnCircle 100 "40" (SetMinute <| MkMinute 40) -(sqrt 3 / 2) (1 / 2)
                        , svgOnCircle 100 "45" (SetMinute <| MkMinute 45) -1 0
                        , svgOnCircle 100 "50" (SetMinute <| MkMinute 50) -(sqrt 3 / 2) -(1 / 2)
                        , svgOnCircle 100 "55" (SetMinute <| MkMinute 55) -(1 / 2) -(sqrt 3 / 2)
                        , svgOnCircle 60 "12" (SetHour <| MkHour 12) 0 -1
                        , svgOnCircle 60 "1" (SetHour <| MkHour 1) (1 / 2) -(sqrt 3 / 2)
                        , svgOnCircle 60 "2" (SetHour <| MkHour 2) (sqrt 3 / 2) -(1 / 2)
                        , svgOnCircle 60 "3" (SetHour <| MkHour 3) 1 0
                        , svgOnCircle 60 "4" (SetHour <| MkHour 4) (sqrt 3 / 2) (1 / 2)
                        , svgOnCircle 60 "5" (SetHour <| MkHour 5) (1 / 2) (sqrt 3 / 2)
                        , svgOnCircle 60 "6" (SetHour <| MkHour 6) 0 1
                        , svgOnCircle 60 "7" (SetHour <| MkHour 7) -(1 / 2) (sqrt 3 / 2)
                        , svgOnCircle 60 "8" (SetHour <| MkHour 8) -(sqrt 3 / 2) (1 / 2)
                        , svgOnCircle 60 "9" (SetHour <| MkHour 9) -1 0
                        , svgOnCircle 60 "10" (SetHour <| MkHour 10) -(sqrt 3 / 2) -(1 / 2)
                        , svgOnCircle 60 "11" (SetHour <| MkHour 11) -(1 / 2) -(sqrt 3 / 2)
                        , svgOnCircle 40 "0" (SetHour <| MkHour 0) 0 -1
                        , svgOnCircle 40 "13" (SetHour <| MkHour 13) (1 / 2) -(sqrt 3 / 2)
                        , svgOnCircle 40 "14" (SetHour <| MkHour 14) (sqrt 3 / 2) -(1 / 2)
                        , svgOnCircle 40 "15" (SetHour <| MkHour 15) 1 0
                        , svgOnCircle 40 "16" (SetHour <| MkHour 16) (sqrt 3 / 2) (1 / 2)
                        , svgOnCircle 40 "17" (SetHour <| MkHour 17) (1 / 2) (sqrt 3 / 2)
                        , svgOnCircle 40 "18" (SetHour <| MkHour 18) 0 1
                        , svgOnCircle 40 "19" (SetHour <| MkHour 19) -(1 / 2) (sqrt 3 / 2)
                        , svgOnCircle 40 "20" (SetHour <| MkHour 20) -(sqrt 3 / 2) (1 / 2)
                        , svgOnCircle 40 "21" (SetHour <| MkHour 21) -1 0
                        , svgOnCircle 40 "22" (SetHour <| MkHour 22) -(sqrt 3 / 2) -(1 / 2)
                        , svgOnCircle 40 "23" (SetHour <| MkHour 23) -(1 / 2) -(sqrt 3 / 2)
                        ]


svgOnCircle : Float -> String -> msg -> Float -> Float -> List (Svg.Svg msg)
svgOnCircle factor string msg x y =
    [ Svg.circle
        [ Svg.Attributes.cx <| String.fromFloat <| factor * x
        , Svg.Attributes.cy <| String.fromFloat <| factor * y
        , Svg.Attributes.r "10"
        , Svg.Attributes.fill "rgba(255,255,255,0.1)"
        ]
        []
    , Svg.text_
        [ Svg.Attributes.x <| String.fromFloat <| factor * x
        , Svg.Attributes.y <| String.fromFloat <| factor * y
        , Svg.Attributes.textAnchor "middle"
        , Svg.Attributes.dominantBaseline "central"
        , Svg.Attributes.style "user-select: none;"
        ]
        [ Svg.text string ]
    , Svg.circle
        [ Svg.Attributes.cx <| String.fromFloat <| factor * x
        , Svg.Attributes.cy <| String.fromFloat <| factor * y
        , Svg.Attributes.r "10"
        , Svg.Attributes.style "cursor: pointer;"
        , Svg.Attributes.fill "rgba(255,255,255,0.0)"
        , Svg.Events.onClick msg
        ]
        []
    ]


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
