module Mensam.Time exposing (..)

import Element
import Element.Background
import Element.Events
import Html.Attributes
import Time
import Time.Extra


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


type ModelMonth
    = MkModelMonth
        { year : Year
        , month : Month
        }


type MessageMonth
    = MonthNext
    | MonthPrevious


elementPickMonth : ModelMonth -> Element.Element MessageMonth
elementPickMonth (MkModelMonth model) =
    Element.el
        [ Element.width <| Element.px 230
        , Element.height <| Element.px 40
        , Element.spaceEvenly
        , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
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
                , Element.mouseOver
                    [ Element.Background.color <| Element.rgba 1 1 1 0.1
                    ]
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
                        yearToString model.year
                            ++ ", "
                            ++ monthToString model.month
            , Element.el
                [ Element.width <| Element.px 35
                , Element.height <| Element.px 35
                , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                , Element.mouseOver
                    [ Element.Background.color <| Element.rgba 1 1 1 0.1
                    ]
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


type ModelDay
    = MkModelDay
        { year : Year
        , month : Month
        , selected : List Day
        }


type MessageDay
    = ClickDay Day


elementPickDay : ModelDay -> Element.Element MessageDay
elementPickDay (MkModelDay model) =
    let
        weekdayOfFirst =
            Time.toWeekday Time.utc <|
                toPosix Time.utc <|
                    MkTimestamp
                        { date =
                            MkDate
                                { year = model.year
                                , month = model.month
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
            List.map (Just << MkDay) <| List.range 1 (daysInMonth model.year model.month)

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
                                if List.member day model.selected then
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
                                    dayToString day
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


type ModelDate
    = MkModelDate
        { year : Year
        , month : Month
        , selected : List Day
        }


type MessageDate
    = MessageMonth MessageMonth
    | MessageDay MessageDay


updateDateNextMonth : ModelDate -> ModelDate
updateDateNextMonth (MkModelDate model) =
    case unMonth model.month of
        Time.Jan ->
            MkModelDate { model | month = MkMonth Time.Feb, selected = [] }

        Time.Feb ->
            MkModelDate { model | month = MkMonth Time.Mar, selected = [] }

        Time.Mar ->
            MkModelDate { model | month = MkMonth Time.Apr, selected = [] }

        Time.Apr ->
            MkModelDate { model | month = MkMonth Time.May, selected = [] }

        Time.May ->
            MkModelDate { model | month = MkMonth Time.Jun, selected = [] }

        Time.Jun ->
            MkModelDate { model | month = MkMonth Time.Jul, selected = [] }

        Time.Jul ->
            MkModelDate { model | month = MkMonth Time.Aug, selected = [] }

        Time.Aug ->
            MkModelDate { model | month = MkMonth Time.Sep, selected = [] }

        Time.Sep ->
            MkModelDate { model | month = MkMonth Time.Oct, selected = [] }

        Time.Oct ->
            MkModelDate { model | month = MkMonth Time.Nov, selected = [] }

        Time.Nov ->
            MkModelDate { model | month = MkMonth Time.Dec, selected = [] }

        Time.Dec ->
            MkModelDate
                { model
                    | year = MkYear <| unYear model.year + 1
                    , month = MkMonth Time.Jan
                    , selected = []
                }


updateDatePreviousMonth : ModelDate -> ModelDate
updateDatePreviousMonth (MkModelDate model) =
    case unMonth model.month of
        Time.Jan ->
            MkModelDate
                { model
                    | year = MkYear <| unYear model.year - 1
                    , month = MkMonth Time.Dec
                    , selected = []
                }

        Time.Feb ->
            MkModelDate { model | month = MkMonth Time.Jan, selected = [] }

        Time.Mar ->
            MkModelDate { model | month = MkMonth Time.Feb, selected = [] }

        Time.Apr ->
            MkModelDate { model | month = MkMonth Time.Mar, selected = [] }

        Time.May ->
            MkModelDate { model | month = MkMonth Time.Apr, selected = [] }

        Time.Jun ->
            MkModelDate { model | month = MkMonth Time.May, selected = [] }

        Time.Jul ->
            MkModelDate { model | month = MkMonth Time.Jun, selected = [] }

        Time.Aug ->
            MkModelDate { model | month = MkMonth Time.Jul, selected = [] }

        Time.Sep ->
            MkModelDate { model | month = MkMonth Time.Aug, selected = [] }

        Time.Oct ->
            MkModelDate { model | month = MkMonth Time.Sep, selected = [] }

        Time.Nov ->
            MkModelDate { model | month = MkMonth Time.Oct, selected = [] }

        Time.Dec ->
            MkModelDate { model | month = MkMonth Time.Nov, selected = [] }


elementPickDate : ModelDate -> Element.Element MessageDate
elementPickDate (MkModelDate model) =
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
                Element.map MessageMonth <|
                    elementPickMonth <|
                        MkModelMonth
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
                Element.map MessageDay <|
                    elementPickDay <|
                        MkModelDay
                            { year = model.year
                            , month = model.month
                            , selected = model.selected
                            }
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
