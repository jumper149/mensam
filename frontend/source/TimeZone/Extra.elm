module TimeZone.Extra exposing (zones)

import Dict
import Time
import TimeZone


zones : Dict.Dict String (() -> Time.Zone)
zones =
    let
        aliases =
            regions1 ++ regions2

        additions =
            List.map (\( key, value ) -> ( key, \() -> value ))
                etcZones
    in
    List.foldl alias (List.foldl insertTuple TimeZone.zones additions) aliases


insertTuple : ( comparable, v ) -> Dict.Dict comparable v -> Dict.Dict comparable v
insertTuple ( key, value ) dict =
    Dict.insert key value dict


alias : ( comparable, comparable ) -> Dict.Dict comparable v -> Dict.Dict comparable v
alias ( aliasKey, originalKey ) dict =
    case Dict.get originalKey dict of
        Nothing ->
            dict

        Just value ->
            Dict.insert aliasKey value dict


regions1 : List ( String, String )
regions1 =
    [ ( "CET", "Europe/Paris" )
    , ( "EET", "Europe/Sofia" )
    , ( "EST", "America/Cancun" )
    , ( "HST", "Pacific/Honolulu" )
    , ( "MET", "Europe/Paris" )
    , ( "MST", "America/Phoenix" )
    , ( "WET", "Europe/Lisbon" )
    ]


regions2 : List ( String, String )
regions2 =
    [ ( "CST6CDT", "America/Chicago" )
    , ( "EST5EDT", "America/New_York" )
    , ( "MST7MDT", "America/Denver" )
    , ( "PST8PDT", "America/Los_Angeles" )
    ]


etcZones : List ( String, Time.Zone )
etcZones =
    let
        simpleZone offset =
            Time.customZone (offset * 60) []
    in
    [ ( "Etc/GMT", simpleZone 0 )
    , ( "Etc/GMT-14", simpleZone -14 )
    , ( "Etc/GMT-13", simpleZone -13 )
    , ( "Etc/GMT-12", simpleZone -12 )
    , ( "Etc/GMT-11", simpleZone -11 )
    , ( "Etc/GMT-10", simpleZone -10 )
    , ( "Etc/GMT-9", simpleZone -9 )
    , ( "Etc/GMT-8", simpleZone -8 )
    , ( "Etc/GMT-7", simpleZone -7 )
    , ( "Etc/GMT-6", simpleZone -6 )
    , ( "Etc/GMT-5", simpleZone -5 )
    , ( "Etc/GMT-4", simpleZone -4 )
    , ( "Etc/GMT-3", simpleZone -3 )
    , ( "Etc/GMT-2", simpleZone -2 )
    , ( "Etc/GMT-1", simpleZone -1 )
    , ( "Etc/GMT+0", simpleZone 0 )
    , ( "Etc/GMT+1", simpleZone 1 )
    , ( "Etc/GMT+2", simpleZone 2 )
    , ( "Etc/GMT+3", simpleZone 3 )
    , ( "Etc/GMT+4", simpleZone 4 )
    , ( "Etc/GMT+5", simpleZone 5 )
    , ( "Etc/GMT+6", simpleZone 6 )
    , ( "Etc/GMT+7", simpleZone 7 )
    , ( "Etc/GMT+8", simpleZone 8 )
    , ( "Etc/GMT+9", simpleZone 9 )
    , ( "Etc/GMT+10", simpleZone 10 )
    , ( "Etc/GMT+11", simpleZone 11 )
    , ( "Etc/GMT+12", simpleZone 12 )
    , ( "Etc/UTC", simpleZone 0 )
    ]
