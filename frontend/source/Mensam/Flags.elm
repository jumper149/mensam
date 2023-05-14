module Mensam.Flags exposing
    ( Flags(..)
    , FlagsRaw
    , parse
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Error
import Mensam.Storage
import Mensam.Time
import Time


type Flags
    = MkFlags
        { storage : Maybe Mensam.Storage.Storage
        , time :
            { now : Time.Posix
            , zone : Time.Zone
            , zoneIdentifier : Mensam.Time.TimezoneIdentifier
            }
        }


type alias FlagsRaw =
    Encode.Value


parse : FlagsRaw -> Result Mensam.Error.Error Flags
parse flagsRaw =
    Result.mapError
        (Mensam.Error.message "Failed to parse flags" << Mensam.Error.json)
    <|
        Decode.decodeValue decoder flagsRaw


decoder : Decode.Decoder Flags
decoder =
    Decode.map2 (\storage time -> MkFlags { storage = storage, time = time })
        (Decode.field "storage" Mensam.Storage.decoder)
        (Decode.field "time" <|
            Decode.map2 (\now ( zoneIdentifier, zone ) -> { now = now, zone = zone, zoneIdentifier = zoneIdentifier })
                (Decode.field "now" (Decode.map Time.millisToPosix Decode.int))
                (Decode.field "zone"
                    (Decode.andThen
                        (\string ->
                            case Mensam.Time.mkTimezone string of
                                Nothing ->
                                    Decode.fail <| "Trying to decode timezone, but this timezone is not supported: " ++ string

                                Just ( identifier, zone ) ->
                                    Decode.succeed ( identifier, zone )
                        )
                        Decode.string
                    )
                )
        )
