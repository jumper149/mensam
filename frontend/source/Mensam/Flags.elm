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
            , zone : Mensam.Time.Timezone
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
            Decode.map2 (\now timezone -> { now = now, zone = timezone })
                (Decode.field "now" (Decode.map Time.millisToPosix Decode.int))
                (Decode.field "zone" Mensam.Time.timezoneDecoder)
        )
