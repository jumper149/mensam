module Mensam.Flags exposing
    ( Flags(..)
    , FlagsRaw
    , parse
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Error
import Mensam.Storage


type Flags
    = MkFlags
        { storage : Maybe Mensam.Storage.Storage
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
    Decode.map (\storage -> MkFlags { storage = storage })
        (Decode.field "storage" Mensam.Storage.decoder)
