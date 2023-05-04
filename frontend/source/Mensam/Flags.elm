module Mensam.Flags exposing
    ( Flags(..)
    , FlagsRaw
    , parse
    )

import Json.Decode
import Json.Encode
import Mensam.Error
import Mensam.Storage


type Flags
    = MkFlags
        { maybeStorage : Maybe Mensam.Storage.Storage
        }


type alias FlagsRaw =
    Json.Encode.Value


parse : FlagsRaw -> Result Mensam.Error.Error Flags
parse flagsRaw =
    Result.mapError
        (\error ->
            Mensam.Error.message "Failed to parse flags." <|
                Mensam.Error.message (Json.Decode.errorToString error)
                    Mensam.Error.undefined
        )
    <|
        Json.Decode.decodeValue decoder flagsRaw


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map (\maybeStorage -> MkFlags { maybeStorage = maybeStorage })
        Mensam.Storage.decoder
