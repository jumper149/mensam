module Mensam.Reservation exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode


type Identifier
    = MkIdentifier Int


identifierEncode : Identifier -> Encode.Value
identifierEncode (MkIdentifier identifier) =
    Encode.int identifier


identifierDecoder : Decode.Decoder Identifier
identifierDecoder =
    Decode.map MkIdentifier
        Decode.int


type Status
    = MkStatusPlanned
    | MkStatusCancelled


statusToString : Status -> String
statusToString status =
    case status of
        MkStatusPlanned ->
            "planned"

        MkStatusCancelled ->
            "cancelled"


statusEncode : Status -> Encode.Value
statusEncode status =
    case status of
        MkStatusPlanned ->
            Encode.string "planned"

        MkStatusCancelled ->
            Encode.string "cancelled"


statusDecoder : Decode.Decoder Status
statusDecoder =
    Decode.andThen
        (\string ->
            case string of
                "planned" ->
                    Decode.succeed MkStatusPlanned

                "cancelled" ->
                    Decode.succeed MkStatusCancelled

                _ ->
                    Decode.fail <| "Trying to decode reservation status, but this option is not supported: " ++ string
        )
        Decode.string
