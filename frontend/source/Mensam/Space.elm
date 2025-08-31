module Mensam.Space exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode


type Identifier
    = MkIdentifier Int


identifierToString : Identifier -> String
identifierToString (MkIdentifier identifier) =
    String.fromInt identifier


identifierEncode : Identifier -> Encode.Value
identifierEncode (MkIdentifier identifier) =
    Encode.int identifier


identifierDecoder : Decode.Decoder Identifier
identifierDecoder =
    Decode.map MkIdentifier
        Decode.int


type Name
    = MkName String


nameToString : Name -> String
nameToString (MkName name) =
    name


nameEncode : Name -> Encode.Value
nameEncode =
    Encode.string << nameToString


nameDecoder : Decode.Decoder Name
nameDecoder =
    Decode.map MkName Decode.string


type Discoverability
    = MkDiscoverabilityPublic
    | MkDiscoverabilityPrivate


discoverabilityToString : Discoverability -> String
discoverabilityToString discoverability =
    case discoverability of
        MkDiscoverabilityPublic ->
            "public"

        MkDiscoverabilityPrivate ->
            "private"


discoverabilityEncode : Discoverability -> Encode.Value
discoverabilityEncode discoverability =
    Encode.string <|
        case discoverability of
            MkDiscoverabilityPublic ->
                "public"

            MkDiscoverabilityPrivate ->
                "private"


discoverabilityDecoder : Decode.Decoder Discoverability
discoverabilityDecoder =
    Decode.andThen
        (\string ->
            case string of
                "public" ->
                    Decode.succeed MkDiscoverabilityPublic

                "private" ->
                    Decode.succeed MkDiscoverabilityPrivate

                _ ->
                    Decode.fail <| "Trying to decode discoverability, but this option is not supported: " ++ string
        )
        Decode.string
