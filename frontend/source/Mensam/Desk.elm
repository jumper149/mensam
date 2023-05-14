module Mensam.Desk exposing (..)

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
