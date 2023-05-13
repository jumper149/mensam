module Mensam.Space exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode


type Space
    = MkSpace
        { id : Identifier
        , name : Name
        }


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


type Accessibility
    = MkAccessibilityJoinable
    | MkAccessibilityInaccessible


accessibilityEncode : Accessibility -> Encode.Value
accessibilityEncode accessibility =
    Encode.string <|
        case accessibility of
            MkAccessibilityJoinable ->
                "joinable"

            MkAccessibilityInaccessible ->
                "inaccessible"


type Visibility
    = MkVisibilityVisible
    | MkVisibilityHidden


visibilityEncode : Visibility -> Encode.Value
visibilityEncode visibility =
    Encode.string <|
        case visibility of
            MkVisibilityVisible ->
                "visible"

            MkVisibilityHidden ->
                "hidden"
