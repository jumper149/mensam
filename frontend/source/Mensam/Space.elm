module Mensam.Space exposing (..)

import Json.Decode
import Json.Encode


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


identifierEncode : Identifier -> Json.Encode.Value
identifierEncode (MkIdentifier identifier) =
    Json.Encode.int identifier


identifierDecoder : Json.Decode.Decoder Identifier
identifierDecoder =
    Json.Decode.map MkIdentifier
        Json.Decode.int


type Name
    = MkName String


nameToString : Name -> String
nameToString (MkName name) =
    name


nameEncode : Name -> Json.Encode.Value
nameEncode =
    Json.Encode.string << nameToString


type Accessibility
    = MkAccessibilityJoinable
    | MkAccessibilityInaccessible


accessibilityEncode : Accessibility -> Json.Encode.Value
accessibilityEncode accessibility =
    Json.Encode.string <|
        case accessibility of
            MkAccessibilityJoinable ->
                "joinable"

            MkAccessibilityInaccessible ->
                "inaccessible"


type Visibility
    = MkVisibilityVisible
    | MkVisibilityHidden


visibilityEncode : Visibility -> Json.Encode.Value
visibilityEncode visibility =
    Json.Encode.string <|
        case visibility of
            MkVisibilityVisible ->
                "visible"

            MkVisibilityHidden ->
                "hidden"
