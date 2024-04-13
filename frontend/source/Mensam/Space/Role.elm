module Mensam.Space.Role exposing (..)

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


type Permission
    = MkPermissionViewSpace
    | MkPermissionEditDesk
    | MkPermissionCreateReservation
    | MkPermissionCancelReservation


permissionToString : Permission -> String
permissionToString permission =
    case permission of
        MkPermissionViewSpace ->
            "view-space"

        MkPermissionEditDesk ->
            "edit-desk"

        MkPermissionCreateReservation ->
            "create-reservation"

        MkPermissionCancelReservation ->
            "cancel-reservation"


type Accessibility
    = MkAccessibilityJoinable
    | MkAccessibilityJoinableWithPassword
    | MkAccessibilityInaccessible


accessibilityEncode : Accessibility -> Encode.Value
accessibilityEncode accessibility =
    Encode.string <|
        case accessibility of
            MkAccessibilityJoinable ->
                "joinable"

            MkAccessibilityJoinableWithPassword ->
                "joinable-with-password"

            MkAccessibilityInaccessible ->
                "inaccessible"


accessibilityDecoder : Decode.Decoder Accessibility
accessibilityDecoder =
    Decode.andThen
        (\string ->
            case string of
                "joinable" ->
                    Decode.succeed MkAccessibilityJoinable

                "joinable-with-password" ->
                    Decode.succeed MkAccessibilityJoinableWithPassword

                "inaccessible" ->
                    Decode.succeed MkAccessibilityInaccessible

                _ ->
                    Decode.fail <| "Trying to decode accessibility, but this option is not supported: " ++ string
        )
        Decode.string
