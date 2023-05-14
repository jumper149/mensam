module Mensam.Space exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Time
import Set


type SpaceView
    = MkSpaceView
        { id : Identifier
        , name : Name
        , timezone : Mensam.Time.TimezoneIdentifier
        , visibility : Visibility
        , accessibility : Accessibility
        , permissions : Set.Set String
        }


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


accessibilityDecoder : Decode.Decoder Accessibility
accessibilityDecoder =
    Decode.andThen
        (\string ->
            case string of
                "joinable" ->
                    Decode.succeed MkAccessibilityJoinable

                "inaccessible" ->
                    Decode.succeed MkAccessibilityInaccessible

                _ ->
                    Decode.fail <| "Trying to decode accessibility, but this option is not supported: " ++ string
        )
        Decode.string


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


visibilityDecoder : Decode.Decoder Visibility
visibilityDecoder =
    Decode.andThen
        (\string ->
            case string of
                "visible" ->
                    Decode.succeed MkVisibilityVisible

                "hidden" ->
                    Decode.succeed MkVisibilityHidden

                _ ->
                    Decode.fail <| "Trying to decode visibility, but this option is not supported: " ++ string
        )
        Decode.string


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
