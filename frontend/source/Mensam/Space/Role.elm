module Mensam.Space.Role exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Set


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
    | MkPermissionEditSpace
    | MkPermissionCreateReservation
    | MkPermissionCancelReservation


permissionToInt : Permission -> Int
permissionToInt permission =
    case permission of
        MkPermissionViewSpace ->
            0

        MkPermissionEditDesk ->
            1

        MkPermissionEditSpace ->
            2

        MkPermissionCreateReservation ->
            3

        MkPermissionCancelReservation ->
            4


permissionFromInt : Int -> Maybe Permission
permissionFromInt int =
    case int of
        0 ->
            Just MkPermissionViewSpace

        1 ->
            Just MkPermissionEditDesk

        2 ->
            Just MkPermissionEditSpace

        3 ->
            Just MkPermissionCreateReservation

        4 ->
            Just MkPermissionCancelReservation

        _ ->
            Nothing


permissionToString : Permission -> String
permissionToString permission =
    case permission of
        MkPermissionViewSpace ->
            "view-space"

        MkPermissionEditDesk ->
            "edit-desk"

        MkPermissionEditSpace ->
            "edit-space"

        MkPermissionCreateReservation ->
            "create-reservation"

        MkPermissionCancelReservation ->
            "cancel-reservation"


permissionEncode : Permission -> Encode.Value
permissionEncode =
    Encode.string << permissionToString


permissionDecoder : Decode.Decoder Permission
permissionDecoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "view-space" ->
                        Decode.succeed MkPermissionViewSpace

                    "edit-desk" ->
                        Decode.succeed MkPermissionEditDesk

                    "edit-space" ->
                        Decode.succeed MkPermissionEditSpace

                    "create-reservation" ->
                        Decode.succeed MkPermissionCreateReservation

                    "cancel-reservation" ->
                        Decode.succeed MkPermissionCancelReservation

                    _ ->
                        Decode.fail <| "Trying to decode permission, but this permission is not supported: " ++ string
            )


type Permissions
    = MkPermissions (Set.Set Int)


permissionsToList : Permissions -> List Permission
permissionsToList (MkPermissions permissions) =
    List.filterMap permissionFromInt <| Set.toList permissions


permissionsDecoder : Decode.Decoder Permissions
permissionsDecoder =
    Decode.map (MkPermissions << Set.fromList << List.map permissionToInt) <| Decode.list permissionDecoder


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
