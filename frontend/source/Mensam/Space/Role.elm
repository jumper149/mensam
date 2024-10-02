module Mensam.Space.Role exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Error
import Set


type Identifier
    = MkIdentifier Int


identifierToInt : Identifier -> Int
identifierToInt (MkIdentifier identifier) =
    identifier


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
    | MkPermissionEditUser
    | MkPermissionEditRole
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

        MkPermissionEditUser ->
            2

        MkPermissionEditRole ->
            3

        MkPermissionEditSpace ->
            4

        MkPermissionCreateReservation ->
            5

        MkPermissionCancelReservation ->
            6


permissionFromInt : Int -> Maybe Permission
permissionFromInt int =
    case int of
        0 ->
            Just MkPermissionViewSpace

        1 ->
            Just MkPermissionEditDesk

        2 ->
            Just MkPermissionEditUser

        3 ->
            Just MkPermissionEditRole

        4 ->
            Just MkPermissionEditSpace

        5 ->
            Just MkPermissionCreateReservation

        6 ->
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

        MkPermissionEditUser ->
            "edit-user"

        MkPermissionEditRole ->
            "edit-role"

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

                    "edit-user" ->
                        Decode.succeed MkPermissionEditUser

                    "edit-role" ->
                        Decode.succeed MkPermissionEditRole

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


permissionsFromList : List Permission -> Permissions
permissionsFromList =
    MkPermissions << Set.fromList << List.map permissionToInt


permissionCheck : Permission -> Permissions -> Bool
permissionCheck permissionToCheck (MkPermissions permissionsGiven) =
    Set.member (permissionToInt permissionToCheck) permissionsGiven


allPermissions : Permissions
allPermissions =
    permissionsFromList
        [ MkPermissionViewSpace
        , MkPermissionEditDesk
        , MkPermissionEditUser
        , MkPermissionEditRole
        , MkPermissionEditSpace
        , MkPermissionCreateReservation
        , MkPermissionCancelReservation
        ]


permissionsEncoder : Permissions -> Encode.Value
permissionsEncoder =
    Encode.list permissionEncode << permissionsToList


permissionsDecoder : Decode.Decoder Permissions
permissionsDecoder =
    Decode.map (MkPermissions << Set.fromList << List.map permissionToInt) <| Decode.list permissionDecoder


http403BodyDecoder : Decode.Decoder Permission
http403BodyDecoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                let
                    stripPrefix : String -> String -> Maybe String
                    stripPrefix prefix str =
                        if String.startsWith prefix str then
                            Just <| String.dropLeft (String.length prefix) str

                        else
                            Nothing
                in
                case stripPrefix "Insufficient permission: " string of
                    Nothing ->
                        Decode.fail <| "Trying to decode permission error, but the prefix is not \"Insufficient permission: \""

                    Just suffix ->
                        case suffix of
                            "'MkPermissionSpaceViewSpace" ->
                                Decode.succeed MkPermissionViewSpace

                            "'MkPermissionSpaceEditDesk" ->
                                Decode.succeed MkPermissionEditDesk

                            "'MkPermissionSpaceEditUser" ->
                                Decode.succeed MkPermissionEditUser

                            "'MkPermissionSpaceEditRole" ->
                                Decode.succeed MkPermissionEditRole

                            "'MkPermissionSpaceEditSpace" ->
                                Decode.succeed MkPermissionEditSpace

                            "'MkPermissionSpaceCreateReservation" ->
                                Decode.succeed MkPermissionCreateReservation

                            "'MkPermissionSpaceCancelReservation" ->
                                Decode.succeed MkPermissionCancelReservation

                            unknownSuffix ->
                                Decode.fail <| "Trying to decode permission error, but this option is not supported: " ++ unknownSuffix
            )


errorInsufficientPermission : Permission -> Mensam.Error.Error
errorInsufficientPermission err =
    Mensam.Error.message "Request this permission from the administrator of your space" <|
        Mensam.Error.message (permissionToString err) <|
            Mensam.Error.message "Insufficient permission" <|
                Mensam.Error.undefined


type Accessibility
    = MkAccessibilityJoinable
    | MkAccessibilityJoinableWithPassword
    | MkAccessibilityInaccessible


accessibilityToString : Accessibility -> String
accessibilityToString accessibility =
    case accessibility of
        MkAccessibilityJoinable ->
            "Joinable"

        MkAccessibilityJoinableWithPassword ->
            "Requires Password"

        MkAccessibilityInaccessible ->
            "Inaccessible"


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
