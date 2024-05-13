module Mensam.Api.SpaceView exposing (..)

import Dict
import Http
import Http.Extra
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Auth.Bearer
import Mensam.Space
import Mensam.Space.Role
import Mensam.Time
import Mensam.User
import Url.Builder


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , id : Mensam.Space.Identifier
    }


type Response
    = Success
        { id : Mensam.Space.Identifier
        , name : Mensam.Space.Name
        , roles :
            List
                { accessibility : Mensam.Space.Role.Accessibility
                , id : Mensam.Space.Role.Identifier
                , name : Mensam.Space.Role.Name
                , permissions : Mensam.Space.Role.Permissions
                }
        , users :
            List
                { user : Mensam.User.Identifier
                , role : Mensam.Space.Role.Identifier
                }
        , timezone : Mensam.Time.TimezoneIdentifier
        , visibility : Mensam.Space.Visibility
        , owner : Mensam.User.Identifier
        , yourRole :
            Maybe
                { accessibility : Mensam.Space.Role.Accessibility
                , id : Mensam.Space.Role.Identifier
                , name : Mensam.Space.Role.Name
                , permissions : Mensam.Space.Role.Permissions
                }
        }
    | ErrorInsufficientPermission Mensam.Space.Role.Permission
    | ErrorBody String
    | ErrorAuth Mensam.Auth.Bearer.Error


request : Request -> (Result Http.Error Response -> a) -> Cmd a
request body handleResult =
    Http.request
        { method = "POST"
        , headers =
            [ Mensam.Auth.Bearer.authorizationHeader body.jwt
            ]
        , url =
            Url.Builder.absolute
                [ "api"
                , "space"
                , "view"
                ]
                []
        , body = Http.jsonBody <| encodeBody body
        , expect = Http.expectStringResponse handleResult responseResult
        , timeout = Nothing
        , tracker = Http.Extra.tracker
        }


responseResult : Http.Response String -> Result Http.Error Response
responseResult httpResponse =
    case httpResponse of
        Http.BadUrl_ err ->
            Err <| Http.BadUrl err

        Http.Timeout_ ->
            Err <| Http.Timeout

        Http.NetworkError_ ->
            Err <| Http.NetworkError

        Http.BadStatus_ metadata body ->
            case metadata.statusCode of
                400 ->
                    case Decode.decodeString decodeBody400 body of
                        Ok error ->
                            Ok <| ErrorBody error

                        Err err ->
                            Err <| Http.BadBody <| Decode.errorToString err

                401 ->
                    case Decode.decodeString Mensam.Auth.Bearer.http401BodyDecoder body of
                        Ok error ->
                            Ok <| ErrorAuth error

                        Err err ->
                            Err <| Http.BadBody <| Decode.errorToString err

                403 ->
                    case Decode.decodeString Mensam.Space.Role.http403BodyDecoder body of
                        Ok permission ->
                            Ok <| ErrorInsufficientPermission permission

                        Err err ->
                            Err <| Http.BadBody <| Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status

        Http.GoodStatus_ metadata body ->
            case metadata.statusCode of
                200 ->
                    case Decode.decodeString decodeBody200 body of
                        Ok response ->
                            Ok <| Success response

                        Err err ->
                            Err <| Http.BadBody <| Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status


encodeBody : Request -> Encode.Value
encodeBody body =
    Encode.object
        [ ( "id"
          , Mensam.Space.identifierEncode body.id
          )
        ]


decodeBody200 :
    Decode.Decoder
        { id : Mensam.Space.Identifier
        , name : Mensam.Space.Name
        , roles :
            List
                { accessibility : Mensam.Space.Role.Accessibility
                , id : Mensam.Space.Role.Identifier
                , name : Mensam.Space.Role.Name
                , permissions : Mensam.Space.Role.Permissions
                }
        , users :
            List
                { user : Mensam.User.Identifier
                , role : Mensam.Space.Role.Identifier
                }
        , timezone : Mensam.Time.TimezoneIdentifier
        , visibility : Mensam.Space.Visibility
        , owner : Mensam.User.Identifier
        , yourRole :
            Maybe
                { accessibility : Mensam.Space.Role.Accessibility
                , id : Mensam.Space.Role.Identifier
                , name : Mensam.Space.Role.Name
                , permissions : Mensam.Space.Role.Permissions
                }
        }
decodeBody200 =
    Decode.andThen
        (\record ->
            case record.yourRole of
                Nothing ->
                    Decode.fail "`your-role` does not refer to any known role"

                Just yourRole ->
                    Decode.succeed <|
                        { id = record.id
                        , name = record.name
                        , roles = record.roles
                        , users = record.users
                        , timezone = record.timezone
                        , visibility = record.visibility
                        , owner = record.owner
                        , yourRole = yourRole
                        }
        )
    <|
        Decode.map8
            (\id name roles users timezone visibility owner maybeYourRoleId ->
                { id = id
                , name = name
                , roles = roles
                , users = users
                , timezone = timezone
                , visibility = visibility
                , owner = owner
                , yourRole =
                    case maybeYourRoleId of
                        Nothing ->
                            Just Nothing

                        Just yourRoleId ->
                            let
                                unIdentifierRole (Mensam.Space.Role.MkIdentifier roleId) =
                                    roleId
                            in
                            case Dict.get (unIdentifierRole yourRoleId) <| Dict.fromList <| List.map (\role -> ( unIdentifierRole role.id, role )) roles of
                                Nothing ->
                                    Just Nothing

                                Just role ->
                                    Just <| Just role
                }
            )
            (Decode.field "id" Mensam.Space.identifierDecoder)
            (Decode.field "name" Mensam.Space.nameDecoder)
            (Decode.field "roles" <|
                Decode.list <|
                    Decode.map4
                        (\accessibility id name permissions ->
                            { accessibility = accessibility
                            , id = id
                            , name = name
                            , permissions = permissions
                            }
                        )
                        (Decode.field "accessibility" Mensam.Space.Role.accessibilityDecoder)
                        (Decode.field "id" Mensam.Space.Role.identifierDecoder)
                        (Decode.field "name" Mensam.Space.Role.nameDecoder)
                        (Decode.field "permissions" <| Mensam.Space.Role.permissionsDecoder)
            )
            (Decode.field "users" <|
                Decode.list <|
                    Decode.map2
                        (\user role ->
                            { user = user
                            , role = role
                            }
                        )
                        (Decode.field "user" Mensam.User.identifierDecoder)
                        (Decode.field "role" Mensam.Space.Role.identifierDecoder)
            )
            (Decode.field "timezone" Mensam.Time.timezoneIdentifierDecoder)
            (Decode.field "visibility" Mensam.Space.visibilityDecoder)
            (Decode.field "owner" Mensam.User.identifierDecoder)
            (Decode.field "your-role" <| Decode.nullable Mensam.Space.Role.identifierDecoder)


decodeBody400 : Decode.Decoder String
decodeBody400 =
    Decode.field "error" Decode.string
