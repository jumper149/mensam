module Mensam.Api.RoleEdit exposing (..)

import Http
import Http.Extra
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Auth.Bearer
import Mensam.Space.Role
import Url.Builder


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , id : Mensam.Space.Role.Identifier
    , name : Maybe Mensam.Space.Role.Name
    , accessibilityAndPassword :
        Maybe
            { accessibility : Mensam.Space.Role.Accessibility
            , maybePassword : Maybe String
            }
    , permissions : Maybe Mensam.Space.Role.Permissions
    }


type Response
    = Success
    | ErrorInsufficientPermission Mensam.Space.Role.Permission
    | ErrorBody String
    | ErrorAuth Mensam.Auth.Bearer.Error


request : Request -> (Result Http.Error Response -> a) -> Cmd a
request body handleResult =
    Http.request
        { method = "PATCH"
        , headers =
            [ Mensam.Auth.Bearer.authorizationHeader body.jwt
            ]
        , url =
            Url.Builder.absolute
                [ "api"
                , "role"
                , "edit"
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
                        Ok () ->
                            Ok <| Success

                        Err err ->
                            Err <| Http.BadBody <| Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status


encodeBody : Request -> Encode.Value
encodeBody body =
    Encode.object
        [ ( "id"
          , Mensam.Space.Role.identifierEncode body.id
          )
        , ( "name"
          , case body.name of
                Nothing ->
                    Encode.object
                        [ ( "update", Encode.bool False )
                        ]

                Just name ->
                    Encode.object
                        [ ( "update", Encode.bool True )
                        , ( "value", Mensam.Space.Role.nameEncode name )
                        ]
          )
        , ( "accessibility-and-password"
          , case body.accessibilityAndPassword of
                Nothing ->
                    Encode.object
                        [ ( "update", Encode.bool False )
                        ]

                Just { accessibility, maybePassword } ->
                    Encode.object
                        [ ( "update", Encode.bool True )
                        , ( "value"
                          , Encode.object
                                [ ( "accessibility", Mensam.Space.Role.accessibilityEncode accessibility )
                                , ( "password"
                                  , case maybePassword of
                                        Nothing ->
                                            Encode.null

                                        Just password ->
                                            Encode.string password
                                  )
                                ]
                          )
                        ]
          )
        , ( "permissions"
          , case body.permissions of
                Nothing ->
                    Encode.object
                        [ ( "update", Encode.bool False )
                        ]

                Just permissions ->
                    Encode.object
                        [ ( "update", Encode.bool True )
                        , ( "value", Mensam.Space.Role.permissionsEncoder permissions )
                        ]
          )
        ]


decodeBody200 : Decode.Decoder ()
decodeBody200 =
    Decode.map
        (\_ -> ())
        (Decode.field "unit" <| Decode.list <| Decode.succeed ())


decodeBody400 : Decode.Decoder String
decodeBody400 =
    Decode.field "error" Decode.string
