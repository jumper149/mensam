module Mensam.Api.Login exposing (..)

import Http
import Iso8601
import Json.Decode as Decode
import Mensam.Auth.Basic
import Mensam.Auth.Bearer
import Time
import Url.Builder


type Request
    = BasicAuth Mensam.Auth.Basic.Credentials
    | Bearer Mensam.Auth.Bearer.Jwt


type Response
    = Success { jwt : Mensam.Auth.Bearer.Jwt, expiration : Maybe Time.Posix }
    | ErrorAuth Mensam.Auth.Basic.Error


request : Request -> (Result Http.Error Response -> a) -> Cmd a
request body handleResult =
    Http.request
        { method = "POST"
        , headers =
            [ case body of
                BasicAuth credentials ->
                    Mensam.Auth.Basic.authorizationHeader credentials

                Bearer jwt ->
                    Mensam.Auth.Bearer.authorizationHeader jwt
            ]
        , url =
            Url.Builder.absolute
                [ "api"
                , "login"
                ]
                []
        , body = Http.emptyBody
        , expect = Http.expectStringResponse handleResult responseResult
        , timeout = Nothing
        , tracker = Nothing
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
                401 ->
                    case Decode.decodeString Mensam.Auth.Basic.http401BodyDecoder body of
                        Ok value ->
                            Ok <| ErrorAuth value

                        Err err ->
                            Err <| Http.BadBody <| Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status

        Http.GoodStatus_ metadata body ->
            case metadata.statusCode of
                200 ->
                    case Decode.decodeString decodeBody200 body of
                        Ok value ->
                            Ok <| Success value

                        Err err ->
                            Err <| Http.BadBody <| Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status


decodeBody200 : Decode.Decoder { jwt : Mensam.Auth.Bearer.Jwt, expiration : Maybe Time.Posix }
decodeBody200 =
    Decode.map2 (\jwt expiration -> { jwt = jwt, expiration = expiration })
        (Decode.field "jwt" Mensam.Auth.Bearer.decoder)
        (Decode.maybe <| Decode.field "expiration" Iso8601.decoder)
