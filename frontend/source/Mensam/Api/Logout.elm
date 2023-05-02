module Mensam.Api.Logout exposing (..)

import Http
import Json.Decode
import Mensam.Auth.Bearer
import Url.Builder


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    }


type Response
    = Success
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
                , "logout"
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
                    case Json.Decode.decodeString Mensam.Auth.Bearer.http401BodyDecoder body of
                        Ok value ->
                            Ok <| ErrorAuth value

                        Err err ->
                            Err <| Http.BadBody <| Json.Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status

        Http.GoodStatus_ metadata body ->
            case metadata.statusCode of
                200 ->
                    case Json.Decode.decodeString decodeBody200 body of
                        Ok () ->
                            Ok <| Success

                        Err err ->
                            Err <| Http.BadBody <| Json.Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status


decodeBody200 : Json.Decode.Decoder ()
decodeBody200 =
    Json.Decode.map (\_ -> ())
        (Json.Decode.field "unit" <| Json.Decode.list <| Json.Decode.succeed ())
