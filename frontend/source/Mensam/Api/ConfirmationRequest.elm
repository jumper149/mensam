module Mensam.Api.ConfirmationRequest exposing (..)

import Http
import Http.Extra
import Json.Decode as Decode
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
                , "confirmation"
                , "request"
                ]
                []
        , body = Http.emptyBody
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
                401 ->
                    case Decode.decodeString Mensam.Auth.Bearer.http401BodyDecoder body of
                        Ok error ->
                            Ok <| ErrorAuth error

                        Err err ->
                            Err <| Http.BadBody <| Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status

        Http.GoodStatus_ metadata body ->
            case metadata.statusCode of
                200 ->
                    case Decode.decodeString decodeBody200 body of
                        Ok () ->
                            Ok Success

                        Err err ->
                            Err <| Http.BadBody <| Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status


decodeBody200 : Decode.Decoder ()
decodeBody200 =
    Decode.map (\_ -> ())
        (Decode.field "unit" <| Decode.list <| Decode.succeed ())
