module Mensam.Api.PasswordChange exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Auth.Bearer
import Mensam.Tracker
import Mensam.Url
import Mensam.User


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , newPassword : Mensam.User.Password
    }


type Response
    = Success
    | ErrorBody String
    | ErrorAuth Mensam.Auth.Bearer.Error


request : Maybe Mensam.Tracker.Tracker -> Mensam.Url.BaseUrl -> Request -> (Result Http.Error Response -> a) -> Cmd a
request tracker baseUrl body handleResult =
    Http.request
        { method = "PATCH"
        , headers =
            [ Mensam.Auth.Bearer.authorizationHeader body.jwt
            ]
        , url =
            Mensam.Url.absolute baseUrl
                [ "api"
                , "password"
                ]
                []
        , body = Http.jsonBody <| encodeBody body
        , expect = Http.expectStringResponse handleResult responseResult
        , timeout = Nothing
        , tracker = Maybe.map Mensam.Tracker.toHttp tracker
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


encodeBody : Request -> Encode.Value
encodeBody body =
    Encode.object
        [ ( "new-password"
          , Mensam.User.passwordEncode body.newPassword
          )
        ]


decodeBody200 : Decode.Decoder ()
decodeBody200 =
    Decode.map (\_ -> ())
        (Decode.field "unit" <| Decode.list <| Decode.succeed ())


decodeBody400 : Decode.Decoder String
decodeBody400 =
    Decode.field "error" Decode.string
