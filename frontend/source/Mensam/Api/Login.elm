module Mensam.Api.Login exposing (..)

import Base64
import Http
import Iso8601
import Json.Decode
import Mensam.Auth.Bearer
import Mensam.Error
import Time
import Url.Builder


type Request
    = BasicAuth
        { username : String
        , password : String
        }
    | Bearer
        { jwt : Mensam.Auth.Bearer.Jwt
        }


type Response
    = Success { jwt : Mensam.Auth.Bearer.Jwt, expiration : Maybe Time.Posix }
    | ErrorAuth ErrorAuth


type ErrorAuth
    = ErrorAuthUsername
    | ErrorAuthPassword
    | ErrorAuthIndefinite


errorAuth : ErrorAuth -> Mensam.Error.Error
errorAuth error =
    Mensam.Error.message "Authentication failed" <|
        case error of
            ErrorAuthUsername ->
                Mensam.Error.message "Unknown username" Mensam.Error.undefined

            ErrorAuthPassword ->
                Mensam.Error.message "Bad password" Mensam.Error.undefined

            ErrorAuthIndefinite ->
                Mensam.Error.message "Indefinite" Mensam.Error.undefined


request : Request -> (Result Http.Error Response -> a) -> Cmd a
request body handleResult =
    Http.request
        { method = "POST"
        , headers =
            [ case body of
                BasicAuth { username, password } ->
                    Http.header
                        "Authorization"
                        ("Basic " ++ Base64.encode (username ++ ":" ++ password))

                Bearer { jwt } ->
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
                    case Json.Decode.decodeString decodeBody401 body of
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
                        Ok value ->
                            Ok <| Success value

                        Err err ->
                            Err <| Http.BadBody <| Json.Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status


decodeBody200 : Json.Decode.Decoder { jwt : Mensam.Auth.Bearer.Jwt, expiration : Maybe Time.Posix }
decodeBody200 =
    Json.Decode.map2 (\jwt expiration -> { jwt = jwt, expiration = expiration })
        (Json.Decode.field "jwt" Mensam.Auth.Bearer.decode)
        (Json.Decode.maybe <| Json.Decode.field "expiration" Iso8601.decoder)


decodeBody401 : Json.Decode.Decoder ErrorAuth
decodeBody401 =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "username" ->
                        Json.Decode.succeed ErrorAuthUsername

                    "password" ->
                        Json.Decode.succeed ErrorAuthPassword

                    "indefinite" ->
                        Json.Decode.succeed ErrorAuthIndefinite

                    _ ->
                        Json.Decode.fail <| "Trying to decode authentication error, but this option is not supported: " ++ string
            )
