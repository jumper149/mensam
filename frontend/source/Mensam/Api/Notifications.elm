module Mensam.Api.Notifications exposing (..)

import Http
import Http.Extra
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Auth.Bearer
import Url.Builder


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , receiveEmailNotifications : Maybe Bool
    }


type Response
    = Success
        { receiveEmailNotifications : Bool
        }
    | ErrorEmailNotVerified
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
                , "notifications"
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
                    case Decode.decodeString decodeBody403 body of
                        Ok () ->
                            Ok ErrorEmailNotVerified

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


encodeBody : Request -> Encode.Value
encodeBody body =
    Encode.object
        [ ( "receive-notifications"
          , case body.receiveEmailNotifications of
                Nothing ->
                    Encode.null

                Just receiveEmailNotifications ->
                    Encode.bool receiveEmailNotifications
          )
        ]


decodeBody200 : Decode.Decoder { receiveEmailNotifications : Bool }
decodeBody200 =
    Decode.map (\receiveEmailNotifications -> { receiveEmailNotifications = receiveEmailNotifications })
        (Decode.field "receive-notifications" Decode.bool)


decodeBody400 : Decode.Decoder String
decodeBody400 =
    Decode.field "error" Decode.string


decodeBody403 : Decode.Decoder ()
decodeBody403 =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "Email address is not verified." ->
                        Decode.succeed ()

                    _ ->
                        Decode.fail <| "Unexpected HTTP 403 message: " ++ string
            )
