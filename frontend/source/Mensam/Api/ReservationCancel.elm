module Mensam.Api.ReservationCancel exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Auth.Bearer
import Mensam.Http.Tracker
import Mensam.Reservation
import Mensam.Space.Role
import Mensam.Url


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , id : Mensam.Reservation.Identifier
    }


type Response
    = Success
    | ErrorInsufficientPermission Mensam.Space.Role.Permission
    | ErrorAlreadyCancelled
    | ErrorAlreadyHappened
    | ErrorBody String
    | ErrorAuth Mensam.Auth.Bearer.Error


request : Maybe Mensam.Http.Tracker.Tracker -> Mensam.Url.BaseUrl -> Request -> (Result Http.Error Response -> a) -> Cmd a
request tracker baseUrl body handleResult =
    Http.request
        { method = "POST"
        , headers =
            [ Mensam.Auth.Bearer.authorizationHeader body.jwt
            ]
        , url =
            Mensam.Url.absolute baseUrl
                [ "api"
                , "reservation"
                , "cancel"
                ]
                []
        , body = Http.jsonBody <| encodeBody body
        , expect = Http.expectStringResponse handleResult responseResult
        , timeout = Nothing
        , tracker = Maybe.map Mensam.Http.Tracker.toHttp tracker
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

                409 ->
                    case Decode.decodeString decodeBody409 body of
                        Ok () ->
                            Ok ErrorAlreadyCancelled

                        Err err ->
                            Err <| Http.BadBody <| Decode.errorToString err

                410 ->
                    case Decode.decodeString decodeBody410 body of
                        Ok () ->
                            Ok ErrorAlreadyHappened

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
          , Mensam.Reservation.identifierEncode body.id
          )
        ]


decodeBody200 : Decode.Decoder ()
decodeBody200 =
    Decode.map (\_ -> ())
        (Decode.field "unit" <| Decode.list <| Decode.succeed ())


decodeBody400 : Decode.Decoder String
decodeBody400 =
    Decode.field "error" Decode.string


decodeBody409 : Decode.Decoder ()
decodeBody409 =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "Already cancelled." ->
                        Decode.succeed ()

                    _ ->
                        Decode.fail <| "Unexpected HTTP 409 message: " ++ string
            )


decodeBody410 : Decode.Decoder ()
decodeBody410 =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "Already happened." ->
                        Decode.succeed ()

                    _ ->
                        Decode.fail <| "Unexpected HTTP 410 message: " ++ string
            )
