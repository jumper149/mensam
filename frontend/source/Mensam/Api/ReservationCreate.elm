module Mensam.Api.ReservationCreate exposing (..)

import Http
import Http.Extra
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Auth.Bearer
import Mensam.Desk
import Mensam.Reservation
import Time
import Url.Builder


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , desk : { id : Mensam.Desk.Identifier }
    , timeWindow :
        { start : Time.Posix
        , end : Time.Posix
        }
    }


type Response
    = Success { id : Mensam.Reservation.Identifier }
    | ErrorTimeUnavailable
    | ErrorBody String
    | ErrorAuth Mensam.Auth.Bearer.Error


request : Request -> (Result Http.Error Response -> a) -> Cmd a
request body handleResult =
    Http.request
        { method = "PUT"
        , headers =
            [ Mensam.Auth.Bearer.authorizationHeader body.jwt
            ]
        , url =
            Url.Builder.absolute
                [ "api"
                , "reservation"
                , "create"
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

                409 ->
                    case Decode.decodeString decodeBody409 body of
                        Ok () ->
                            Ok <| ErrorTimeUnavailable

                        Err err ->
                            Err <| Http.BadBody <| Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status

        Http.GoodStatus_ metadata body ->
            case metadata.statusCode of
                201 ->
                    case Decode.decodeString decodeBody201 body of
                        Ok response ->
                            Ok <| Success response

                        Err err ->
                            Err <| Http.BadBody <| Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status


encodeBody : Request -> Encode.Value
encodeBody body =
    Encode.object
        [ ( "desk"
          , Encode.object
                [ ( "tag", Encode.string "identifier" )
                , ( "value", Mensam.Desk.identifierEncode body.desk.id )
                ]
          )
        , ( "time-window"
          , Encode.object
                [ ( "start", Iso8601.encode body.timeWindow.start )
                , ( "end", Iso8601.encode body.timeWindow.end )
                ]
          )
        ]


decodeBody201 : Decode.Decoder { id : Mensam.Reservation.Identifier }
decodeBody201 =
    Decode.map
        (\id -> { id = id })
        (Decode.field "id" Mensam.Reservation.identifierDecoder)


decodeBody400 : Decode.Decoder String
decodeBody400 =
    Decode.field "error" Decode.string


decodeBody409 : Decode.Decoder ()
decodeBody409 =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "Desk is not available within the given time window." ->
                        Decode.succeed ()

                    _ ->
                        Decode.fail <| "Unexpected HTTP 409 message: " ++ string
            )
