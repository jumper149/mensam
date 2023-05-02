module Mensam.Api.ReservationCreate exposing (..)

import Http
import Iso8601
import Json.Decode
import Json.Encode
import Mensam.Auth.Bearer
import Time
import Url.Builder


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , desk : { id : Int }
    , timeWindow :
        { start : Time.Posix
        , end : Time.Posix
        }
    }


type Response
    = Success { id : Int }
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
                400 ->
                    case Json.Decode.decodeString decodeBody400 body of
                        Ok error ->
                            Ok <| ErrorBody error

                        Err err ->
                            Err <| Http.BadBody <| Json.Decode.errorToString err

                401 ->
                    case Json.Decode.decodeString Mensam.Auth.Bearer.http401BodyDecoder body of
                        Ok error ->
                            Ok <| ErrorAuth error

                        Err err ->
                            Err <| Http.BadBody <| Json.Decode.errorToString err

                409 ->
                    case Json.Decode.decodeString decodeBody409 body of
                        Ok () ->
                            Ok <| ErrorTimeUnavailable

                        Err err ->
                            Err <| Http.BadBody <| Json.Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status

        Http.GoodStatus_ metadata body ->
            case metadata.statusCode of
                201 ->
                    case Json.Decode.decodeString decodeBody201 body of
                        Ok response ->
                            Ok <| Success response

                        Err err ->
                            Err <| Http.BadBody <| Json.Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status


encodeBody : Request -> Json.Encode.Value
encodeBody body =
    Json.Encode.object
        [ ( "desk"
          , Json.Encode.object
                [ ( "tag", Json.Encode.string "identifier" )
                , ( "value", Json.Encode.int body.desk.id )
                ]
          )
        , ( "time-window"
          , Json.Encode.object
                [ ( "start", Iso8601.encode body.timeWindow.start )
                , ( "end", Iso8601.encode body.timeWindow.end )
                ]
          )
        ]


decodeBody201 : Json.Decode.Decoder { id : Int }
decodeBody201 =
    Json.Decode.map
        (\id -> { id = id })
        (Json.Decode.field "id" Json.Decode.int)


decodeBody400 : Json.Decode.Decoder String
decodeBody400 =
    Json.Decode.field "error" Json.Decode.string


decodeBody409 : Json.Decode.Decoder ()
decodeBody409 =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "Desk is not available within the given time window." ->
                        Json.Decode.succeed ()

                    _ ->
                        Json.Decode.fail <| "Unexpected HTTP 409 message: " ++ string
            )
