module Mensam.Api.Register exposing (..)

import Http
import Json.Decode
import Json.Encode
import Url.Builder


type alias Request =
    { email : String
    , emailVisible : Bool
    , name : String
    , password : String
    }


type Response
    = Success
    | ErrorBody String


request : Request -> (Result Http.Error Response -> a) -> Cmd a
request body handleResult =
    Http.request
        { method = "POST"
        , headers = []
        , url =
            Url.Builder.absolute
                [ "api"
                , "register"
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

                status ->
                    Err <| Http.BadStatus status

        Http.GoodStatus_ metadata body ->
            case metadata.statusCode of
                201 ->
                    case Json.Decode.decodeString decodeBody201 body of
                        Ok () ->
                            Ok <| Success

                        Err err ->
                            Err <| Http.BadBody <| Json.Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status


encodeBody : Request -> Json.Encode.Value
encodeBody body =
    Json.Encode.object
        [ ( "email", Json.Encode.string body.email )
        , ( "email-visible", Json.Encode.bool body.emailVisible )
        , ( "name", Json.Encode.string body.name )
        , ( "password", Json.Encode.string body.password )
        ]


decodeBody201 : Json.Decode.Decoder ()
decodeBody201 =
    Json.Decode.list (Json.Decode.succeed ())
        |> Json.Decode.andThen
            (\x ->
                case x of
                    [] ->
                        Json.Decode.succeed ()

                    _ ->
                        Json.Decode.fail "Trying to decode empty list, but list has elements."
            )


decodeBody400 : Json.Decode.Decoder String
decodeBody400 =
    Json.Decode.field "error" Json.Decode.string
