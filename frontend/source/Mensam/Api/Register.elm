module Mensam.Api.Register exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Url.Builder


type alias Request =
    { email : String
    , emailVisible : Bool
    , name : String
    , password : String
    }


type Response
    = Success
        { emailSent : Bool
        }
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
                    case Decode.decodeString decodeBody400 body of
                        Ok error ->
                            Ok <| ErrorBody error

                        Err err ->
                            Err <| Http.BadBody <| Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status

        Http.GoodStatus_ metadata body ->
            case metadata.statusCode of
                201 ->
                    case Decode.decodeString decodeBody201 body of
                        Ok value ->
                            Ok <| Success value

                        Err err ->
                            Err <| Http.BadBody <| Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status


encodeBody : Request -> Encode.Value
encodeBody body =
    Encode.object
        [ ( "email", Encode.string body.email )
        , ( "email-visible", Encode.bool body.emailVisible )
        , ( "name", Encode.string body.name )
        , ( "password", Encode.string body.password )
        ]


decodeBody201 : Decode.Decoder { emailSent : Bool }
decodeBody201 =
    Decode.map (\emailSent -> { emailSent = emailSent })
        (Decode.field "email-sent" Decode.bool)


decodeBody400 : Decode.Decoder String
decodeBody400 =
    Decode.field "error" Decode.string
