module Mensam.Api.SpaceList exposing (..)

import Http
import Json.Decode
import Json.Encode
import Mensam.Auth.Bearer
import Url.Builder


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , order : List { category : String, order : String }
    }


type Response
    = Success { spaces : List { id : Int, name : String } }
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
                , "space"
                , "list"
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

                status ->
                    Err <| Http.BadStatus status

        Http.GoodStatus_ metadata body ->
            case metadata.statusCode of
                200 ->
                    case Json.Decode.decodeString decodeBody200 body of
                        Ok response ->
                            Ok <| Success response

                        Err err ->
                            Err <| Http.BadBody <| Json.Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status


encodeBody : Request -> Json.Encode.Value
encodeBody body =
    Json.Encode.object
        [ ( "order"
          , Json.Encode.list
                (\x ->
                    Json.Encode.object
                        [ ( "category", Json.Encode.string x.category )
                        , ( "order", Json.Encode.string x.order )
                        ]
                )
                body.order
          )
        ]


decodeBody200 : Json.Decode.Decoder { spaces : List { id : Int, name : String } }
decodeBody200 =
    Json.Decode.map (\x -> { spaces = x }) <|
        Json.Decode.field "spaces" <|
            Json.Decode.list <|
                Json.Decode.map2
                    (\x y -> { id = x, name = y })
                    (Json.Decode.field "id" Json.Decode.int)
                    (Json.Decode.field "name" Json.Decode.string)


decodeBody400 : Json.Decode.Decoder String
decodeBody400 =
    Json.Decode.field "error" Json.Decode.string
