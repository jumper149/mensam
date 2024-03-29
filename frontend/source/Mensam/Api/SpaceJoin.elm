module Mensam.Api.SpaceJoin exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Auth.Bearer
import Mensam.NameOrIdentifier
import Mensam.Space
import Url.Builder


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , role : Mensam.NameOrIdentifier.NameOrIdentifier String Int
    , space : Mensam.NameOrIdentifier.NameOrIdentifier Mensam.Space.Name Mensam.Space.Identifier
    }


type Response
    = Success
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
                , "join"
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
                            Ok <| Success

                        Err err ->
                            Err <| Http.BadBody <| Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status


encodeBody : Request -> Encode.Value
encodeBody body =
    Encode.object
        [ ( "role"
          , Mensam.NameOrIdentifier.encode Encode.string Encode.int body.role
          )
        , ( "space"
          , Mensam.NameOrIdentifier.encode Mensam.Space.nameEncode Mensam.Space.identifierEncode body.space
          )
        ]


decodeBody200 : Decode.Decoder ()
decodeBody200 =
    Decode.map (\_ -> ())
        (Decode.field "unit" <| Decode.list <| Decode.succeed ())


decodeBody400 : Decode.Decoder String
decodeBody400 =
    Decode.field "error" Decode.string
