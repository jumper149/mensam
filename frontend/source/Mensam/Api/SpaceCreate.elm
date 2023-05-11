module Mensam.Api.SpaceCreate exposing (..)

import Http
import Json.Decode
import Json.Encode
import Mensam.Auth.Bearer
import Mensam.Space
import Url.Builder


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , name : Mensam.Space.Name
    , accessibility : Mensam.Space.Accessibility
    , visibility : Mensam.Space.Visibility
    }


type Response
    = Success { id : Mensam.Space.Identifier }
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
                , "space"
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
        [ ( "name"
          , Mensam.Space.nameEncode body.name
          )
        , ( "accessibility"
          , Mensam.Space.accessibilityEncode body.accessibility
          )
        , ( "visibility"
          , Mensam.Space.visibilityEncode body.visibility
          )
        ]


decodeBody201 : Json.Decode.Decoder { id : Mensam.Space.Identifier }
decodeBody201 =
    Json.Decode.map
        (\id -> { id = id })
        (Json.Decode.field "id" Mensam.Space.identifierDecoder)


decodeBody400 : Json.Decode.Decoder String
decodeBody400 =
    Json.Decode.field "error" Json.Decode.string
