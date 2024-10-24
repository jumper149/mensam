module Mensam.Api.SpaceCreate exposing (..)

import Http
import Http.Extra
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Auth.Bearer
import Mensam.Space
import Mensam.Time
import Mensam.Url


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , name : Mensam.Space.Name
    , timezone : Mensam.Time.Timezone
    , visibility : Mensam.Space.Visibility
    }


type Response
    = Success { id : Mensam.Space.Identifier }
    | ErrorBody String
    | ErrorAuth Mensam.Auth.Bearer.Error


request : Mensam.Url.BaseUrl -> Request -> (Result Http.Error Response -> a) -> Cmd a
request baseUrl body handleResult =
    Http.request
        { method = "PUT"
        , headers =
            [ Mensam.Auth.Bearer.authorizationHeader body.jwt
            ]
        , url =
            Mensam.Url.absolute baseUrl
                [ "api"
                , "space"
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
        [ ( "name"
          , Mensam.Space.nameEncode body.name
          )
        , ( "timezone"
          , Mensam.Time.timezoneEncode body.timezone
          )
        , ( "visibility"
          , Mensam.Space.visibilityEncode body.visibility
          )
        ]


decodeBody201 : Decode.Decoder { id : Mensam.Space.Identifier }
decodeBody201 =
    Decode.map
        (\id -> { id = id })
        (Decode.field "id" Mensam.Space.identifierDecoder)


decodeBody400 : Decode.Decoder String
decodeBody400 =
    Decode.field "error" Decode.string
