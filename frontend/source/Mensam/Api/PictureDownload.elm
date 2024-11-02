module Mensam.Api.PictureDownload exposing (..)

import Base64.Encode
import Bytes
import Http
import Mensam.Auth.Bearer
import Mensam.Tracker
import Mensam.Url
import Mensam.User
import Url.Builder


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , user : Mensam.User.Identifier
    }


type Response
    = Success { url : String }


request : Maybe Mensam.Tracker.Tracker -> Mensam.Url.BaseUrl -> Request -> (Result Http.Error Response -> a) -> Cmd a
request tracker baseUrl body handleResult =
    Http.request
        { method = "GET"
        , headers =
            [ Mensam.Auth.Bearer.authorizationHeader body.jwt
            ]
        , url =
            Mensam.Url.absolute baseUrl
                [ "api"
                , "picture"
                ]
                [ Url.Builder.string "user" (Mensam.User.identifierToString body.user) ]
        , body = Http.emptyBody
        , expect = Http.expectBytesResponse handleResult responseResult
        , timeout = Nothing
        , tracker = Maybe.map Mensam.Tracker.toHttp tracker
        }


responseResult : Http.Response Bytes.Bytes -> Result Http.Error Response
responseResult httpResponse =
    case httpResponse of
        Http.BadUrl_ err ->
            Err <| Http.BadUrl err

        Http.Timeout_ ->
            Err <| Http.Timeout

        Http.NetworkError_ ->
            Err <| Http.NetworkError

        Http.BadStatus_ metadata _ ->
            case metadata.statusCode of
                status ->
                    Err <| Http.BadStatus status

        Http.GoodStatus_ metadata body ->
            case metadata.statusCode of
                200 ->
                    Ok <| Success <| decodeBody200 body

                status ->
                    Err <| Http.BadStatus status


decodeBody200 : Bytes.Bytes -> { url : String }
decodeBody200 bytes =
    let
        base64Jpeg =
            Base64.Encode.encode <| Base64.Encode.bytes bytes

        base64Url =
            "data:image/png;base64, " ++ base64Jpeg
    in
    { url = base64Url }
