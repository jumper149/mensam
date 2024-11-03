module Mensam.Api.SpacePictureDelete exposing (..)

import Http
import Json.Decode as Decode
import Mensam.Auth.Bearer
import Mensam.Space
import Mensam.Space.Role
import Mensam.Http.Tracker
import Mensam.Url
import Url.Builder


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , space : Mensam.Space.Identifier
    }


type Response
    = Success
    | ErrorInsufficientPermission Mensam.Space.Role.Permission
    | ErrorSpaceNotFound
    | ErrorAuth Mensam.Auth.Bearer.Error


request : Maybe Mensam.Http.Tracker.Tracker -> Mensam.Url.BaseUrl -> Request -> (Result Http.Error Response -> a) -> Cmd a
request tracker baseUrl body handleResult =
    Http.request
        { method = "DELETE"
        , headers =
            [ Mensam.Auth.Bearer.authorizationHeader body.jwt
            ]
        , url =
            Mensam.Url.absolute baseUrl
                [ "api"
                , "space"
                , "picture"
                ]
                [ Url.Builder.string "space" (Mensam.Space.identifierToString body.space) ]
        , body = Http.emptyBody
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

                404 ->
                    case Decode.decodeString decodeBody404 body of
                        Ok () ->
                            Ok <| ErrorSpaceNotFound

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


decodeBody200 : Decode.Decoder ()
decodeBody200 =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "Deleted space picture." ->
                        Decode.succeed ()

                    _ ->
                        Decode.fail <| "Unexpected HTTP 200 message: " ++ string
            )


decodeBody404 : Decode.Decoder ()
decodeBody404 =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "Space not found." ->
                        Decode.succeed ()

                    _ ->
                        Decode.fail <| "Unexpected HTTP 404 message: " ++ string
            )
