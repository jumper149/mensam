module Mensam.Api.DeskCreate exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Auth.Bearer
import Mensam.Desk
import Mensam.Space
import Mensam.Space.Role
import Mensam.Http.Tracker
import Mensam.Url


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , space : Mensam.Space.Identifier
    , name : Mensam.Desk.Name
    , location : Maybe Mensam.Desk.Location
    }


type Response
    = Success { id : Mensam.Desk.Identifier }
    | ErrorSpaceNotFound
    | ErrorInsufficientPermission Mensam.Space.Role.Permission
    | ErrorBody String
    | ErrorAuth Mensam.Auth.Bearer.Error


request : Maybe Mensam.Http.Tracker.Tracker -> Mensam.Url.BaseUrl -> Request -> (Result Http.Error Response -> a) -> Cmd a
request tracker baseUrl body handleResult =
    Http.request
        { method = "PUT"
        , headers =
            [ Mensam.Auth.Bearer.authorizationHeader body.jwt
            ]
        , url =
            Mensam.Url.absolute baseUrl
                [ "api"
                , "desk"
                , "create"
                ]
                []
        , body = Http.jsonBody <| encodeBody body
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
          , Mensam.Desk.nameEncode body.name
          )
        , ( "space"
          , Encode.object
                [ ( "tag"
                  , Encode.string "identifier"
                  )
                , ( "value"
                  , Mensam.Space.identifierEncode body.space
                  )
                ]
          )
        , ( "location"
          , case body.location of
                Nothing ->
                    Encode.null

                Just location ->
                    Mensam.Desk.locationEncode location
          )
        ]


decodeBody201 : Decode.Decoder { id : Mensam.Desk.Identifier }
decodeBody201 =
    Decode.map
        (\id -> { id = id })
        (Decode.field "id" Mensam.Desk.identifierDecoder)


decodeBody400 : Decode.Decoder String
decodeBody400 =
    Decode.field "error" Decode.string


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
