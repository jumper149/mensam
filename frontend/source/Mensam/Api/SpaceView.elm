module Mensam.Api.SpaceView exposing (..)

import Dict
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Auth.Bearer
import Mensam.Space
import Mensam.Time
import Set
import Url.Builder


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , id : Mensam.Space.Identifier
    }


type Response
    = Success { space : Mensam.Space.SpaceView }
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
                , "view"
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
                        Ok response ->
                            Ok <| Success response

                        Err err ->
                            Err <| Http.BadBody <| Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status


encodeBody : Request -> Encode.Value
encodeBody body =
    Encode.object
        [ ( "id"
          , Mensam.Space.identifierEncode body.id
          )
        ]


decodeBody200 : Decode.Decoder { space : Mensam.Space.SpaceView }
decodeBody200 =
    Decode.map (\spaceView -> { space = spaceView }) <|
        Decode.field "space" <|
            Decode.map6
                (\id name roles timezone visibility yourRoleId ->
                    Mensam.Space.MkSpaceView
                        { id = id
                        , name = Mensam.Space.MkName name
                        , roles = Dict.fromList <| List.map (\role -> ( role.name, role )) roles
                        , timezone = timezone
                        , visibility = visibility
                        , yourRole = Dict.get yourRoleId <| Dict.fromList <| List.map (\role -> ( role.id, role )) roles
                        }
                )
                (Decode.field "id" Mensam.Space.identifierDecoder)
                (Decode.field "name" Decode.string)
                (Decode.field "roles" <|
                    Decode.list <|
                        Decode.map4
                            (\accessibility id name permissions ->
                                { accessibility = accessibility
                                , id = id
                                , name = name
                                , permissions = Set.fromList permissions
                                }
                            )
                            (Decode.field "accessibility" Mensam.Space.accessibilityDecoder)
                            (Decode.field "id" Decode.int)
                            (Decode.field "name" Decode.string)
                            (Decode.field "permissions" <| Decode.list Decode.string)
                )
                (Decode.field "timezone" Mensam.Time.timezoneIdentifierDecoder)
                (Decode.field "visibility" Mensam.Space.visibilityDecoder)
                (Decode.field "your-role" Decode.int)


decodeBody400 : Decode.Decoder String
decodeBody400 =
    Decode.field "error" Decode.string
