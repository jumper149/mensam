module Mensam.Api.SpaceList exposing (..)

import Http
import Http.Extra
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Auth.Bearer
import Mensam.Space
import Mensam.Time
import Mensam.User
import Url.Builder


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , order : List { category : String, order : String }
    }


type Response
    = Success { spaces : List Mensam.Space.Space }
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
        [ ( "order"
          , Encode.list
                (\x ->
                    Encode.object
                        [ ( "category", Encode.string x.category )
                        , ( "order", Encode.string x.order )
                        ]
                )
                body.order
          )
        ]


decodeBody200 : Decode.Decoder { spaces : List Mensam.Space.Space }
decodeBody200 =
    Decode.map (\x -> { spaces = x }) <|
        Decode.field "spaces" <|
            Decode.list <|
                Decode.map4
                    (\id name timezone owner ->
                        Mensam.Space.MkSpace
                            { id = id
                            , name = name
                            , timezone = timezone
                            , owner = owner
                            }
                    )
                    (Decode.field "id" Mensam.Space.identifierDecoder)
                    (Decode.field "name" Mensam.Space.nameDecoder)
                    (Decode.field "timezone" Mensam.Time.timezoneIdentifierDecoder)
                    (Decode.field "owner" Mensam.User.identifierDecoder)


decodeBody400 : Decode.Decoder String
decodeBody400 =
    Decode.field "error" Decode.string
