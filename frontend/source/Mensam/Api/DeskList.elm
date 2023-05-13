module Mensam.Api.DeskList exposing (..)

import Http
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Auth.Bearer
import Mensam.Space
import Time
import Url.Builder


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , space : Mensam.Space.Identifier
    }


type Response
    = Success
        { desks :
            List
                { desk :
                    { id : Int
                    , name : String
                    , space : Int
                    }
                , reservations :
                    List
                        { desk : Int
                        , id : Int
                        , status : String
                        , timeBegin : Time.Posix
                        , timeEnd : Time.Posix
                        , user : Int
                        }
                }
        }
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
                , "desk"
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
        [ ( "space"
          , Encode.object
                [ ( "tag", Encode.string "identifier" )
                , ( "value", Mensam.Space.identifierEncode body.space )
                ]
          )
        ]


decodeBody200 : Decode.Decoder { desks : List { desk : { id : Int, name : String, space : Int }, reservations : List { desk : Int, id : Int, status : String, timeBegin : Time.Posix, timeEnd : Time.Posix, user : Int } } }
decodeBody200 =
    let
        decodeDesk =
            Decode.map3
                (\id name space -> { id = id, name = name, space = space })
                (Decode.field "id" Decode.int)
                (Decode.field "name" Decode.string)
                (Decode.field "space" Decode.int)

        decodeReservation =
            Decode.map6
                (\desk id status timeBegin timeEnd user -> { desk = desk, id = id, status = status, timeBegin = timeBegin, timeEnd = timeEnd, user = user })
                (Decode.field "desk" Decode.int)
                (Decode.field "id" Decode.int)
                (Decode.field "status" Decode.string)
                (Decode.field "time-begin" Iso8601.decoder)
                (Decode.field "time-end" Iso8601.decoder)
                (Decode.field "user" Decode.int)
    in
    Decode.map (\desks -> { desks = desks }) <|
        Decode.field "desks" <|
            Decode.list <|
                Decode.map2
                    (\desk reservations -> { desk = desk, reservations = reservations })
                    (Decode.field "desk" decodeDesk)
                    (Decode.field "reservations" <| Decode.list <| decodeReservation)


decodeBody400 : Decode.Decoder String
decodeBody400 =
    Decode.field "error" Decode.string
