module Mensam.Api.DeskList exposing (..)

import Http
import Iso8601
import Json.Decode
import Json.Encode
import Mensam.Error
import Mensam.Jwt
import Time
import Url.Builder


type alias Request =
    { jwt : Mensam.Jwt.Jwt
    , space : { id : Int }
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
    | ErrorAuth ErrorAuth


type ErrorAuth
    = ErrorAuthUsername
    | ErrorAuthPassword
    | ErrorAuthIndefinite


errorAuth : ErrorAuth -> Mensam.Error.Error
errorAuth error =
    case error of
        ErrorAuthUsername ->
            Mensam.Error.message "Unknown username" Mensam.Error.undefined

        ErrorAuthPassword ->
            Mensam.Error.message "Bad password" Mensam.Error.undefined

        ErrorAuthIndefinite ->
            Mensam.Error.message "Indefinite" Mensam.Error.undefined


request : Request -> (Result Http.Error Response -> a) -> Cmd a
request body handleResult =
    Http.request
        { method = "POST"
        , headers =
            [ Mensam.Jwt.authorizationHeader body.jwt
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
                    case Json.Decode.decodeString decodeBody400 body of
                        Ok error ->
                            Ok <| ErrorBody error

                        Err err ->
                            Err <| Http.BadBody <| Json.Decode.errorToString err

                401 ->
                    case Json.Decode.decodeString decodeBody401 body of
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
        [ ( "space"
          , Json.Encode.object
                [ ( "tag", Json.Encode.string "identifier" )
                , ( "value", Json.Encode.int body.space.id )
                ]
          )
        ]


decodeBody200 : Json.Decode.Decoder { desks : List { desk : { id : Int, name : String, space : Int }, reservations : List { desk : Int, id : Int, status : String, timeBegin : Time.Posix, timeEnd : Time.Posix, user : Int } } }
decodeBody200 =
    let
        decodeDesk =
            Json.Decode.map3
                (\id name space -> { id = id, name = name, space = space })
                (Json.Decode.field "id" Json.Decode.int)
                (Json.Decode.field "name" Json.Decode.string)
                (Json.Decode.field "space" Json.Decode.int)

        decodeReservation =
            Json.Decode.map6
                (\desk id status timeBegin timeEnd user -> { desk = desk, id = id, status = status, timeBegin = timeBegin, timeEnd = timeEnd, user = user })
                (Json.Decode.field "desk" Json.Decode.int)
                (Json.Decode.field "id" Json.Decode.int)
                (Json.Decode.field "status" Json.Decode.string)
                (Json.Decode.field "time-begin" Iso8601.decoder)
                (Json.Decode.field "time-end" Iso8601.decoder)
                (Json.Decode.field "user" Json.Decode.int)
    in
    Json.Decode.map (\desks -> { desks = desks }) <|
        Json.Decode.field "desks" <|
            Json.Decode.list <|
                Json.Decode.map2
                    (\desk reservations -> { desk = desk, reservations = reservations })
                    (Json.Decode.field "desk" decodeDesk)
                    (Json.Decode.field "reservations" <| Json.Decode.list <| decodeReservation)


decodeBody400 : Json.Decode.Decoder String
decodeBody400 =
    Json.Decode.field "error" Json.Decode.string


decodeBody401 : Json.Decode.Decoder ErrorAuth
decodeBody401 =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "username" ->
                        Json.Decode.succeed ErrorAuthUsername

                    "password" ->
                        Json.Decode.succeed ErrorAuthPassword

                    "indefinite" ->
                        Json.Decode.succeed ErrorAuthIndefinite

                    _ ->
                        Json.Decode.fail <| "Trying to decode authentication error, but this option is not supported: " ++ string
            )
