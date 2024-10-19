module Mensam.Api.DeskList exposing (..)

import Http
import Http.Extra
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Auth.Bearer
import Mensam.Desk
import Mensam.Reservation
import Mensam.Space
import Mensam.Space.Role
import Time
import Url.Builder


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , space : Mensam.Space.Identifier
    , timeWindow :
        { start : Maybe Time.Posix
        , end : Maybe Time.Posix
        }
    }


type Response
    = Success
        { desks :
            List
                { desk :
                    { id : Mensam.Desk.Identifier
                    , name : Mensam.Desk.Name
                    , space : Mensam.Space.Identifier
                    , location : Maybe Mensam.Desk.Location
                    }
                , reservations :
                    List
                        { desk : Mensam.Desk.Identifier
                        , id : Mensam.Reservation.Identifier
                        , status : Mensam.Reservation.Status
                        , timeBegin : Time.Posix
                        , timeEnd : Time.Posix
                        , user : Int
                        }
                }
        }
    | ErrorInsufficientPermission Mensam.Space.Role.Permission
    | ErrorSpaceNotFound
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
        , ( "time-window"
          , Encode.object
                [ ( "start"
                  , case body.timeWindow.start of
                        Nothing ->
                            Encode.null

                        Just time ->
                            Iso8601.encode time
                  )
                , ( "end"
                  , case body.timeWindow.end of
                        Nothing ->
                            Encode.null

                        Just time ->
                            Iso8601.encode time
                  )
                ]
          )
        ]


decodeBody200 :
    Decode.Decoder
        { desks :
            List
                { desk :
                    { id : Mensam.Desk.Identifier
                    , name : Mensam.Desk.Name
                    , space : Mensam.Space.Identifier
                    , location : Maybe Mensam.Desk.Location
                    }
                , reservations :
                    List
                        { desk : Mensam.Desk.Identifier
                        , id : Mensam.Reservation.Identifier
                        , status : Mensam.Reservation.Status
                        , timeBegin : Time.Posix
                        , timeEnd : Time.Posix
                        , user : Int
                        }
                }
        }
decodeBody200 =
    let
        decodeDesk =
            Decode.map4
                (\id name space location -> { id = id, name = name, space = space, location = location })
                (Decode.field "id" Mensam.Desk.identifierDecoder)
                (Decode.field "name" Mensam.Desk.nameDecoder)
                (Decode.field "space" Mensam.Space.identifierDecoder)
                (Decode.field "location" <| Decode.nullable Mensam.Desk.locationDecoder)

        decodeReservation =
            Decode.map6
                (\desk id status timeBegin timeEnd user -> { desk = desk, id = id, status = status, timeBegin = timeBegin, timeEnd = timeEnd, user = user })
                (Decode.field "desk" Mensam.Desk.identifierDecoder)
                (Decode.field "id" Mensam.Reservation.identifierDecoder)
                (Decode.field "status" Mensam.Reservation.statusDecoder)
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
