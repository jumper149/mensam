module Mensam.Api.ReservationList exposing (..)

import Http
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Auth.Bearer
import Mensam.Desk
import Mensam.Reservation
import Mensam.Space
import Mensam.Time
import Mensam.Tracker
import Mensam.Url
import Mensam.User
import Time


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , timeWindow :
        { start : Maybe Time.Posix
        , end : Maybe Time.Posix
        }
    }


type Response
    = Success
        { reservations :
            List
                { desk :
                    { id : Mensam.Desk.Identifier
                    , name : Mensam.Desk.Name
                    , location : Maybe Mensam.Desk.Location
                    }
                , reservation :
                    { id : Mensam.Reservation.Identifier
                    , status : Mensam.Reservation.Status
                    , timeBegin : Time.Posix
                    , timeEnd : Time.Posix
                    }
                , space :
                    { id : Mensam.Space.Identifier
                    , name : Mensam.Space.Name
                    , timezone : Mensam.Time.Timezone
                    , owner : Mensam.User.Identifier
                    }
                , user :
                    { id : Mensam.User.Identifier
                    }
                }
        }
    | ErrorBody String
    | ErrorAuth Mensam.Auth.Bearer.Error


request : Maybe Mensam.Tracker.Tracker -> Mensam.Url.BaseUrl -> Request -> (Result Http.Error Response -> a) -> Cmd a
request tracker baseUrl body handleResult =
    Http.request
        { method = "POST"
        , headers =
            [ Mensam.Auth.Bearer.authorizationHeader body.jwt
            ]
        , url =
            Mensam.Url.absolute baseUrl
                [ "api"
                , "reservation"
                , "list"
                ]
                []
        , body = Http.jsonBody <| encodeBody body
        , expect = Http.expectStringResponse handleResult responseResult
        , timeout = Nothing
        , tracker = Maybe.map Mensam.Tracker.toHttp tracker
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
        [ ( "time-window"
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
        { reservations :
            List
                { desk :
                    { id : Mensam.Desk.Identifier
                    , name : Mensam.Desk.Name
                    , location : Maybe Mensam.Desk.Location
                    }
                , reservation :
                    { id : Mensam.Reservation.Identifier
                    , status : Mensam.Reservation.Status
                    , timeBegin : Time.Posix
                    , timeEnd : Time.Posix
                    }
                , space :
                    { id : Mensam.Space.Identifier
                    , name : Mensam.Space.Name
                    , timezone : Mensam.Time.Timezone
                    , owner : Mensam.User.Identifier
                    }
                , user :
                    { id : Mensam.User.Identifier
                    }
                }
        }
decodeBody200 =
    Decode.map (\x -> { reservations = x }) <|
        Decode.field "reservations" <|
            Decode.list <|
                Decode.map3
                    (\desk reservation space ->
                        { desk =
                            { id = desk.id
                            , name = desk.name
                            , location = desk.location
                            }
                        , reservation =
                            { id = reservation.id
                            , status = reservation.status
                            , timeBegin = reservation.timeBegin
                            , timeEnd = reservation.timeEnd
                            }
                        , space =
                            { id = space.id
                            , name = space.name
                            , timezone = space.timezone
                            , owner = space.owner
                            }
                        , user =
                            { id = reservation.user
                            }
                        }
                    )
                    (Decode.field "desk" <|
                        Decode.map4 (\id name space location -> { id = id, name = name, space = space, location = location })
                            (Decode.field "id" Mensam.Desk.identifierDecoder)
                            (Decode.field "name" Mensam.Desk.nameDecoder)
                            (Decode.field "space" Mensam.Space.identifierDecoder)
                            (Decode.field "location" <| Decode.nullable Mensam.Desk.locationDecoder)
                    )
                    (Decode.field "reservation" <|
                        Decode.map6 (\id desk status timeBegin timeEnd user -> { id = id, desk = desk, status = status, timeBegin = timeBegin, timeEnd = timeEnd, user = user })
                            (Decode.field "id" Mensam.Reservation.identifierDecoder)
                            (Decode.field "desk" Mensam.Desk.identifierDecoder)
                            (Decode.field "status" Mensam.Reservation.statusDecoder)
                            (Decode.field "time-begin" Iso8601.decoder)
                            (Decode.field "time-end" Iso8601.decoder)
                            (Decode.field "user" Mensam.User.identifierDecoder)
                    )
                    (Decode.field "space" <|
                        Decode.map4 (\id name timezone owner -> { id = id, name = name, timezone = timezone, owner = owner })
                            (Decode.field "id" Mensam.Space.identifierDecoder)
                            (Decode.field "name" Mensam.Space.nameDecoder)
                            (Decode.field "timezone" Mensam.Time.timezoneDecoder)
                            (Decode.field "owner" Mensam.User.identifierDecoder)
                    )


decodeBody400 : Decode.Decoder String
decodeBody400 =
    Decode.field "error" Decode.string
