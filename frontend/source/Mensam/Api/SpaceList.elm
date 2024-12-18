module Mensam.Api.SpaceList exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Auth.Bearer
import Mensam.Http.Tracker
import Mensam.Space
import Mensam.Time
import Mensam.Url
import Mensam.User


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , order : List { category : String, order : String }
    , member : Maybe Bool
    }


type Response
    = Success
        { spaces :
            List
                { id : Mensam.Space.Identifier
                , name : Mensam.Space.Name
                , timezone : Mensam.Time.Timezone
                , owner : Mensam.User.Identifier
                , users : Int
                , desks : Int
                }
        }
    | ErrorBody String
    | ErrorAuth Mensam.Auth.Bearer.Error


request : Maybe Mensam.Http.Tracker.Tracker -> Mensam.Url.BaseUrl -> Request -> (Result Http.Error Response -> a) -> Cmd a
request tracker baseUrl body handleResult =
    Http.request
        { method = "POST"
        , headers =
            [ Mensam.Auth.Bearer.authorizationHeader body.jwt
            ]
        , url =
            Mensam.Url.absolute baseUrl
                [ "api"
                , "space"
                , "list"
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
        , ( "member"
          , case body.member of
                Nothing ->
                    Encode.null

                Just bool ->
                    Encode.bool bool
          )
        ]


decodeBody200 :
    Decode.Decoder
        { spaces :
            List
                { id : Mensam.Space.Identifier
                , name : Mensam.Space.Name
                , timezone : Mensam.Time.Timezone
                , owner : Mensam.User.Identifier
                , users : Int
                , desks : Int
                }
        }
decodeBody200 =
    Decode.map (\x -> { spaces = x }) <|
        Decode.field "spaces" <|
            Decode.list <|
                Decode.map6
                    (\id name timezone owner users desks ->
                        { id = id
                        , name = name
                        , timezone = timezone
                        , owner = owner
                        , users = users
                        , desks = desks
                        }
                    )
                    (Decode.field "id" Mensam.Space.identifierDecoder)
                    (Decode.field "name" Mensam.Space.nameDecoder)
                    (Decode.field "timezone" Mensam.Time.timezoneDecoder)
                    (Decode.field "owner" Mensam.User.identifierDecoder)
                    (Decode.field "users" Decode.int)
                    (Decode.field "desks" Decode.int)


decodeBody400 : Decode.Decoder String
decodeBody400 =
    Decode.field "error" Decode.string
