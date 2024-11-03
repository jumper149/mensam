module Mensam.Api.SpaceEdit exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Auth.Bearer
import Mensam.Http.Tracker
import Mensam.Space
import Mensam.Space.Role
import Mensam.Time
import Mensam.Url


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , id : Mensam.Space.Identifier
    , name : Maybe Mensam.Space.Name
    , timezone : Maybe Mensam.Time.Timezone
    , visibility : Maybe Mensam.Space.Visibility
    }


type Response
    = Success
        { id : Mensam.Space.Identifier
        , name : Mensam.Space.Name
        , timezone : Mensam.Time.Timezone
        , visibility : Mensam.Space.Visibility
        }
    | ErrorInsufficientPermission Mensam.Space.Role.Permission
    | ErrorSpaceNotFound
    | ErrorBody String
    | ErrorAuth Mensam.Auth.Bearer.Error


request : Maybe Mensam.Http.Tracker.Tracker -> Mensam.Url.BaseUrl -> Request -> (Result Http.Error Response -> a) -> Cmd a
request tracker baseUrl body handleResult =
    Http.request
        { method = "PATCH"
        , headers =
            [ Mensam.Auth.Bearer.authorizationHeader body.jwt
            ]
        , url =
            Mensam.Url.absolute baseUrl
                [ "api"
                , "space"
                , "edit"
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
        , ( "name"
          , case body.name of
                Nothing ->
                    Encode.object
                        [ ( "update", Encode.bool False )
                        ]

                Just name ->
                    Encode.object
                        [ ( "update", Encode.bool True )
                        , ( "value", Mensam.Space.nameEncode name )
                        ]
          )
        , ( "timezone"
          , case body.timezone of
                Nothing ->
                    Encode.object
                        [ ( "update", Encode.bool False )
                        ]

                Just timezone ->
                    Encode.object
                        [ ( "update", Encode.bool True )
                        , ( "value", Mensam.Time.timezoneEncode timezone )
                        ]
          )
        , ( "visibility"
          , case body.visibility of
                Nothing ->
                    Encode.object
                        [ ( "update", Encode.bool False )
                        ]

                Just visibility ->
                    Encode.object
                        [ ( "update", Encode.bool True )
                        , ( "value", Mensam.Space.visibilityEncode visibility )
                        ]
          )
        ]


decodeBody200 :
    Decode.Decoder
        { id : Mensam.Space.Identifier
        , name : Mensam.Space.Name
        , timezone : Mensam.Time.Timezone
        , visibility : Mensam.Space.Visibility
        }
decodeBody200 =
    Decode.map4
        (\id name timezone visibility ->
            { id = id
            , name = name
            , timezone = timezone
            , visibility = visibility
            }
        )
        (Decode.field "id" Mensam.Space.identifierDecoder)
        (Decode.field "name" Mensam.Space.nameDecoder)
        (Decode.field "timezone" Mensam.Time.timezoneDecoder)
        (Decode.field "visibility" Mensam.Space.visibilityDecoder)


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
