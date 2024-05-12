module Mensam.Api.Profile exposing (..)

import Http
import Http.Extra
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Auth.Bearer
import Mensam.User
import Url.Builder


type alias Request =
    { jwt : Mensam.Auth.Bearer.Jwt
    , id : Mensam.User.Identifier
    }


type Response
    = Success
        { email : Maybe Mensam.User.Email
        , emailVerified : Bool
        , id : Mensam.User.Identifier
        , name : Mensam.User.Name
        }
    | ErrorUnknownUser
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
                , "profile"
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

                404 ->
                    case Decode.decodeString decodeBody404 body of
                        Ok () ->
                            Ok <| ErrorUnknownUser

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
        [ ( "user"
          , Encode.object
                [ ( "tag"
                  , Encode.string "identifier"
                  )
                , ( "value"
                  , Mensam.User.identifierEncode body.id
                  )
                ]
          )
        ]


decodeBody200 : Decode.Decoder { email : Maybe Mensam.User.Email, emailVerified : Bool, id : Mensam.User.Identifier, name : Mensam.User.Name }
decodeBody200 =
    Decode.map4
        (\email emailVerified id name -> { email = email, emailVerified = emailVerified, id = id, name = name })
        (Decode.field "email" <| Decode.maybe Mensam.User.emailDecoder)
        (Decode.field "email-verified" <| Decode.bool)
        (Decode.field "id" Mensam.User.identifierDecoder)
        (Decode.field "name" Mensam.User.nameDecoder)


decodeBody400 : Decode.Decoder String
decodeBody400 =
    Decode.field "error" Decode.string


decodeBody404 : Decode.Decoder ()
decodeBody404 =
    Decode.map (\_ -> ()) <|
        Decode.list <|
            Decode.succeed ()
