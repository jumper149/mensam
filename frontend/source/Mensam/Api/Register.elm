module Mensam.Api.Register exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Tracker
import Mensam.Url
import Mensam.User


type alias Request =
    { email : Mensam.User.Email
    , emailNotifications : Bool
    , emailVisible : Bool
    , name : Mensam.User.Name
    , password : Mensam.User.Password
    }


type Response
    = Success
        { emailSent : Bool
        }
    | ErrorUsernameIsTaken
    | ErrorBody String


request : Maybe Mensam.Tracker.Tracker -> Mensam.Url.BaseUrl -> Request -> (Result Http.Error Response -> a) -> Cmd a
request tracker baseUrl body handleResult =
    Http.request
        { method = "POST"
        , headers = []
        , url =
            Mensam.Url.absolute baseUrl
                [ "api"
                , "register"
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

                409 ->
                    case Decode.decodeString decodeBody409 body of
                        Ok () ->
                            Ok ErrorUsernameIsTaken

                        Err err ->
                            Err <| Http.BadBody <| Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status

        Http.GoodStatus_ metadata body ->
            case metadata.statusCode of
                201 ->
                    case Decode.decodeString decodeBody201 body of
                        Ok value ->
                            Ok <| Success value

                        Err err ->
                            Err <| Http.BadBody <| Decode.errorToString err

                status ->
                    Err <| Http.BadStatus status


encodeBody : Request -> Encode.Value
encodeBody body =
    Encode.object
        [ ( "email", Mensam.User.emailEncode body.email )
        , ( "email-notifications", Encode.bool body.emailNotifications )
        , ( "email-visible", Encode.bool body.emailVisible )
        , ( "name", Mensam.User.nameEncode body.name )
        , ( "password", Mensam.User.passwordEncode body.password )
        ]


decodeBody201 : Decode.Decoder { emailSent : Bool }
decodeBody201 =
    Decode.map (\emailSent -> { emailSent = emailSent })
        (Decode.field "email-sent" Decode.bool)


decodeBody400 : Decode.Decoder String
decodeBody400 =
    Decode.field "error" Decode.string


decodeBody409 : Decode.Decoder ()
decodeBody409 =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "Username is taken." ->
                        Decode.succeed ()

                    _ ->
                        Decode.fail <| "Unexpected HTTP 409 message: " ++ string
            )
