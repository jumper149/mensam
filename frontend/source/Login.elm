module Login exposing (..)

import Base64
import Color
import Element
import Element.Background
import Element.Font
import Element.Input
import Html
import Html.Attributes
import Html.Events
import Http
import Iso8601
import Json.Decode
import Jwt
import Time


type alias Model =
    { username : String, password : String }


init : Model
init =
    { username = "", password = "" }


element : Model -> Element.Element Message
element model =
    Element.el
        [ Element.Background.color (Element.rgba 1 1 1 0.2)
        , Element.Font.color Color.colors.dark.black
        , Element.Font.size 16
        , Element.centerX
        , Element.centerY
        ]
    <|
        Element.column
            [ Element.padding 20
            , Element.spacing 20
            ]
            [ Element.el
                [ Element.Font.size 30
                , Element.Font.hairline
                ]
              <|
                Element.text "Sign in"
            , Element.Input.username
                []
                { onChange = MessagePure << EnterUsername
                , text = model.username
                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Username"
                , label = Element.Input.labelAbove [] <| Element.text "Username"
                }
            , Element.Input.currentPassword
                []
                { onChange = MessagePure << EnterPassword
                , text = model.password
                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Password"
                , label = Element.Input.labelAbove [] <| Element.text "Password"
                , show = False
                }
            , Element.Input.button
                []
                { onPress = Just <| MessageEffect <| SubmitLogin
                , label = Element.text "Sign in"
                }
            ]


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect


type MessagePure
    = EnterUsername String
    | EnterPassword String


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        EnterUsername username ->
            { model | username = username }

        EnterPassword password ->
            { model | password = password }


type MessageEffect
    = ReportError String
    | SubmitLogin
    | Register
    | SetSession { jwt : Jwt.Jwt, expiration : Maybe Time.Posix }


loginRequest : { username : String, password : String } -> Cmd Message
loginRequest body =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Basic " ++ Base64.encode (body.username ++ ":" ++ body.password)) ]
        , url = "api/login"
        , body = Http.emptyBody
        , expect = expectLoginResponse
        , timeout = Nothing
        , tracker = Nothing
        }


expectLoginResponse : Http.Expect Message
expectLoginResponse =
    Http.expectJson handleLoginResponse decodeLoginResponse


handleLoginResponse : Result Http.Error { jwt : Jwt.Jwt, expiration : Maybe Time.Posix } -> Message
handleLoginResponse result =
    case result of
        Ok response ->
            MessageEffect <| SetSession { jwt = response.jwt, expiration = response.expiration }

        Err err ->
            MessageEffect <| ReportError <| Debug.toString err


decodeLoginResponse : Json.Decode.Decoder { jwt : Jwt.Jwt, expiration : Maybe Time.Posix }
decodeLoginResponse =
    Json.Decode.map2 (\jwt expiration -> { jwt = jwt, expiration = expiration })
        (Json.Decode.field "jwt" Jwt.decode)
        (Json.Decode.maybe <| Json.Decode.field "expiration" Iso8601.decoder)
