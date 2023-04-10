module Login exposing (..)

import Base64
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode


type alias Model =
    { username : String, password : String }


init : Model
init =
    { username = "", password = "" }


view : Model -> Html.Html Message
view model =
    Html.form
        [ Html.Attributes.id "form-login"
        , Html.Events.onSubmit <| MessageEffect SubmitLogin
        ]
        [ Html.fieldset
            [ Html.Attributes.form "form-login"
            ]
            [ Html.input
                [ Html.Attributes.id "input-login-username"
                , Html.Attributes.form "form-login"
                , Html.Events.onInput <| MessagePure << EnterUsername
                , Html.Attributes.type_ "text"
                , Html.Attributes.placeholder "Username"
                , Html.Attributes.value model.username
                ]
                []
            , Html.input
                [ Html.Attributes.id "input-login-password"
                , Html.Attributes.form "form-login"
                , Html.Events.onInput <| MessagePure << EnterPassword
                , Html.Attributes.type_ "password"
                , Html.Attributes.placeholder "Password"
                , Html.Attributes.value model.password
                ]
                []
            ]
        , Html.fieldset
            [ Html.Attributes.form "form-login"
            ]
            [ Html.button
                [ Html.Attributes.id "button-login-register"
                , Html.Events.onClick <| MessageEffect Register
                , Html.Attributes.type_ "button"
                ]
                [ Html.text "Register" ]
            , Html.button
                [ Html.Attributes.id "button-login-submit"
                , Html.Attributes.form "form-login"
                , Html.Attributes.type_ "submit"
                ]
                [ Html.text "Login" ]
            ]
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
    = SubmitLogin
    | Register
    | SetSession { jwt : String }


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


handleLoginResponse : Result Http.Error { jwt : String } -> Message
handleLoginResponse result =
    case result of
        Ok response ->
            MessageEffect <| SetSession { jwt = response.jwt }

        Err err ->
            MessageEffect <| SetSession { jwt = "" }


decodeLoginResponse : Json.Decode.Decoder { jwt : String }
decodeLoginResponse =
    Json.Decode.map (\x -> { jwt = x }) (Json.Decode.field "jwt" Json.Decode.string)
