module Register exposing (..)

import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Json.Encode


type alias Model =
    { username : String
    , password : String
    , email : String
    , emailVisible : Bool
    }


init : Model
init =
    { username = "", password = "", email = "", emailVisible = True }


view : Model -> Html.Html Message
view model =
    Html.form
        [ Html.Attributes.id "form-register"
        , Html.Events.onSubmit <| MessageEffect Submit
        ]
        [ Html.fieldset
            [ Html.Attributes.form "form-register"
            ]
            [ Html.input
                [ Html.Attributes.id "input-form-username"
                , Html.Attributes.form "form-register"
                , Html.Events.onInput <| MessagePure << EnterUsername
                , Html.Attributes.type_ "text"
                , Html.Attributes.placeholder "Username"
                , Html.Attributes.value model.username
                ]
                []
            , Html.input
                [ Html.Attributes.id "input-form-password"
                , Html.Attributes.form "form-register"
                , Html.Events.onInput <| MessagePure << EnterPassword
                , Html.Attributes.type_ "password"
                , Html.Attributes.placeholder "Password"
                , Html.Attributes.value model.password
                ]
                []
            , Html.input
                [ Html.Attributes.id "input-form-email"
                , Html.Attributes.form "form-register"
                , Html.Events.onInput <| MessagePure << EnterEmail
                , Html.Attributes.type_ "email"
                , Html.Attributes.placeholder "Email Address"
                , Html.Attributes.value model.email
                ]
                []
            , Html.input
                [ Html.Attributes.id "input-form-email-visible"
                , Html.Attributes.form "form-register"
                , Html.Events.onCheck <| MessagePure << ToggleEmailVisible
                , Html.Attributes.type_ "checkbox"
                , Html.Attributes.checked model.emailVisible
                ]
                []
            , Html.button
                [ Html.Attributes.id "button-register-submit"
                , Html.Attributes.form "form-register"
                , Html.Attributes.type_ "submit"
                ]
                [ Html.text "Register" ]
            ]
        ]


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect


type MessagePure
    = EnterUsername String
    | EnterPassword String
    | EnterEmail String
    | ToggleEmailVisible Bool


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        EnterUsername username ->
            { model | username = username }

        EnterPassword password ->
            { model | password = password }

        EnterEmail email ->
            { model | email = email }

        ToggleEmailVisible emailVisible ->
            { model | emailVisible = emailVisible }


type MessageEffect
    = Submit
    | Submitted


type alias RegisterRequestBody =
    { username : String, password : String, email : String, emailVisible : Bool }


type alias RegisterResponseBody =
    ()


registerRequest : RegisterRequestBody -> Cmd Message
registerRequest body =
    Http.request
        { method = "POST"
        , headers = []
        , url = "api/register"
        , body = serializeRegisterRequest body
        , expect = expectRegisterResponse
        , timeout = Nothing
        , tracker = Nothing
        }


serializeRegisterRequest : RegisterRequestBody -> Http.Body
serializeRegisterRequest body =
    Http.jsonBody <|
        Json.Encode.object
            [ ( "name", Json.Encode.string body.username )
            , ( "password", Json.Encode.string body.password )
            , ( "email", Json.Encode.string body.email )
            , ( "email-visible", Json.Encode.bool body.emailVisible )
            ]


expectRegisterResponse : Http.Expect Message
expectRegisterResponse =
    Http.expectJson handleRegisterResponse decodeRegisterResponse


handleRegisterResponse : Result Http.Error RegisterResponseBody -> Message
handleRegisterResponse result =
    case result of
        Ok response ->
            MessageEffect Submitted

        Err err ->
            MessageEffect Submitted


decodeRegisterResponse : Json.Decode.Decoder RegisterResponseBody
decodeRegisterResponse =
    Json.Decode.list (Json.Decode.succeed ())
        |> Json.Decode.andThen
            (\x ->
                case x of
                    [] ->
                        Json.Decode.succeed ()

                    _ ->
                        Json.Decode.fail "Trying to decode empty list, but list has elements."
            )
