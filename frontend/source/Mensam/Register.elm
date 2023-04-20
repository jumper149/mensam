module Mensam.Register exposing (..)

import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Json.Encode
import Mensam.Api.Register


type alias Model =
    { username : String
    , password : String
    , email : String
    , emailVisible : Bool
    }


init : Model
init =
    { username = ""
    , password = ""
    , email = ""
    , emailVisible = True
    }


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
    = ReportError String
    | Submit
    | Submitted


register : Model -> Cmd Message
register model =
    Mensam.Api.Register.request { email = model.email, emailVisible = model.emailVisible, name = model.username, password = model.password } <|
        \result ->
            case result of
                Ok Mensam.Api.Register.Success ->
                    MessageEffect <| Submitted

                Ok (Mensam.Api.Register.ErrorBody err) ->
                    MessageEffect <| ReportError <| Debug.toString err

                Err err ->
                    MessageEffect <| ReportError <| Debug.toString err
