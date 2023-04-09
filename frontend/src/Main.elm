module Main exposing (..)

import Base64
import Browser
import Browser.Navigation
import Debug
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Platform.Cmd
import Platform.Sub
import Url


main : Program () Model Message
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Platform.Sub.none
        , onUrlRequest = \_ -> SubmitLogin
        , onUrlChange = \_ -> SubmitLogin
        }


type Model
    = MkModel { username : String, password : String, jwt : Maybe String }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Platform.Cmd.Cmd Message )
init _ _ _ =
    ( MkModel { username = "", password = "", jwt = Nothing }, Platform.Cmd.none )


type Message
    = EnterUsername String
    | EnterPassword String
    | SubmitLogin
    | SetSession { jwt : String }


update : Message -> Model -> ( Model, Platform.Cmd.Cmd Message )
update msg (MkModel model) =
    case msg of
        EnterUsername x ->
            ( MkModel { model | username = x }, Platform.Cmd.none )

        EnterPassword x ->
            ( MkModel { model | password = x }, Platform.Cmd.none )

        SubmitLogin ->
            ( MkModel model
            , Http.request
                { method = "POST"
                , headers = [ Http.header "Authorization" ("Basic " ++ Base64.encode (model.username ++ ":" ++ model.password)) ]
                , url = "api/login"
                , body = Http.emptyBody
                , expect = expectLoginResponse
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        SetSession x ->
            ( MkModel { model | jwt = Just x.jwt }, Platform.Cmd.none )


expectLoginResponse : Http.Expect Message
expectLoginResponse =
    Http.expectJson handleLoginResponse decodeLoginResponse


handleLoginResponse : Result Http.Error { jwt : String } -> Message
handleLoginResponse result =
    case result of
        Ok response ->
            SetSession { jwt = response.jwt }

        Err err ->
            SetSession { jwt = "" }


decodeLoginResponse : Json.Decode.Decoder { jwt : String }
decodeLoginResponse =
    Json.Decode.map (\x -> { jwt = x }) (Json.Decode.field "jwt" Json.Decode.string)


view : Model -> Browser.Document Message
view (MkModel model) =
    { title = "Mensam"
    , body =
        [ Html.form [ Html.Events.onSubmit SubmitLogin ]
            [ Html.fieldset []
                [ Html.input
                    [ Html.Events.onInput EnterUsername
                    , Html.Attributes.type_ "text"
                    , Html.Attributes.placeholder "Username"
                    ]
                    []
                ]
            , Html.fieldset []
                [ Html.input
                    [ Html.Events.onInput EnterPassword
                    , Html.Attributes.type_ "password"
                    , Html.Attributes.placeholder "Password"
                    ]
                    []
                ]
            , Html.button []
                [ Html.text "Login" ]
            ]
        , Html.text (Debug.toString model.jwt)
        ]
    }
