module Main exposing (..)

import Browser
import Browser.Navigation
import Debug
import Html
import Login
import Platform.Cmd
import Platform.Sub
import Register
import Url


main : Program () Model Message
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Platform.Sub.none
        , onUrlRequest = \_ -> EmptyMessage
        , onUrlChange = \_ -> EmptyMessage
        }


type Model
    = MkModel
        { modelRegister : Register.Model
        , modelLogin : Login.Model
        , jwt : Maybe String
        }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Platform.Cmd.Cmd Message )
init _ _ _ =
    ( MkModel { modelRegister = Register.init, modelLogin = Login.init, jwt = Nothing }, Platform.Cmd.none )


type Message
    = MessageRegister Register.Message
    | MessageLogin Login.Message
    | EmptyMessage


update : Message -> Model -> ( Model, Platform.Cmd.Cmd Message )
update message (MkModel model) =
    case message of
        MessageRegister (Register.MessagePure m) ->
            ( MkModel { model | modelRegister = Register.updatePure m model.modelRegister }, Platform.Cmd.none )

        MessageRegister (Register.MessageEffect m) ->
            case m of
                Register.Submit ->
                    ( MkModel model
                    , Platform.Cmd.map MessageRegister <| Register.registerRequest model.modelRegister
                    )

                Register.Submitted ->
                    ( MkModel model
                    , Platform.Cmd.none
                    )

        MessageLogin (Login.MessagePure m) ->
            ( MkModel { model | modelLogin = Login.updatePure m model.modelLogin }, Platform.Cmd.none )

        MessageLogin (Login.MessageEffect m) ->
            case m of
                Login.SubmitLogin ->
                    ( MkModel model
                    , Platform.Cmd.map MessageLogin <| Login.loginRequest model.modelLogin
                    )

                Login.SetSession x ->
                    ( MkModel { model | jwt = Just x.jwt }, Platform.Cmd.none )

        EmptyMessage ->
            ( MkModel model, Platform.Cmd.none )


view : Model -> Browser.Document Message
view (MkModel model) =
    { title = "Mensam"
    , body =
        [ Html.map MessageLogin <| Login.view model.modelLogin
        , Html.map MessageRegister <| Register.view model.modelRegister
        , Html.text (Debug.toString model.jwt)
        ]
    }
