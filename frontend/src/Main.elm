module Main exposing (..)

import Browser
import Browser.Navigation
import Debug
import Html
import Login
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
        , onUrlRequest = \_ -> EmptyMessage
        , onUrlChange = \_ -> EmptyMessage
        }


type Model
    = MkModel { modelLogin : Login.Model, jwt : Maybe String }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Platform.Cmd.Cmd Message )
init _ _ _ =
    ( MkModel { modelLogin = Login.init, jwt = Nothing }, Platform.Cmd.none )


type Message
    = MessageLogin Login.Message
    | EmptyMessage


update : Message -> Model -> ( Model, Platform.Cmd.Cmd Message )
update message (MkModel model) =
    case message of
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
        , Html.text (Debug.toString model.jwt)
        ]
    }
