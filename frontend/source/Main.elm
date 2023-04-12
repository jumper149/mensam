module Main exposing (..)

import Browser
import Browser.Navigation
import Debug
import Element
import Element.Background
import Element.Font
import Html
import Html.Events
import Login
import Platform.Cmd
import Platform.Sub
import Register
import Spaces
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
        { screen : Screen
        , jwt : Maybe String
        , error : List String
        }


type Screen
    = ScreenRegister Register.Model
    | ScreenLogin Login.Model
    | ScreenSpaces Spaces.Model
    | NoScreen


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Platform.Cmd.Cmd Message )
init _ _ _ =
    ( MkModel { screen = ScreenLogin Login.init, jwt = Nothing, error = [] }, Platform.Cmd.none )


type Message
    = EmptyMessage
    | ReportError String
    | ClearErrors
    | MessageRegister Register.Message
    | SwitchScreenRegister
    | MessageLogin Login.Message
    | SwitchScreenLogin
    | MessageSpaces Spaces.Message
    | SwitchScreenSpaces


update : Message -> Model -> ( Model, Platform.Cmd.Cmd Message )
update message (MkModel model) =
    case message of
        EmptyMessage ->
            ( MkModel model, Platform.Cmd.none )

        ReportError err ->
            ( MkModel { model | error = err :: model.error }, Platform.Cmd.none )

        ClearErrors ->
            ( MkModel { model | error = [] }, Platform.Cmd.none )

        MessageRegister (Register.MessagePure m) ->
            case model.screen of
                ScreenRegister screenModel ->
                    ( MkModel { model | screen = ScreenRegister <| Register.updatePure m screenModel }
                    , Platform.Cmd.none
                    )

                _ ->
                    ( MkModel { model | error = "Can't process a message for the wrong screen." :: model.error }
                    , Platform.Cmd.none
                    )

        MessageRegister (Register.MessageEffect m) ->
            case m of
                Register.Submit ->
                    case model.screen of
                        ScreenRegister screenModel ->
                            ( MkModel model
                            , Platform.Cmd.map MessageRegister <| Register.registerRequest screenModel
                            )

                        _ ->
                            ( MkModel { model | error = "Can't process a message for the wrong screen." :: model.error }
                            , Platform.Cmd.none
                            )

                Register.Submitted ->
                    ( MkModel model
                    , Platform.Cmd.none
                    )

        SwitchScreenRegister ->
            ( MkModel { model | screen = ScreenRegister Register.init }
            , Platform.Cmd.none
            )

        MessageLogin (Login.MessagePure m) ->
            case model.screen of
                ScreenLogin screenModel ->
                    ( MkModel { model | screen = ScreenLogin <| Login.updatePure m screenModel }
                    , Platform.Cmd.none
                    )

                _ ->
                    ( MkModel { model | error = "Can't process a message for the wrong screen." :: model.error }
                    , Platform.Cmd.none
                    )

        MessageLogin (Login.MessageEffect m) ->
            case m of
                Login.SubmitLogin ->
                    case model.screen of
                        ScreenLogin screenModel ->
                            ( MkModel model
                            , Platform.Cmd.map MessageLogin <| Login.loginRequest screenModel
                            )

                        _ ->
                            ( MkModel { model | error = "Can't process a message for the wrong screen." :: model.error }
                            , Platform.Cmd.none
                            )

                Login.Register ->
                    ( MkModel { model | screen = ScreenRegister Register.init }, Platform.Cmd.none )

                Login.SetSession x ->
                    ( MkModel { model | jwt = Just x.jwt }, Platform.Cmd.none )

        SwitchScreenLogin ->
            ( MkModel { model | screen = ScreenLogin Login.init }
            , Platform.Cmd.none
            )

        MessageSpaces (Spaces.MessagePure m) ->
            case model.screen of
                ScreenSpaces screenModel ->
                    ( MkModel { model | screen = ScreenSpaces <| Spaces.updatePure m screenModel }
                    , Platform.Cmd.none
                    )

                _ ->
                    ( MkModel { model | error = "Can't process a message for the wrong screen." :: model.error }
                    , Platform.Cmd.none
                    )

        MessageSpaces (Spaces.MessageEffect m) ->
            case m of
                Spaces.ReportError err ->
                    ( MkModel { model | error = err :: model.error }, Platform.Cmd.none )

                Spaces.RefreshSpaces ->
                    case model.jwt of
                        Just jwt ->
                            ( MkModel model
                            , Platform.Cmd.map MessageSpaces <| Spaces.deskListRequest { jwt = jwt }
                            )

                        Nothing ->
                            ( MkModel { model | error = "Can't make request without JWT." :: model.error }
                            , Platform.Cmd.none
                            )

        SwitchScreenSpaces ->
            ( MkModel { model | screen = ScreenSpaces Spaces.init }
            , Platform.Cmd.none
            )


view : Model -> Browser.Document Message
view (MkModel model) =
    { title = "Mensam"
    , body =
        [ Element.layout
            [ Element.Background.color (Element.rgba 0 0 0 1)
            , Element.Font.color (Element.rgba 1 1 1 1)
            , Element.Font.medium
            , Element.Font.size 20
            , Element.Font.family [ Element.Font.sansSerif ]
            ]
          <|
            Element.html <|
                Html.div []
                    [ Html.h1 [] [ Html.text "Mensam" ]
                    , Html.button [ Html.Events.onClick SwitchScreenLogin ] [ Html.text "Login" ]
                    , Html.button [ Html.Events.onClick SwitchScreenRegister ] [ Html.text "Register" ]
                    , Html.button [ Html.Events.onClick SwitchScreenSpaces ] [ Html.text "Spaces" ]
                    , Html.h3 [] [ Html.text "Screen" ]
                    , case model.screen of
                        ScreenLogin screenModel ->
                            Html.map MessageLogin <| Login.view screenModel

                        ScreenRegister screenModel ->
                            Html.map MessageRegister <| Register.view screenModel

                        ScreenSpaces screenModel ->
                            Html.map MessageSpaces <| Spaces.view screenModel

                        NoScreen ->
                            Html.div [] []
                    , Html.h3 [] [ Html.text "JWT" ]
                    , Html.text (Debug.toString model.jwt)
                    , Html.h3 [] [ Html.text "Error" ]
                    , Html.button [ Html.Events.onClick ClearErrors ] [ Html.text "Clear" ]
                    , Html.text (Debug.toString model.error)
                    ]
        ]
    }
