module Main exposing (..)

import Browser
import Browser.Navigation
import Color
import Debug
import Element
import Element.Background
import Element.Events
import Element.Font
import Html
import Html.Attributes
import Html.Events
import Jwt
import Login
import Platform.Cmd
import Platform.Sub
import Register
import Spaces
import Time
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
        , authenticated : Maybe Authentication
        , error : List String
        }


type alias Authentication =
    { jwt : Jwt.Jwt
    , expiration : Maybe Time.Posix
    }


type Screen
    = ScreenRegister Register.Model
    | ScreenLogin Login.Model
    | ScreenSpaces Spaces.Model
    | NoScreen


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Platform.Cmd.Cmd Message )
init _ _ _ =
    ( MkModel { screen = ScreenLogin Login.init, authenticated = Nothing, error = [] }, Platform.Cmd.none )


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
                Register.ReportError err ->
                    ( MkModel { model | error = err :: model.error }, Platform.Cmd.none )

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
                Login.ReportError err ->
                    ( MkModel { model | error = err :: model.error }, Platform.Cmd.none )

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
                    ( MkModel { model | authenticated = Just x }, Platform.Cmd.none )

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
                    case model.authenticated of
                        Just { jwt } ->
                            ( MkModel model
                            , Platform.Cmd.map MessageSpaces <| Spaces.deskListRequest jwt
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
            [ Element.Background.gradient { angle = 0, steps = [ Color.colors.dark.yellow, Color.colors.bright.yellow ] }
            , Element.Font.color Color.colors.dark.black
            , Element.Font.medium
            , Element.Font.size 20
            , Element.Font.family [ Element.Font.sansSerif ]
            ]
          <|
            Element.el
                [ Element.htmlAttribute <| Html.Attributes.style "min-width" "800px"
                , Element.htmlAttribute <| Html.Attributes.style "max-width" "1000px"
                , Element.htmlAttribute <| Html.Attributes.style "margin-left" "auto"
                , Element.htmlAttribute <| Html.Attributes.style "margin-right" "auto"
                , Element.htmlAttribute <| Html.Attributes.style "padding-left" "20px"
                , Element.htmlAttribute <| Html.Attributes.style "padding-right" "20px"
                , Element.htmlAttribute <| Html.Attributes.style "padding-top" "0px"
                , Element.htmlAttribute <| Html.Attributes.style "padding-bottom" "20px"
                , Element.width Element.fill
                , Element.height Element.fill
                , Element.Background.color Color.colors.dark.black
                , Element.Font.color Color.colors.bright.white
                ]
            <|
                Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.spacing 10
                    ]
                    [ elementNavigationBar <| MkModel model
                    , case model.screen of
                        ScreenLogin screenModel ->
                            Element.el
                                [ Element.width Element.fill
                                , Element.height Element.fill
                                ]
                            <|
                                Element.map MessageLogin <|
                                    Login.element screenModel

                        ScreenRegister screenModel ->
                            Element.html <|
                                Html.div []
                                    [ Html.map MessageRegister <| Register.view screenModel
                                    ]

                        ScreenSpaces screenModel ->
                            Element.html <|
                                Html.div []
                                    [ Html.map MessageSpaces <| Spaces.view screenModel
                                    ]

                        NoScreen ->
                            Element.none
                    , Element.paragraph []
                        [ Element.text <| "Authenticated: " ++ Debug.toString model.authenticated
                        ]
                    , Element.paragraph []
                        [ Element.text <| "Error: " ++ Debug.toString model.error
                        ]
                    , Element.html <| Html.button [ Html.Events.onClick ClearErrors ] [ Html.text "Clear" ]
                    ]
        ]
    }


elementNavigationBar : Model -> Element.Element Message
elementNavigationBar (MkModel model) =
    let
        tabDescriptions =
            [ { name = "Login"
              , message = SwitchScreenLogin
              , active =
                    case model.screen of
                        ScreenLogin _ ->
                            True

                        _ ->
                            False
              }
            , { name = "Register"
              , message = SwitchScreenRegister
              , active =
                    case model.screen of
                        ScreenRegister _ ->
                            True

                        _ ->
                            False
              }
            , { name = "Spaces"
              , message = SwitchScreenSpaces
              , active =
                    case model.screen of
                        ScreenSpaces _ ->
                            True

                        _ ->
                            False
              }
            ]

        viewTabDescriptions description =
            Element.el
                [ Element.Events.onClick description.message
                , Element.height Element.fill
                , Element.paddingXY 20 0
                , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                , Element.mouseOver [ Element.Background.color <| Element.rgba 1 1 1 0.3 ]
                , if description.active then
                    Element.Background.color <| Element.rgba 1 1 1 0.1

                  else
                    Element.Background.color <| Element.rgba 1 1 1 0
                ]
            <|
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                <|
                    Element.text description.name

        elementsTabDescription =
            List.map viewTabDescriptions tabDescriptions

        loginStatus =
            case model.authenticated of
                Nothing ->
                    Element.el
                        [ Element.Events.onClick SwitchScreenLogin
                        , Element.height Element.fill
                        , Element.paddingXY 20 0
                        , Element.alignRight
                        , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                        , Element.mouseOver [ Element.Background.color <| Element.rgba 1 1 1 0.3 ]
                        ]
                    <|
                        Element.el
                            [ Element.centerX
                            , Element.centerY
                            ]
                        <|
                            Element.text "Sign in"

                Just _ ->
                    Element.el
                        [ Element.height Element.fill
                        , Element.paddingXY 20 0
                        , Element.alignRight
                        , Element.htmlAttribute <| Html.Attributes.style "cursor" "default"
                        ]
                    <|
                        Element.el
                            [ Element.centerX
                            , Element.centerY
                            ]
                        <|
                            Element.text "Signed in"
    in
    Element.row
        [ Element.Font.size 20
        , Element.width Element.fill
        , Element.height <| Element.px 60
        , Element.Background.color Color.colors.bright.black
        ]
        (elementsTabDescription ++ [ loginStatus ])
