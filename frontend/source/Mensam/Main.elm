module Mensam.Main exposing (..)

import Browser
import Browser.Navigation
import Debug
import Element
import Element.Background
import Element.Events
import Element.Font
import Html
import Html.Attributes
import Html.Events
import Mensam.Color
import Mensam.Jwt
import Mensam.Login
import Mensam.Register
import Mensam.Spaces
import Platform.Cmd
import Platform.Sub
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
    { jwt : Mensam.Jwt.Jwt
    , expiration : Maybe Time.Posix
    }


type Screen
    = ScreenRegister Mensam.Register.Model
    | ScreenLogin Mensam.Login.Model
    | ScreenSpaces Mensam.Spaces.Model
    | NoScreen


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Platform.Cmd.Cmd Message )
init _ _ _ =
    ( MkModel { screen = ScreenLogin Mensam.Login.init, authenticated = Nothing, error = [] }, Platform.Cmd.none )


type Message
    = EmptyMessage
    | ReportError String
    | ClearErrors
    | MessageRegister Mensam.Register.Message
    | SwitchScreenRegister
    | MessageLogin Mensam.Login.Message
    | SwitchScreenLogin
    | MessageSpaces Mensam.Spaces.Message
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

        MessageRegister (Mensam.Register.MessagePure m) ->
            case model.screen of
                ScreenRegister screenModel ->
                    ( MkModel { model | screen = ScreenRegister <| Mensam.Register.updatePure m screenModel }
                    , Platform.Cmd.none
                    )

                _ ->
                    ( MkModel { model | error = "Can't process a message for the wrong screen." :: model.error }
                    , Platform.Cmd.none
                    )

        MessageRegister (Mensam.Register.MessageEffect m) ->
            case m of
                Mensam.Register.ReportError err ->
                    ( MkModel { model | error = err :: model.error }, Platform.Cmd.none )

                Mensam.Register.Submit ->
                    case model.screen of
                        ScreenRegister screenModel ->
                            ( MkModel model
                            , Platform.Cmd.map MessageRegister <| Mensam.Register.registerRequest screenModel
                            )

                        _ ->
                            ( MkModel { model | error = "Can't process a message for the wrong screen." :: model.error }
                            , Platform.Cmd.none
                            )

                Mensam.Register.Submitted ->
                    ( MkModel model
                    , Platform.Cmd.none
                    )

        SwitchScreenRegister ->
            ( MkModel { model | screen = ScreenRegister Mensam.Register.init }
            , Platform.Cmd.none
            )

        MessageLogin (Mensam.Login.MessagePure m) ->
            case model.screen of
                ScreenLogin screenModel ->
                    ( MkModel { model | screen = ScreenLogin <| Mensam.Login.updatePure m screenModel }
                    , Platform.Cmd.none
                    )

                _ ->
                    ( MkModel { model | error = "Can't process a message for the wrong screen." :: model.error }
                    , Platform.Cmd.none
                    )

        MessageLogin (Mensam.Login.MessageEffect m) ->
            case m of
                Mensam.Login.ReportError err ->
                    ( MkModel { model | error = err :: model.error }, Platform.Cmd.none )

                Mensam.Login.SubmitLogin ->
                    case model.screen of
                        ScreenLogin screenModel ->
                            ( MkModel model
                            , Platform.Cmd.map MessageLogin <| Mensam.Login.loginRequest screenModel
                            )

                        _ ->
                            ( MkModel { model | error = "Can't process a message for the wrong screen." :: model.error }
                            , Platform.Cmd.none
                            )

                Mensam.Login.Register ->
                    ( MkModel { model | screen = ScreenRegister Mensam.Register.init }, Platform.Cmd.none )

                Mensam.Login.SetSession x ->
                    ( MkModel { model | authenticated = Just x }, Platform.Cmd.none )

        SwitchScreenLogin ->
            ( MkModel { model | screen = ScreenLogin Mensam.Login.init }
            , Platform.Cmd.none
            )

        MessageSpaces (Mensam.Spaces.MessagePure m) ->
            case model.screen of
                ScreenSpaces screenModel ->
                    ( MkModel { model | screen = ScreenSpaces <| Mensam.Spaces.updatePure m screenModel }
                    , Platform.Cmd.none
                    )

                _ ->
                    ( MkModel { model | error = "Can't process a message for the wrong screen." :: model.error }
                    , Platform.Cmd.none
                    )

        MessageSpaces (Mensam.Spaces.MessageEffect m) ->
            case m of
                Mensam.Spaces.ReportError err ->
                    ( MkModel { model | error = err :: model.error }, Platform.Cmd.none )

                Mensam.Spaces.RefreshSpaces ->
                    case model.authenticated of
                        Just { jwt } ->
                            ( MkModel model
                            , Platform.Cmd.map MessageSpaces <| Mensam.Spaces.deskListRequest jwt
                            )

                        Nothing ->
                            ( MkModel { model | error = "Can't make request without JWT." :: model.error }
                            , Platform.Cmd.none
                            )

        SwitchScreenSpaces ->
            ( MkModel { model | screen = ScreenSpaces Mensam.Spaces.init }
            , Platform.Cmd.none
            )


view : Model -> Browser.Document Message
view (MkModel model) =
    { title = "Mensam"
    , body =
        [ Element.layout
            [ Element.Background.gradient { angle = 0, steps = [ Mensam.Color.colors.dark.yellow, Mensam.Color.colors.bright.yellow ] }
            , Element.Font.color Mensam.Color.colors.dark.black
            , Element.Font.regular
            , Element.Font.size 20
            , Element.Font.family
                [ Element.Font.external
                    { url = "fonts.css"
                    , name = "Fira Sans"
                    }
                , Element.Font.external
                    { url = "fonts.css"
                    , name = "Fira Sans Condensed"
                    }
                , Element.Font.external
                    { url = "fonts.css"
                    , name = "Fira Sans Mono"
                    }
                ]
            ]
          <|
            Element.el
                [ Element.htmlAttribute <| Html.Attributes.style "min-width" "800px"
                , Element.htmlAttribute <| Html.Attributes.style "max-width" "1000px"
                , Element.htmlAttribute <| Html.Attributes.style "margin-left" "auto"
                , Element.htmlAttribute <| Html.Attributes.style "margin-right" "auto"
                , Element.width Element.fill
                , Element.height Element.fill
                , Element.Background.color Mensam.Color.colors.dark.black
                , Element.Font.color Mensam.Color.colors.bright.white
                ]
            <|
                Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.spacing 10
                    ]
                    [ elementNavigationBar <| MkModel model
                    , Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.padding 20
                        ]
                        [ case model.screen of
                            ScreenLogin screenModel ->
                                Element.el
                                    [ Element.width Element.fill
                                    , Element.height Element.fill
                                    ]
                                <|
                                    Element.map MessageLogin <|
                                        Mensam.Login.element screenModel

                            ScreenRegister screenModel ->
                                Element.html <|
                                    Html.div []
                                        [ Html.map MessageRegister <| Mensam.Register.view screenModel
                                        ]

                            ScreenSpaces screenModel ->
                                Element.html <|
                                    Html.div []
                                        [ Html.map MessageSpaces <| Mensam.Spaces.view screenModel
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

        title =
            Element.el
                [ Element.height Element.fill
                , Element.paddingXY 20 0
                , Element.alignLeft
                , Element.htmlAttribute <| Html.Attributes.style "cursor" "default"
                , Element.Font.family [ Element.Font.typeface "Fira Sans" ]
                , Element.htmlAttribute <| Html.Attributes.style "text-transform" "none"
                , Element.Font.color Mensam.Color.colors.bright.yellow
                , Element.Font.italic
                , Element.Font.extraLight
                , Element.Font.size 25
                ]
            <|
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                <|
                    Element.text "Mensam"
    in
    Element.row
        [ Element.width Element.fill
        , Element.height <| Element.px 60
        , Element.Background.color Mensam.Color.colors.bright.black
        , Element.Font.family [ Element.Font.typeface "Fira Sans Condensed" ]
        , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
        , Element.Font.light
        , Element.Font.size 17
        ]
        ([ title ] ++ elementsTabDescription ++ [ loginStatus ])
