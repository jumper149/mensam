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
import Mensam.Font
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
            update EmptyMessage <| MkModel { model | error = err :: model.error }

        ClearErrors ->
            update EmptyMessage <| MkModel { model | error = [] }

        MessageRegister (Mensam.Register.MessagePure m) ->
            case model.screen of
                ScreenRegister screenModel ->
                    update EmptyMessage <|
                        MkModel { model | screen = ScreenRegister <| Mensam.Register.updatePure m screenModel }

                _ ->
                    update (ReportError "Can't process a message for the wrong screen.") <| MkModel model

        MessageRegister (Mensam.Register.MessageEffect m) ->
            case m of
                Mensam.Register.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Register.Submit ->
                    case model.screen of
                        ScreenRegister screenModel ->
                            ( MkModel model
                            , Platform.Cmd.map MessageRegister <| Mensam.Register.registerRequest screenModel
                            )

                        _ ->
                            update (ReportError "Can't process a message for the wrong screen.") <| MkModel model

                Mensam.Register.Submitted ->
                    update EmptyMessage <| MkModel model

        SwitchScreenRegister ->
            update EmptyMessage <| MkModel { model | screen = ScreenRegister Mensam.Register.init }

        MessageLogin (Mensam.Login.MessagePure m) ->
            case model.screen of
                ScreenLogin screenModel ->
                    update EmptyMessage <| MkModel { model | screen = ScreenLogin <| Mensam.Login.updatePure m screenModel }

                _ ->
                    update (ReportError "Can't process a message for the wrong screen.") <| MkModel model

        MessageLogin (Mensam.Login.MessageEffect m) ->
            case m of
                Mensam.Login.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Login.SubmitLogin ->
                    case model.screen of
                        ScreenLogin screenModel ->
                            ( MkModel model
                            , Platform.Cmd.map MessageLogin <| Mensam.Login.login screenModel
                            )

                        _ ->
                            update (ReportError "Can't process a message for the wrong screen.") <| MkModel model

                Mensam.Login.Register ->
                    update EmptyMessage <| MkModel { model | screen = ScreenRegister Mensam.Register.init }

                Mensam.Login.SetSession x ->
                    update SwitchScreenSpaces <| MkModel { model | authenticated = Just x }

        SwitchScreenLogin ->
            update EmptyMessage <| MkModel { model | screen = ScreenLogin Mensam.Login.init }

        MessageSpaces (Mensam.Spaces.MessagePure m) ->
            case model.screen of
                ScreenSpaces screenModel ->
                    update EmptyMessage <| MkModel { model | screen = ScreenSpaces <| Mensam.Spaces.updatePure m screenModel }

                _ ->
                    update (ReportError "Can't process a message for the wrong screen.") <| MkModel model

        MessageSpaces (Mensam.Spaces.MessageEffect m) ->
            case m of
                Mensam.Spaces.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Spaces.RefreshSpaces ->
                    case model.authenticated of
                        Just { jwt } ->
                            ( MkModel model
                            , Platform.Cmd.map MessageSpaces <| Mensam.Spaces.deskListRequest jwt
                            )

                        Nothing ->
                            update (ReportError "Can't make request without JWT.") <| MkModel model

        SwitchScreenSpaces ->
            update EmptyMessage <| MkModel { model | screen = ScreenSpaces Mensam.Spaces.init }


view : Model -> Browser.Document Message
view (MkModel model) =
    { title = "Mensam"
    , body =
        [ Element.layout
            [ Element.Background.gradient { angle = 0, steps = [ Mensam.Color.dark.yellow, Mensam.Color.bright.yellow ] }
            , Element.Font.color Mensam.Color.dark.black
            , Element.Font.regular
            , Element.Font.size 20
            , Mensam.Font.families
            ]
          <|
            Element.el
                [ Element.htmlAttribute <| Html.Attributes.style "min-width" "800px"
                , Element.htmlAttribute <| Html.Attributes.style "max-width" "1000px"
                , Element.htmlAttribute <| Html.Attributes.style "margin-left" "auto"
                , Element.htmlAttribute <| Html.Attributes.style "margin-right" "auto"
                , Element.width Element.fill
                , Element.height Element.fill
                , Element.Background.color Mensam.Color.dark.black
                , Element.Font.color Mensam.Color.bright.white
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
                , Element.Font.family [ Mensam.Font.sansSerif ]
                , Element.htmlAttribute <| Html.Attributes.style "text-transform" "none"
                , Element.Font.color Mensam.Color.bright.yellow
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
        , Element.Background.color Mensam.Color.bright.black
        , Element.Font.family [ Mensam.Font.condensed ]
        , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
        , Element.Font.light
        , Element.Font.size 17
        ]
        ([ title ] ++ elementsTabDescription ++ [ loginStatus ])
