module Mensam.Main exposing (..)

import Browser
import Browser.Navigation
import Element
import Element.Background
import Element.Events
import Element.Font
import Html.Attributes
import Mensam.Color
import Mensam.Font
import Mensam.Jwt
import Mensam.Login
import Mensam.Register
import Mensam.Space
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
        , viewErrors : Bool
        }


type alias Authentication =
    { jwt : Mensam.Jwt.Jwt
    , expiration : Maybe Time.Posix
    }


type Screen
    = ScreenRegister Mensam.Register.Model
    | ScreenLogin Mensam.Login.Model
    | ScreenSpaces Mensam.Spaces.Model
    | ScreenSpace Mensam.Space.Model
    | NoScreen


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Platform.Cmd.Cmd Message )
init _ _ _ =
    ( MkModel { screen = ScreenLogin Mensam.Login.init, authenticated = Nothing, error = [], viewErrors = False }, Platform.Cmd.none )


type Message
    = EmptyMessage
    | ReportError String
    | ClearErrors
    | ViewErrors
    | HideErrors
    | MessageRegister Mensam.Register.Message
    | SwitchScreenRegister
    | MessageLogin Mensam.Login.Message
    | SwitchScreenLogin (Maybe Mensam.Login.Model)
    | MessageSpaces Mensam.Spaces.Message
    | SwitchScreenSpaces
    | MessageSpace Mensam.Space.Message
    | SwitchScreenSpace Int


update : Message -> Model -> ( Model, Platform.Cmd.Cmd Message )
update message (MkModel model) =
    case message of
        EmptyMessage ->
            ( MkModel model, Platform.Cmd.none )

        ReportError err ->
            update EmptyMessage <| MkModel { model | error = err :: model.error }

        ClearErrors ->
            update EmptyMessage <| MkModel { model | error = [] }

        ViewErrors ->
            update EmptyMessage <| MkModel { model | viewErrors = True }

        HideErrors ->
            update ClearErrors <| MkModel { model | viewErrors = False }

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
                            , Platform.Cmd.map MessageRegister <| Mensam.Register.register screenModel
                            )

                        _ ->
                            update (ReportError "Can't process a message for the wrong screen.") <| MkModel model

                Mensam.Register.Submitted ->
                    case model.screen of
                        ScreenRegister screenModel ->
                            update (SwitchScreenLogin <| Just { username = screenModel.username, password = screenModel.password, hint = "" }) <| MkModel model

                        _ ->
                            update (ReportError "Can't process a message for the wrong screen.") <| MkModel model

                Mensam.Register.Login ->
                    update EmptyMessage <| MkModel { model | screen = ScreenLogin Mensam.Login.init }

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

        SwitchScreenLogin maybeInitLogin ->
            case maybeInitLogin of
                Nothing ->
                    update EmptyMessage <| MkModel { model | screen = ScreenLogin Mensam.Login.init }

                Just initLogin ->
                    update EmptyMessage <| MkModel { model | screen = ScreenLogin initLogin }

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
                            , Platform.Cmd.map MessageSpaces <| Mensam.Spaces.spaceList jwt
                            )

                        Nothing ->
                            update (ReportError "Can't make request without JWT.") <| MkModel model

                Mensam.Spaces.ChooseSpace { id } ->
                    update (SwitchScreenSpace id) <| MkModel model

        SwitchScreenSpaces ->
            update (MessageSpaces <| Mensam.Spaces.MessageEffect Mensam.Spaces.RefreshSpaces) <| MkModel { model | screen = ScreenSpaces Mensam.Spaces.init }

        MessageSpace (Mensam.Space.MessagePure m) ->
            case model.screen of
                ScreenSpace screenModel ->
                    update EmptyMessage <| MkModel { model | screen = ScreenSpace <| Mensam.Space.updatePure m screenModel }

                _ ->
                    update (ReportError "Can't process a message for the wrong screen.") <| MkModel model

        MessageSpace (Mensam.Space.MessageEffect m) ->
            case m of
                Mensam.Space.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Space.RefreshDesks ->
                    case model.authenticated of
                        Just { jwt } ->
                            case model.screen of
                                ScreenSpace screenModel ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageSpace <| Mensam.Space.deskList jwt screenModel
                                    )

                                _ ->
                                    update (ReportError "Can't process a message for the wrong screen.") <| MkModel model

                        Nothing ->
                            update (ReportError "Can't make request without JWT.") <| MkModel model

        SwitchScreenSpace id ->
            update (MessageSpace <| Mensam.Space.MessageEffect Mensam.Space.RefreshDesks) <| MkModel { model | screen = ScreenSpace <| Mensam.Space.init { id = id } }


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
                [ Element.htmlAttribute <| Html.Attributes.style "min-width" "393px"
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
                                Element.el
                                    [ Element.width Element.fill
                                    , Element.height Element.fill
                                    ]
                                <|
                                    Element.map MessageRegister <|
                                        Mensam.Register.element screenModel

                            ScreenSpaces screenModel ->
                                Element.el
                                    [ Element.width Element.fill
                                    , Element.height Element.fill
                                    ]
                                <|
                                    Element.map MessageSpaces <|
                                        Mensam.Spaces.element screenModel

                            ScreenSpace screenModel ->
                                Element.el
                                    [ Element.width Element.fill
                                    , Element.height Element.fill
                                    ]
                                <|
                                    Element.map MessageSpace <|
                                        Mensam.Space.element screenModel

                            NoScreen ->
                                Element.none
                        ]
                    ]
        ]
    }


elementNavigationBar : Model -> Element.Element Message
elementNavigationBar (MkModel model) =
    let
        tabDescriptions =
            [ { name = "Spaces"
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

        errorViewer =
            case model.error of
                [] ->
                    Element.none

                errors ->
                    Element.el
                        [ Element.Events.onClick <|
                            if model.viewErrors then
                                HideErrors

                            else
                                ViewErrors
                        , Element.height Element.fill
                        , Element.paddingXY 20 0
                        , Element.alignRight
                        , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                        , Element.mouseOver
                            [ Element.Background.color <| Mensam.Color.bright.white
                            , Element.Font.color <| Mensam.Color.dark.black
                            ]
                        , Element.Font.color Mensam.Color.bright.red
                        , Element.below <|
                            if model.viewErrors then
                                Element.el
                                    [ Element.Font.family [ Mensam.Font.condensed ]
                                    , Element.htmlAttribute <| Html.Attributes.style "text-transform" "none"
                                    , Element.width <| Element.px 200
                                    , Element.padding 15
                                    , Element.Background.color Mensam.Color.bright.black
                                    , Element.Font.color Mensam.Color.bright.white
                                    , Element.mouseOver
                                        [ Element.Background.color <| Mensam.Color.bright.white
                                        , Element.Font.color <| Mensam.Color.dark.black
                                        ]
                                    , Element.alignRight
                                    ]
                                <|
                                    Element.column
                                        [ Element.spacing 10
                                        ]
                                    <|
                                        List.map (\message -> Element.paragraph [] [ Element.text message ]) errors

                            else
                                Element.none
                        ]
                    <|
                        Element.el
                            [ Element.centerX
                            , Element.centerY
                            ]
                        <|
                            Element.text "?"

        loginStatus =
            case model.authenticated of
                Nothing ->
                    Element.el
                        [ Element.Events.onClick <| SwitchScreenLogin Nothing
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
        (title :: elementsTabDescription ++ [ errorViewer, loginStatus ])
