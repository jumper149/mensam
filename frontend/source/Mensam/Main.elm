module Mensam.Main exposing (..)

import Browser
import Browser.Navigation
import Element
import Element.Background
import Element.Events
import Element.Font
import Html.Attributes
import Json.Decode
import Json.Encode
import Mensam.Color
import Mensam.Error
import Mensam.Font
import Mensam.Jwt
import Mensam.Screen.Landing
import Mensam.Screen.Login
import Mensam.Screen.Register
import Mensam.Screen.Space
import Mensam.Screen.Spaces
import Mensam.Storage
import Platform.Cmd
import Platform.Sub
import Time
import Url
import Url.Builder
import Url.Parser


main : Program Json.Encode.Value Model Message
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Platform.Sub.none
        , onUrlRequest = \_ -> EmptyMessage
        , onUrlChange = onUrlChange
        }


type Model
    = MkModel
        { navigationKey : Browser.Navigation.Key
        , screen : Screen
        , authenticated : Maybe Authentication
        , errors : List Mensam.Error.Error
        , viewErrors : Bool
        }


type alias Authentication =
    { jwt : Mensam.Jwt.Jwt
    , expiration : Maybe Time.Posix
    }


type Screen
    = ScreenLanding Mensam.Screen.Landing.Model
    | ScreenRegister Mensam.Screen.Register.Model
    | ScreenLogin Mensam.Screen.Login.Model
    | ScreenSpaces Mensam.Screen.Spaces.Model
    | ScreenSpace Mensam.Screen.Space.Model
    | NoScreen


type Route
    = RouteLanding
    | RouteLogin (Maybe Mensam.Screen.Login.Model)
    | RouteRegister
    | RouteSpaces
    | RouteSpace Int


routeToUrl : Route -> String
routeToUrl _ =
    Url.Builder.absolute [] []


routeToModelUpdate : Route -> Model -> ( Model, Cmd Message )
routeToModelUpdate route (MkModel model) =
    case route of
        RouteLanding ->
            update EmptyMessage <| MkModel { model | screen = ScreenLanding Mensam.Screen.Landing.init }

        RouteLogin maybeInitLogin ->
            case maybeInitLogin of
                Nothing ->
                    update EmptyMessage <| MkModel { model | screen = ScreenLogin Mensam.Screen.Login.init }

                Just initLogin ->
                    update EmptyMessage <| MkModel { model | screen = ScreenLogin initLogin }

        RouteRegister ->
            update EmptyMessage <| MkModel { model | screen = ScreenRegister Mensam.Screen.Register.init }

        RouteSpaces ->
            update (MessageSpaces <| Mensam.Screen.Spaces.MessageEffect Mensam.Screen.Spaces.RefreshSpaces) <|
                MkModel { model | screen = ScreenSpaces Mensam.Screen.Spaces.init }

        RouteSpace id ->
            update (MessageSpace <| Mensam.Screen.Space.MessageEffect Mensam.Screen.Space.RefreshDesks) <|
                MkModel { model | screen = ScreenSpace <| Mensam.Screen.Space.init { id = id } }


urlParser : Url.Parser.Parser (Route -> c) c
urlParser =
    Url.Parser.oneOf
        [ Url.Parser.map RouteLanding <| Url.Parser.top
        , Url.Parser.map (RouteLogin Nothing) <| Url.Parser.s "login"
        , Url.Parser.map RouteRegister <| Url.Parser.s "register"
        , Url.Parser.map RouteSpaces <| Url.Parser.s "spaces"
        ]


onUrlChange : Url.Url -> Message
onUrlChange _ =
    EmptyMessage


init : Json.Encode.Value -> Url.Url -> Browser.Navigation.Key -> ( Model, Platform.Cmd.Cmd Message )
init flags url navigationKey =
    let
        modelInit =
            MkModel
                { navigationKey = navigationKey
                , screen = ScreenLanding Mensam.Screen.Landing.init
                , authenticated =
                    case Json.Decode.decodeValue Mensam.Storage.decode flags of
                        Ok (Mensam.Storage.MkStorage storage) ->
                            Just storage

                        Err _ ->
                            Nothing
                , errors = []
                , viewErrors = False
                }

        model =
            case Url.Parser.parse urlParser url of
                Nothing ->
                    update (SetUrl RouteLanding) modelInit

                Just route ->
                    update (SetUrl route) modelInit
    in
    model


type Message
    = EmptyMessage
    | SetUrl Route
    | SetModel Route
    | ReportError Mensam.Error.Error
    | ClearErrors
    | ViewErrors
    | HideErrors
    | Logout
    | MessageLanding Mensam.Screen.Landing.Message
    | MessageRegister Mensam.Screen.Register.Message
    | MessageLogin Mensam.Screen.Login.Message
    | MessageSpaces Mensam.Screen.Spaces.Message
    | MessageSpace Mensam.Screen.Space.Message


update : Message -> Model -> ( Model, Platform.Cmd.Cmd Message )
update message (MkModel model) =
    case message of
        EmptyMessage ->
            ( MkModel model, Platform.Cmd.none )

        SetUrl route ->
            let
                updateUrlCmd =
                    Browser.Navigation.pushUrl model.navigationKey <| routeToUrl route

                ( setModel, setCmd ) =
                    update (SetModel route) <| MkModel model
            in
            ( setModel, Platform.Cmd.batch [ updateUrlCmd, setCmd ] )

        SetModel route ->
            routeToModelUpdate route (MkModel model)

        ReportError error ->
            update EmptyMessage <| MkModel { model | errors = error :: model.errors }

        ClearErrors ->
            update EmptyMessage <| MkModel { model | errors = [] }

        ViewErrors ->
            update EmptyMessage <| MkModel { model | viewErrors = True }

        HideErrors ->
            update ClearErrors <| MkModel { model | viewErrors = False }

        Logout ->
            let
                ( modelLoggedOut, cmdLoggedOut ) =
                    ( MkModel { model | authenticated = Nothing }
                    , Mensam.Storage.unsetStorage
                    )

                ( modelUpdated, cmdUpdated ) =
                    update (SetUrl <| RouteLogin Nothing) <| modelLoggedOut
            in
            ( modelUpdated, Platform.Cmd.batch [ cmdLoggedOut, cmdUpdated ] )

        MessageLanding (Mensam.Screen.Landing.MessagePure m) ->
            case model.screen of
                ScreenLanding screenModel ->
                    update EmptyMessage <|
                        MkModel { model | screen = ScreenLanding <| Mensam.Screen.Landing.updatePure m screenModel }

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageLanding (Mensam.Screen.Landing.MessageEffect m) ->
            case m of
                Mensam.Screen.Landing.ReportError error ->
                    update (ReportError error) <| MkModel model

                Mensam.Screen.Landing.Login ->
                    update (SetUrl <| RouteLogin Nothing) <| MkModel model

                Mensam.Screen.Landing.Register ->
                    update (SetUrl RouteRegister) <| MkModel model

        MessageRegister (Mensam.Screen.Register.MessagePure m) ->
            case model.screen of
                ScreenRegister screenModel ->
                    update EmptyMessage <|
                        MkModel { model | screen = ScreenRegister <| Mensam.Screen.Register.updatePure m screenModel }

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageRegister (Mensam.Screen.Register.MessageEffect m) ->
            case m of
                Mensam.Screen.Register.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Screen.Register.Submit ->
                    case model.screen of
                        ScreenRegister screenModel ->
                            ( MkModel model
                            , Platform.Cmd.map MessageRegister <| Mensam.Screen.Register.register screenModel
                            )

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Register.Submitted ->
                    case model.screen of
                        ScreenRegister screenModel ->
                            update (SetUrl <| RouteLogin <| Just { username = screenModel.username, password = screenModel.password, hint = "" }) <| MkModel model

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Register.Login ->
                    update EmptyMessage <| MkModel { model | screen = ScreenLogin Mensam.Screen.Login.init }

        MessageLogin (Mensam.Screen.Login.MessagePure m) ->
            case model.screen of
                ScreenLogin screenModel ->
                    update EmptyMessage <| MkModel { model | screen = ScreenLogin <| Mensam.Screen.Login.updatePure m screenModel }

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageLogin (Mensam.Screen.Login.MessageEffect m) ->
            case m of
                Mensam.Screen.Login.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Screen.Login.SubmitLogin ->
                    case model.screen of
                        ScreenLogin screenModel ->
                            ( MkModel model
                            , Platform.Cmd.map MessageLogin <| Mensam.Screen.Login.login screenModel
                            )

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Login.Register ->
                    update EmptyMessage <| MkModel { model | screen = ScreenRegister Mensam.Screen.Register.init }

                Mensam.Screen.Login.SetSession x ->
                    let
                        ( modelAuthenticated, cmdAuthenticated ) =
                            ( MkModel { model | authenticated = Just x }
                            , Mensam.Storage.setStorage <| Mensam.Storage.MkStorage x
                            )

                        ( modelUpdated, cmdUpdated ) =
                            update (SetUrl RouteSpaces) <| modelAuthenticated
                    in
                    ( modelUpdated, Platform.Cmd.batch [ cmdAuthenticated, cmdUpdated ] )

        MessageSpaces (Mensam.Screen.Spaces.MessagePure m) ->
            case model.screen of
                ScreenSpaces screenModel ->
                    update EmptyMessage <| MkModel { model | screen = ScreenSpaces <| Mensam.Screen.Spaces.updatePure m screenModel }

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageSpaces (Mensam.Screen.Spaces.MessageEffect m) ->
            case m of
                Mensam.Screen.Spaces.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Screen.Spaces.RefreshSpaces ->
                    case model.authenticated of
                        Just { jwt } ->
                            ( MkModel model
                            , Platform.Cmd.map MessageSpaces <| Mensam.Screen.Spaces.spaceList jwt
                            )

                        Nothing ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Spaces.ChooseSpace { id } ->
                    update (SetUrl <| RouteSpace id) <| MkModel model

        MessageSpace (Mensam.Screen.Space.MessagePure m) ->
            case model.screen of
                ScreenSpace screenModel ->
                    update EmptyMessage <| MkModel { model | screen = ScreenSpace <| Mensam.Screen.Space.updatePure m screenModel }

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageSpace (Mensam.Screen.Space.MessageEffect m) ->
            case m of
                Mensam.Screen.Space.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Screen.Space.RefreshDesks ->
                    case model.authenticated of
                        Just { jwt } ->
                            case model.screen of
                                ScreenSpace screenModel ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageSpace <| Mensam.Screen.Space.deskList jwt screenModel
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Nothing ->
                            update (ReportError errorNoAuth) <| MkModel model


view : Model -> Browser.Document Message
view (MkModel model) =
    { title = "Mensam"
    , body =
        [ Element.layout
            [ Element.Background.gradient { angle = 0, steps = [ Mensam.Color.dark.yellow, Mensam.Color.bright.yellow ] }
            , Element.Font.color Mensam.Color.dark.black
            , Element.Font.regular
            , Element.Font.size 20
            , Element.Font.family [ Mensam.Font.sansSerif ]
            ]
          <|
            Element.el
                [ Element.htmlAttribute <| Html.Attributes.style "min-width" "393px"
                , Element.htmlAttribute <| Html.Attributes.style "max-width" "851px"
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
                            ScreenLanding screenModel ->
                                Element.el
                                    [ Element.width Element.fill
                                    , Element.height Element.fill
                                    ]
                                <|
                                    Element.map MessageLanding <|
                                        Mensam.Screen.Landing.element screenModel

                            ScreenLogin screenModel ->
                                Element.el
                                    [ Element.width Element.fill
                                    , Element.height Element.fill
                                    ]
                                <|
                                    Element.map MessageLogin <|
                                        Mensam.Screen.Login.element screenModel

                            ScreenRegister screenModel ->
                                Element.el
                                    [ Element.width Element.fill
                                    , Element.height Element.fill
                                    ]
                                <|
                                    Element.map MessageRegister <|
                                        Mensam.Screen.Register.element screenModel

                            ScreenSpaces screenModel ->
                                Element.el
                                    [ Element.width Element.fill
                                    , Element.height Element.fill
                                    ]
                                <|
                                    Element.map MessageSpaces <|
                                        Mensam.Screen.Spaces.element screenModel

                            ScreenSpace screenModel ->
                                Element.el
                                    [ Element.width Element.fill
                                    , Element.height Element.fill
                                    ]
                                <|
                                    Element.map MessageSpace <|
                                        Mensam.Screen.Space.element screenModel

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
              , message = SetUrl RouteSpaces
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
            case model.errors of
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
                                    , Element.padding 12
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
                                        List.map Mensam.Error.toElement errors

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
                        [ Element.Events.onClick <| SetUrl <| RouteLogin Nothing
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
                        [ Element.Events.onClick Logout
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
                            Element.text "Sign out"

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


errorScreen : Mensam.Error.Error
errorScreen =
    Mensam.Error.message "Can't process a message for the wrong screen" Mensam.Error.undefined


errorNoAuth : Mensam.Error.Error
errorNoAuth =
    Mensam.Error.message "Can't make request without JWT" Mensam.Error.undefined
