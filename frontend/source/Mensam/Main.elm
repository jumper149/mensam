module Mensam.Main exposing (..)

import Browser
import Browser.Navigation
import Element
import Element.Background
import Element.Events
import Element.Font
import Html.Attributes
import Json.Encode
import Mensam.Api.Logout
import Mensam.Auth.Bearer
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Error
import Mensam.Screen.Landing
import Mensam.Screen.Login
import Mensam.Screen.Register
import Mensam.Screen.Space
import Mensam.Screen.Spaces
import Mensam.Storage
import Platform.Cmd
import Task
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
        , subscriptions = subscriptions
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
        , time :
            { now : Time.Posix
            , zone : Time.Zone
            }
        }


type alias Authentication =
    { jwt : Mensam.Auth.Bearer.Jwt
    , expiration : Maybe Time.Posix
    }


type Screen
    = ScreenLanding Mensam.Screen.Landing.Model
    | ScreenRegister Mensam.Screen.Register.Model
    | ScreenLogin Mensam.Screen.Login.Model
    | ScreenSpaces Mensam.Screen.Spaces.Model
    | ScreenSpace Mensam.Screen.Space.Model


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
                MkModel { model | screen = ScreenSpace <| Mensam.Screen.Space.init { id = id, time = model.time } }


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
            { navigationKey = navigationKey
            , screen = ScreenLanding Mensam.Screen.Landing.init
            , authenticated = Nothing
            , errors = []
            , viewErrors = False
            , time =
                { now = Time.millisToPosix 0
                , zone = Time.utc
                }
            }

        modelStorage =
            case Mensam.Storage.parse flags of
                Ok (Just (Mensam.Storage.MkStorage storage)) ->
                    { modelInit | authenticated = Just storage }

                Ok Nothing ->
                    modelInit

                Err error ->
                    { modelInit
                        | errors = error :: modelInit.errors
                    }
    in
    update
        (Messages
            [ SetTimeZoneHere
            , Raw <|
                \(MkModel model) ->
                    ( MkModel model
                    , Task.perform
                        (\now ->
                            Messages
                                [ Auth <| CheckExpirationExplicit now
                                , Raw <|
                                    \(MkModel m) ->
                                        case Url.Parser.parse urlParser url of
                                            Nothing ->
                                                update (SetUrl RouteLanding) (MkModel m)

                                            Just route ->
                                                let
                                                    redirectedRoute =
                                                        case route of
                                                            RouteLanding ->
                                                                case m.authenticated of
                                                                    Nothing ->
                                                                        RouteLanding

                                                                    Just _ ->
                                                                        RouteSpaces

                                                            _ ->
                                                                route
                                                in
                                                update (SetUrl redirectedRoute) (MkModel m)
                                ]
                        )
                        Time.now
                    )
            ]
        )
    <|
        MkModel modelStorage


type Message
    = EmptyMessage
    | Messages (List Message)
    | Raw (Model -> ( Model, Cmd Message ))
    | SetUrl Route
    | SetModel Route
    | ReportError Mensam.Error.Error
    | ClearErrors
    | ViewErrors
    | HideErrors
    | SetTimeNow Time.Posix
    | SetTimeZone Time.Zone
    | SetTimeZoneHere
    | Auth MessageAuth
    | MessageLanding Mensam.Screen.Landing.Message
    | MessageRegister Mensam.Screen.Register.Message
    | MessageLogin Mensam.Screen.Login.Message
    | MessageSpaces Mensam.Screen.Spaces.Message
    | MessageSpace Mensam.Screen.Space.Message


type MessageAuth
    = SetSession { jwt : Mensam.Auth.Bearer.Jwt, expiration : Maybe Time.Posix }
    | UnsetSession
    | Logout
    | CheckExpirationExplicit Time.Posix


update : Message -> Model -> ( Model, Platform.Cmd.Cmd Message )
update message (MkModel model) =
    case message of
        EmptyMessage ->
            ( MkModel model, Platform.Cmd.none )

        Messages messages ->
            updates (List.map update messages) <| MkModel model

        Raw f ->
            f <| MkModel model

        SetUrl route ->
            update
                (Messages
                    [ Raw <| \m -> ( m, Browser.Navigation.pushUrl model.navigationKey <| routeToUrl route )
                    , SetModel route
                    ]
                )
            <|
                MkModel model

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

        SetTimeNow timestamp ->
            update EmptyMessage <|
                MkModel
                    { model
                        | time =
                            let
                                time =
                                    model.time
                            in
                            { time | now = timestamp }
                    }

        SetTimeZone zone ->
            update EmptyMessage <|
                MkModel
                    { model
                        | time =
                            let
                                time =
                                    model.time
                            in
                            { time | zone = zone }
                    }

        SetTimeZoneHere ->
            let
                handleResult result =
                    case result of
                        Ok zone ->
                            SetTimeZone zone

                        Err _ ->
                            ReportError <| Mensam.Error.message "Failed to set local timezone" <| Mensam.Error.undefined
            in
            ( MkModel model
            , Task.attempt handleResult Time.here
            )

        Auth (SetSession session) ->
            ( MkModel { model | authenticated = Just session }
            , Mensam.Storage.setStorage <| Mensam.Storage.MkStorage session
            )

        Auth UnsetSession ->
            ( MkModel { model | authenticated = Nothing }, Mensam.Storage.unsetStorage )

        Auth Logout ->
            update
                (Messages
                    [ Raw <|
                        \(MkModel m) ->
                            case m.authenticated of
                                Nothing ->
                                    update EmptyMessage <| MkModel m

                                Just { jwt } ->
                                    ( MkModel m
                                    , Mensam.Api.Logout.request { jwt = jwt } <|
                                        \response ->
                                            case response of
                                                Ok Mensam.Api.Logout.Success ->
                                                    Auth UnsetSession

                                                Ok (Mensam.Api.Logout.ErrorAuth error) ->
                                                    ReportError <| Mensam.Auth.Bearer.error error

                                                Err error ->
                                                    ReportError <| Mensam.Error.http error
                                    )
                    , SetUrl <| RouteLanding
                    ]
                )
            <|
                MkModel model

        Auth (CheckExpirationExplicit now) ->
            case model.authenticated of
                Nothing ->
                    update EmptyMessage <| MkModel model

                Just authentication ->
                    if isExpired now authentication then
                        update (Auth UnsetSession) <| MkModel model

                    else
                        update EmptyMessage <| MkModel model

        MessageLanding (Mensam.Screen.Landing.MessageEffect m) ->
            case m of
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

                Mensam.Screen.Login.SetSession session ->
                    update
                        (Messages
                            [ Auth <| SetSession session
                            , SetUrl RouteSpaces
                            ]
                        )
                    <|
                        MkModel model

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

                Mensam.Screen.Space.SubmitReservation ->
                    case model.authenticated of
                        Just { jwt } ->
                            case model.screen of
                                ScreenSpace screenModel ->
                                    case screenModel.viewDetailed of
                                        Nothing ->
                                            update (ReportError errorScreen) <| MkModel model

                                        Just { desk } ->
                                            ( MkModel model
                                            , Platform.Cmd.map MessageSpace <| Mensam.Screen.Space.reservationCreate jwt screenModel { desk = { id = desk.id } }
                                            )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Nothing ->
                            update (ReportError errorNoAuth) <| MkModel model


isExpired : Time.Posix -> Authentication -> Bool
isExpired now authentication =
    case authentication.expiration of
        Nothing ->
            False

        Just expiration ->
            Time.posixToMillis now > Time.posixToMillis expiration


view : Model -> Browser.Document Message
view (MkModel model) =
    { title = "Mensam"
    , body =
        [ Element.layout
            [ Element.Background.gradient { angle = 0, steps = [ Mensam.Element.Color.dark.yellow, Mensam.Element.Color.bright.yellow ] }
            , Element.Font.color Mensam.Element.Color.dark.black
            , Element.Font.regular
            , Element.Font.size 20
            , Element.Font.family [ Mensam.Element.Font.sansSerif ]
            ]
          <|
            Element.el
                [ Element.htmlAttribute <| Html.Attributes.style "min-width" "393px"
                , Element.htmlAttribute <| Html.Attributes.style "max-width" "851px"
                , Element.htmlAttribute <| Html.Attributes.style "margin-left" "auto"
                , Element.htmlAttribute <| Html.Attributes.style "margin-right" "auto"
                , Element.width Element.fill
                , Element.height Element.fill
                , Element.Background.color Mensam.Element.Color.dark.black
                , Element.Font.color Mensam.Element.Color.bright.white
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
                        ]
                    ]
        ]
    }


subscriptions : Model -> Sub Message
subscriptions _ =
    Time.every 100 SetTimeNow


elementNavigationBar : Model -> Element.Element Message
elementNavigationBar (MkModel model) =
    let
        tabDescriptions =
            []

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
                            [ Element.Background.color <| Mensam.Element.Color.bright.white
                            , Element.Font.color <| Mensam.Element.Color.dark.black
                            ]
                        , Element.Font.color Mensam.Element.Color.bright.red
                        , Element.below <|
                            if model.viewErrors then
                                Element.el
                                    [ Element.Font.family [ Mensam.Element.Font.condensed ]
                                    , Element.htmlAttribute <| Html.Attributes.style "text-transform" "none"
                                    , Element.width <| Element.px 200
                                    , Element.padding 12
                                    , Element.Background.color Mensam.Element.Color.bright.black
                                    , Element.Font.color Mensam.Element.Color.bright.white
                                    , Element.mouseOver
                                        [ Element.Background.color <| Mensam.Element.Color.bright.white
                                        , Element.Font.color <| Mensam.Element.Color.dark.black
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
                        [ Element.Events.onClick <| Auth Logout
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
                , Element.Font.family [ Mensam.Element.Font.sansSerif ]
                , Element.htmlAttribute <| Html.Attributes.style "text-transform" "none"
                , Element.Font.color Mensam.Element.Color.bright.yellow
                , Element.Font.italic
                , Element.Font.extraLight
                , Element.Font.size 25
                , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                , Element.mouseOver [ Element.Background.color <| Element.rgba 1 1 1 0.1 ]
                , Element.Events.onClick <|
                    case model.authenticated of
                        Nothing ->
                            SetUrl RouteLanding

                        Just _ ->
                            SetUrl RouteSpaces
                ]
            <|
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                <|
                    Element.text "Mensam"

        screenTitleText =
            case model.screen of
                ScreenLanding _ ->
                    Nothing

                ScreenRegister _ ->
                    Nothing

                ScreenLogin _ ->
                    Nothing

                ScreenSpaces _ ->
                    Just "Spaces"

                ScreenSpace screenModel ->
                    Just <| "Space: " ++ String.fromInt screenModel.space.id

        screenTitle =
            case screenTitleText of
                Nothing ->
                    Element.none

                Just text ->
                    Element.el
                        [ Element.height Element.fill
                        , Element.paddingXY 20 0
                        , Element.centerX
                        , Element.Font.family [ Mensam.Element.Font.sansSerif ]
                        , Element.htmlAttribute <| Html.Attributes.style "text-transform" "none"
                        , Element.Font.color Mensam.Element.Color.bright.white
                        , Element.Font.italic
                        , Element.Font.extraLight
                        , Element.Font.size 17
                        ]
                    <|
                        Element.el
                            [ Element.centerX
                            , Element.centerY
                            ]
                        <|
                            Element.text text
    in
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px 60
        , Element.Background.color Mensam.Element.Color.bright.black
        ]
    <|
        Element.row
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.Font.family [ Mensam.Element.Font.condensed ]
            , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
            , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
            , Element.Font.light
            , Element.Font.size 17
            , Element.behindContent screenTitle
            ]
            (title :: elementsTabDescription ++ [ errorViewer, loginStatus ])


errorScreen : Mensam.Error.Error
errorScreen =
    Mensam.Error.message "Can't process a message for the wrong screen" Mensam.Error.undefined


errorNoAuth : Mensam.Error.Error
errorNoAuth =
    Mensam.Error.message "Can't make request without JWT" Mensam.Error.undefined


updates : List (model -> ( model, Platform.Cmd.Cmd message )) -> model -> ( model, Platform.Cmd.Cmd message )
updates messages model =
    case messages of
        [] ->
            ( model, Platform.Cmd.none )

        updateNow :: otherMessages ->
            let
                ( modelUpdated, cmdUpdated ) =
                    updateNow model

                ( modelFinal, cmdFinal ) =
                    updates otherMessages modelUpdated
            in
            ( modelFinal, Platform.Cmd.batch [ cmdUpdated, cmdFinal ] )
