module Mensam.Main exposing (..)

import Browser
import Browser.Navigation
import Element
import Http
import Http.Extra
import Json.Encode as Encode
import Mensam.Api.Logout
import Mensam.Api.Profile
import Mensam.Application
import Mensam.Auth
import Mensam.Auth.Bearer
import Mensam.Element
import Mensam.Element.Footer
import Mensam.Element.Header
import Mensam.Error
import Mensam.Flags
import Mensam.Screen.Landing
import Mensam.Screen.Login
import Mensam.Screen.Register
import Mensam.Screen.Reservations
import Mensam.Screen.Space
import Mensam.Screen.Space.Join
import Mensam.Screen.Space.Role
import Mensam.Screen.Space.Roles
import Mensam.Screen.Space.Settings
import Mensam.Screen.Space.Users
import Mensam.Screen.Spaces
import Mensam.Space
import Mensam.Space.Role
import Mensam.Storage
import Mensam.Time
import Mensam.User
import Platform.Cmd
import Platform.Sub
import Time
import Url
import Url.Builder
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query


main : Program Encode.Value Model Message
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = FollowLink
        , onUrlChange = ChangedUrl
        }


type Model
    = MkModel
        { navigationKey : Browser.Navigation.Key
        , screen : Screen
        , authenticated : Mensam.Auth.Model
        , errors : List Mensam.Error.Error
        , viewErrors : Bool
        , viewHamburgerMenu : Bool
        , time :
            { now : Time.Posix
            , zone : Time.Zone
            , zoneIdentifier : Mensam.Time.TimezoneIdentifier
            }
        , httpStatus : Http.Extra.Status
        }


type Screen
    = ScreenLanding Mensam.Screen.Landing.Model
    | ScreenRegister Mensam.Screen.Register.Model
    | ScreenLogin Mensam.Screen.Login.Model
    | ScreenSpaces Mensam.Screen.Spaces.Model
    | ScreenSpace Mensam.Screen.Space.Model
    | ScreenSpaceJoin Mensam.Screen.Space.Join.Model
    | ScreenSpaceRoles Mensam.Screen.Space.Roles.Model
    | ScreenSpaceRole Mensam.Screen.Space.Role.Model
    | ScreenSpaceUsers Mensam.Screen.Space.Users.Model
    | ScreenSpaceSettings Mensam.Screen.Space.Settings.Model
    | ScreenReservations Mensam.Screen.Reservations.Model


type Route
    = RouteLanding
    | RouteLogin (Maybe Mensam.Screen.Login.Model)
    | RouteRegister
    | RouteSpaces
    | RouteSpace Mensam.Space.Identifier
    | RouteSpaceJoin { spaceId : Mensam.Space.Identifier, roleId : Maybe Mensam.Space.Role.Identifier, password : Maybe String }
    | RouteSpaceRoles Mensam.Space.Identifier
    | RouteSpaceRole { spaceId : Mensam.Space.Identifier, roleId : Mensam.Space.Role.Identifier }
    | RouteSpaceUsers Mensam.Space.Identifier
    | RouteSpaceSettings Mensam.Space.Identifier
    | RouteReservations


routeToUrl : Route -> String
routeToUrl route =
    case route of
        RouteLanding ->
            Url.Builder.absolute [] []

        RouteLogin _ ->
            Url.Builder.absolute [ "login" ] []

        RouteRegister ->
            Url.Builder.absolute [ "register" ] []

        RouteSpaces ->
            Url.Builder.absolute [ "spaces" ] []

        RouteSpace identifier ->
            Url.Builder.absolute [ "space", Mensam.Space.identifierToString identifier ] []

        RouteSpaceJoin { spaceId, roleId, password } ->
            Url.Builder.absolute
                [ "join"
                , "space"
                , Mensam.Space.identifierToString spaceId
                ]
            <|
                List.filterMap (\x -> x)
                    [ Maybe.map (Url.Builder.int "role" << Mensam.Space.Role.identifierToInt) roleId
                    , Maybe.map (Url.Builder.string "password") password
                    ]

        RouteSpaceRoles identifier ->
            Url.Builder.absolute [ "roles", "space", Mensam.Space.identifierToString identifier ] []

        RouteSpaceRole { spaceId, roleId } ->
            Url.Builder.absolute [ "role", Mensam.Space.Role.identifierToString roleId, "space", Mensam.Space.identifierToString spaceId ] []

        RouteSpaceUsers identifier ->
            Url.Builder.absolute [ "users", "space", Mensam.Space.identifierToString identifier ] []

        RouteSpaceSettings identifier ->
            Url.Builder.absolute [ "settings", "space", Mensam.Space.identifierToString identifier ] []

        RouteReservations ->
            Url.Builder.absolute [ "reservations" ] []


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
                MkModel { model | screen = ScreenSpaces <| Mensam.Screen.Spaces.init }

        RouteSpace identifier ->
            update
                (Messages
                    [ MessageSpace <| Mensam.Screen.Space.MessageEffect Mensam.Screen.Space.RefreshSpace
                    ]
                )
            <|
                MkModel { model | screen = ScreenSpace <| Mensam.Screen.Space.init { id = identifier, time = { now = model.time.now, zone = model.time.zone } } }

        RouteSpaceJoin { spaceId, roleId, password } ->
            update
                (Messages
                    [ MessageSpaceJoin <| Mensam.Screen.Space.Join.MessageEffect Mensam.Screen.Space.Join.RefreshSpace
                    ]
                )
            <|
                MkModel { model | screen = ScreenSpaceJoin <| Mensam.Screen.Space.Join.init { spaceId = spaceId, roleIdSelected = roleId, password = password } }

        RouteSpaceRoles spaceId ->
            update
                (Messages
                    [ MessageSpaceRoles <| Mensam.Screen.Space.Roles.MessageEffect Mensam.Screen.Space.Roles.RefreshRoles
                    ]
                )
            <|
                MkModel { model | screen = ScreenSpaceRoles <| Mensam.Screen.Space.Roles.init { id = spaceId } }

        RouteSpaceRole { spaceId, roleId } ->
            update
                (Messages
                    [ MessageSpaceRole <| Mensam.Screen.Space.Role.MessageEffect Mensam.Screen.Space.Role.RefreshRole
                    ]
                )
            <|
                MkModel { model | screen = ScreenSpaceRole <| Mensam.Screen.Space.Role.init { spaceId = spaceId, roleId = roleId } }

        RouteSpaceUsers identifier ->
            update
                (Messages
                    [ MessageSpaceUsers <| Mensam.Screen.Space.Users.MessageEffect Mensam.Screen.Space.Users.Refresh
                    ]
                )
            <|
                MkModel { model | screen = ScreenSpaceUsers <| Mensam.Screen.Space.Users.init { id = identifier } }

        RouteSpaceSettings identifier ->
            update
                (Messages
                    [ MessageSpaceSettings <| Mensam.Screen.Space.Settings.MessageEffect Mensam.Screen.Space.Settings.RefreshOldSettings
                    ]
                )
            <|
                MkModel { model | screen = ScreenSpaceSettings <| Mensam.Screen.Space.Settings.init { id = identifier } }

        RouteReservations ->
            update (MessageReservations <| Mensam.Screen.Reservations.MessageEffect Mensam.Screen.Reservations.RefreshReservations) <|
                MkModel { model | screen = ScreenReservations <| Mensam.Screen.Reservations.init { time = { now = model.time.now, zone = model.time.zone } } }


urlParser : Url.Parser.Parser (Route -> c) c
urlParser =
    Url.Parser.oneOf
        [ Url.Parser.map RouteLanding <| Url.Parser.top
        , Url.Parser.map (RouteLogin Nothing) <| Url.Parser.s "login"
        , Url.Parser.map RouteRegister <| Url.Parser.s "register"
        , Url.Parser.map RouteReservations <| Url.Parser.s "reservations"
        , Url.Parser.map RouteSpaces <| Url.Parser.s "spaces"
        , Url.Parser.map RouteSpace <| Url.Parser.s "space" </> Url.Parser.map Mensam.Space.MkIdentifier Url.Parser.int
        , Url.Parser.map
            (\spaceId roleId password ->
                RouteSpaceJoin
                    { spaceId = Mensam.Space.MkIdentifier spaceId
                    , roleId = Maybe.map Mensam.Space.Role.MkIdentifier roleId
                    , password = password
                    }
            )
          <|
            Url.Parser.s "join"
                </> Url.Parser.s "space"
                </> Url.Parser.int
                <?> Url.Parser.Query.int "role"
                <?> Url.Parser.Query.string "password"
        , Url.Parser.map RouteSpaceRoles <| Url.Parser.s "roles" </> Url.Parser.s "space" </> Url.Parser.map Mensam.Space.MkIdentifier Url.Parser.int
        , Url.Parser.map
            (\roleId spaceId ->
                RouteSpaceRole
                    { spaceId = spaceId
                    , roleId = roleId
                    }
            )
          <|
            Url.Parser.s "role"
                </> Url.Parser.map Mensam.Space.Role.MkIdentifier Url.Parser.int
                </> Url.Parser.s "space"
                </> Url.Parser.map Mensam.Space.MkIdentifier Url.Parser.int
        , Url.Parser.map RouteSpaceUsers <| Url.Parser.s "users" </> Url.Parser.s "space" </> Url.Parser.map Mensam.Space.MkIdentifier Url.Parser.int
        , Url.Parser.map RouteSpaceSettings <| Url.Parser.s "settings" </> Url.Parser.s "space" </> Url.Parser.map Mensam.Space.MkIdentifier Url.Parser.int
        ]


init : Mensam.Flags.FlagsRaw -> Url.Url -> Browser.Navigation.Key -> ( Model, Platform.Cmd.Cmd Message )
init flagsRaw url navigationKey =
    let
        modelInit =
            MkModel
                { navigationKey = navigationKey
                , screen = ScreenLanding Mensam.Screen.Landing.init
                , authenticated = Mensam.Auth.SignedOut
                , errors = []
                , viewErrors = False
                , viewHamburgerMenu = False
                , time =
                    { now = Time.millisToPosix 0
                    , zone = Time.utc
                    , zoneIdentifier = Mensam.Time.MkTimezoneIdentifier "Etc/UTC"
                    }
                , httpStatus = Http.Extra.Done
                }

        withFlags =
            \(MkModel model) ->
                case Mensam.Flags.parse flagsRaw of
                    Ok (Mensam.Flags.MkFlags flags) ->
                        MkModel
                            { model
                                | authenticated = Mensam.Auth.init flags.storage
                                , time = flags.time
                            }

                    Err error ->
                        MkModel
                            { model
                                | errors = error :: model.errors
                            }

        modelFinal =
            withFlags modelInit
    in
    update (InitModel { url = url }) modelFinal


type Message
    = EmptyMessage
    | Messages (List Message)
    | Raw (Model -> ( Model, Cmd Message ))
    | FollowLink Browser.UrlRequest
    | ChangedUrl Url.Url
    | InitModel { url : Url.Url }
    | SetUrl Route
    | SetModel Route
    | ReportError Mensam.Error.Error
    | ClearErrors
    | ViewErrors
    | HideErrors
    | ViewHamburgerMenu
    | HideHamburgerMenu
    | SetTimeNow Time.Posix
    | Auth MessageAuth
    | SetHttpStatus Http.Extra.Status
    | MessageLanding Mensam.Screen.Landing.Message
    | MessageRegister Mensam.Screen.Register.Message
    | MessageLogin Mensam.Screen.Login.Message
    | MessageSpaces Mensam.Screen.Spaces.Message
    | MessageSpace Mensam.Screen.Space.Message
    | MessageSpaceJoin Mensam.Screen.Space.Join.Message
    | MessageSpaceRoles Mensam.Screen.Space.Roles.Message
    | MessageSpaceRole Mensam.Screen.Space.Role.Message
    | MessageSpaceUsers Mensam.Screen.Space.Users.Message
    | MessageSpaceSettings Mensam.Screen.Space.Settings.Message
    | MessageReservations Mensam.Screen.Reservations.Message


type MessageAuth
    = SetSession { jwt : Mensam.Auth.Bearer.Jwt, expiration : Maybe Time.Posix, id : Mensam.User.Identifier }
    | UnsetSession
    | Logout
    | CheckExpirationExplicit Time.Posix
    | RefreshUserInfo
    | SetUserInfo { name : Mensam.User.Name }


update : Message -> Model -> ( Model, Platform.Cmd.Cmd Message )
update message (MkModel model) =
    case message of
        EmptyMessage ->
            ( MkModel model, Platform.Cmd.none )

        Messages messages ->
            Mensam.Application.updates (List.map update messages) <| MkModel model

        Raw f ->
            f <| MkModel model

        FollowLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case Url.Parser.parse urlParser url of
                        Nothing ->
                            update
                                (ReportError <|
                                    Mensam.Error.message "Invalid internal URL" <|
                                        Mensam.Error.message (Url.toString url) <|
                                            Mensam.Error.undefined
                                )
                            <|
                                MkModel model

                        Just route ->
                            update (SetUrl route) <| MkModel model

                Browser.External url ->
                    update
                        (ReportError <|
                            Mensam.Error.message "External URL currently unsupported" <|
                                Mensam.Error.message url <|
                                    Mensam.Error.undefined
                        )
                    <|
                        MkModel model

        ChangedUrl url ->
            case Url.Parser.parse urlParser url of
                Nothing ->
                    update
                        (ReportError <|
                            Mensam.Error.message "Invalid internal URL" <|
                                Mensam.Error.message (Url.toString url) <|
                                    Mensam.Error.undefined
                        )
                    <|
                        MkModel model

                Just route ->
                    update (SetModel route) <| MkModel model

        InitModel { url } ->
            update
                (Messages
                    [ Auth <| CheckExpirationExplicit model.time.now
                    , Auth <| RefreshUserInfo
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
                                                        Mensam.Auth.SignedOut ->
                                                            RouteLanding

                                                        Mensam.Auth.SignedIn _ ->
                                                            RouteSpaces

                                                _ ->
                                                    route
                                    in
                                    update (SetUrl redirectedRoute) (MkModel m)
                    ]
                )
            <|
                MkModel model

        SetUrl route ->
            update
                (Messages
                    [ Raw <| \m -> ( m, Browser.Navigation.pushUrl model.navigationKey <| routeToUrl route )
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

        ViewHamburgerMenu ->
            update EmptyMessage <| MkModel { model | viewHamburgerMenu = True }

        HideHamburgerMenu ->
            update EmptyMessage <| MkModel { model | viewHamburgerMenu = False }

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

        Auth (SetSession session) ->
            ( MkModel
                { model
                    | authenticated =
                        Mensam.Auth.SignedIn <|
                            Mensam.Auth.MkAuthentication
                                { jwt = session.jwt
                                , expiration = session.expiration
                                , user =
                                    { id = session.id
                                    , info = Nothing
                                    }
                                }
                }
            , Mensam.Storage.set <| Mensam.Storage.MkStorage session
            )

        Auth UnsetSession ->
            ( MkModel { model | authenticated = Mensam.Auth.SignedOut }, Mensam.Storage.unset )

        Auth Logout ->
            update
                (Messages
                    [ Raw <|
                        \(MkModel m) ->
                            case m.authenticated of
                                Mensam.Auth.SignedOut ->
                                    update EmptyMessage <| MkModel m

                                Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                                    ( MkModel m
                                    , Mensam.Api.Logout.request { jwt = jwt } <|
                                        \response ->
                                            case response of
                                                Ok Mensam.Api.Logout.Success ->
                                                    EmptyMessage

                                                Ok (Mensam.Api.Logout.ErrorAuth error) ->
                                                    ReportError <|
                                                        Mensam.Error.message "The server-side session has already been invalidated" <|
                                                            Mensam.Auth.Bearer.error error

                                                Err error ->
                                                    ReportError <|
                                                        Mensam.Error.message "Unable to invalidate the server-side session" <|
                                                            Mensam.Error.http error
                                    )
                    , Auth UnsetSession
                    , SetUrl <| RouteLanding
                    ]
                )
            <|
                MkModel model

        Auth (CheckExpirationExplicit now) ->
            case model.authenticated of
                Mensam.Auth.SignedOut ->
                    update EmptyMessage <| MkModel model

                Mensam.Auth.SignedIn authentication ->
                    if Mensam.Auth.isExpired authentication now then
                        update (Auth UnsetSession) <| MkModel model

                    else
                        update EmptyMessage <| MkModel model

        Auth RefreshUserInfo ->
            case model.authenticated of
                Mensam.Auth.SignedOut ->
                    update EmptyMessage <| MkModel model

                Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication authentication) ->
                    ( MkModel model
                    , Mensam.Api.Profile.request
                        { jwt = authentication.jwt
                        , id = authentication.user.id
                        }
                      <|
                        \response ->
                            case response of
                                Ok (Mensam.Api.Profile.Success body) ->
                                    Auth <| SetUserInfo { name = body.name }

                                Ok Mensam.Api.Profile.ErrorUnknownUser ->
                                    ReportError <| Mensam.Error.message "Unknown user while requesting information" Mensam.Error.undefined

                                Ok (Mensam.Api.Profile.ErrorBody error) ->
                                    ReportError <|
                                        Mensam.Error.message "Failed to request profile" <|
                                            Mensam.Error.message "Bad request body" <|
                                                Mensam.Error.message error
                                                    Mensam.Error.undefined

                                Ok (Mensam.Api.Profile.ErrorAuth error) ->
                                    ReportError <|
                                        Mensam.Error.message "Failed to request profile" <|
                                            Mensam.Auth.Bearer.error error

                                Err error ->
                                    ReportError <|
                                        Mensam.Error.message "Failed to request profile" <|
                                            Mensam.Error.http error
                    )

        Auth (SetUserInfo info) ->
            case model.authenticated of
                Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication authentication) ->
                    update EmptyMessage <|
                        MkModel <|
                            { model
                                | authenticated =
                                    let
                                        userNew =
                                            { id = authentication.user.id
                                            , info = Just info
                                            }
                                    in
                                    Mensam.Auth.SignedIn <|
                                        Mensam.Auth.MkAuthentication
                                            { authentication
                                                | user = userNew
                                            }
                            }

                Mensam.Auth.SignedOut ->
                    update
                        (ReportError <|
                            Mensam.Error.message "Cannot set user information unless signed in"
                                Mensam.Error.undefined
                        )
                    <|
                        MkModel model

        SetHttpStatus status ->
            update EmptyMessage <| MkModel { model | httpStatus = status }

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

                Mensam.Screen.Register.Submitted { emailSent } ->
                    case model.screen of
                        ScreenRegister screenModel ->
                            update
                                (Messages
                                    [ SetUrl <| RouteLogin <| Just { username = screenModel.username, password = screenModel.password, hint = "" }
                                    , if emailSent then
                                        EmptyMessage

                                      else
                                        ReportError <| Mensam.Error.message "Failed to send confirmation email" Mensam.Error.undefined
                                    ]
                                )
                            <|
                                MkModel model

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
                    update (SetUrl RouteRegister) <| MkModel model

                Mensam.Screen.Login.SetSession session ->
                    update
                        (Messages
                            [ Auth <| SetSession session
                            , Auth <| RefreshUserInfo
                            , SetUrl RouteSpaces
                            ]
                        )
                    <|
                        MkModel model

        MessageSpaces (Mensam.Screen.Spaces.MessagePure m) ->
            case model.screen of
                ScreenSpaces screenModel ->
                    update EmptyMessage <| MkModel { model | screen = ScreenSpaces <| Mensam.Screen.Spaces.updatePure m { timezone = model.time.zoneIdentifier } screenModel }

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageSpaces (Mensam.Screen.Spaces.MessageEffect m) ->
            case m of
                Mensam.Screen.Spaces.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Screen.Spaces.RefreshSpaces ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            ( MkModel model
                            , Platform.Cmd.map MessageSpaces <| Mensam.Screen.Spaces.spaceList jwt
                            )

                Mensam.Screen.Spaces.SubmitCreate formData ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            ( MkModel model
                            , Platform.Cmd.map
                                (\msg ->
                                    Messages
                                        [ MessageSpaces msg
                                        , MessageSpaces <| Mensam.Screen.Spaces.MessageEffect Mensam.Screen.Spaces.RefreshSpaces
                                        ]
                                )
                              <|
                                Mensam.Screen.Spaces.spaceCreate
                                    { jwt = jwt
                                    , timezone = formData.timezone
                                    , name = formData.name
                                    , visibility =
                                        if formData.visible then
                                            Mensam.Space.MkVisibilityVisible

                                        else
                                            Mensam.Space.MkVisibilityHidden
                                    }
                            )

                Mensam.Screen.Spaces.ChooseSpace identifier ->
                    update (SetUrl <| RouteSpace identifier) <| MkModel model

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

                Mensam.Screen.Space.RefreshSpace ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpace screenModel ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageSpace <| Mensam.Screen.Space.spaceView jwt screenModel
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.RefreshDesks ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpace screenModel ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageSpace <| Mensam.Screen.Space.deskList jwt screenModel
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.OpenPageToJoin ->
                    case model.screen of
                        ScreenSpace screenModel ->
                            update (SetUrl <| RouteSpaceJoin { spaceId = screenModel.space, roleId = Nothing, password = Nothing }) <| MkModel model

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Space.OpenPageToUsers ->
                    case model.screen of
                        ScreenSpace screenModel ->
                            update (SetUrl <| RouteSpaceUsers screenModel.space) <| MkModel model

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Space.OpenPageToSettings ->
                    case model.screen of
                        ScreenSpace screenModel ->
                            update (SetUrl <| RouteSpaceSettings screenModel.space) <| MkModel model

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Space.SubmitLeave ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpace screenModel ->
                                    case screenModel.popup of
                                        Just Mensam.Screen.Space.PopupLeave ->
                                            ( MkModel model
                                            , Platform.Cmd.map
                                                (\msg ->
                                                    Messages
                                                        [ MessageSpace msg
                                                        , MessageSpace <| Mensam.Screen.Space.MessageEffect Mensam.Screen.Space.RefreshSpace
                                                        ]
                                                )
                                              <|
                                                Mensam.Screen.Space.spaceLeave jwt screenModel.space
                                            )

                                        _ ->
                                            update (ReportError errorScreen) <| MkModel model

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.SubmitCreate ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpace screenModel ->
                                    case screenModel.popup of
                                        Just (Mensam.Screen.Space.PopupCreate { name }) ->
                                            ( MkModel model
                                            , Platform.Cmd.map
                                                (\msg ->
                                                    Messages
                                                        [ MessageSpace msg
                                                        , MessageSpace <| Mensam.Screen.Space.MessageEffect Mensam.Screen.Space.RefreshDesks
                                                        ]
                                                )
                                              <|
                                                Mensam.Screen.Space.deskCreate
                                                    { jwt = jwt
                                                    , space = screenModel.space
                                                    , name = name
                                                    }
                                            )

                                        _ ->
                                            update (ReportError errorScreen) <| MkModel model

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.SubmitReservation ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpace screenModel ->
                                    case screenModel.popup of
                                        Just (Mensam.Screen.Space.PopupReservation { desk }) ->
                                            ( MkModel model
                                            , Platform.Cmd.map MessageSpace <| Mensam.Screen.Space.reservationCreate jwt screenModel { desk = { id = desk.id } }
                                            )

                                        _ ->
                                            update (ReportError errorScreen) <| MkModel model

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

        MessageSpace (Mensam.Screen.Space.Messages ms) ->
            case model.screen of
                ScreenSpace _ ->
                    update (Messages <| List.map MessageSpace ms) <| MkModel model

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageSpaceJoin (Mensam.Screen.Space.Join.MessagePure m) ->
            case model.screen of
                ScreenSpaceJoin screenModel ->
                    update EmptyMessage <| MkModel { model | screen = ScreenSpaceJoin <| Mensam.Screen.Space.Join.updatePure m screenModel }

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageSpaceJoin (Mensam.Screen.Space.Join.MessageEffect m) ->
            case m of
                Mensam.Screen.Space.Join.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Screen.Space.Join.RefreshSpace ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceJoin screenModel ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageSpaceJoin <| Mensam.Screen.Space.Join.spaceView jwt screenModel
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Join.SubmitJoin ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceJoin screenModel ->
                                    case screenModel.roleIdSelected of
                                        Nothing ->
                                            update (ReportError errorScreen) <| MkModel model

                                        Just justRoleId ->
                                            ( MkModel model
                                            , Platform.Cmd.map MessageSpaceJoin <|
                                                Mensam.Screen.Space.Join.spaceJoin jwt screenModel.spaceId justRoleId screenModel.password
                                            )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Join.JoinedSuccessfully ->
                    case model.screen of
                        ScreenSpaceJoin screenModel ->
                            update (SetUrl <| RouteSpace screenModel.spaceId) <| MkModel model

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

        MessageSpaceRoles (Mensam.Screen.Space.Roles.MessagePure m) ->
            case model.screen of
                ScreenSpaceRoles screenModel ->
                    update EmptyMessage <| MkModel { model | screen = ScreenSpaceRoles <| Mensam.Screen.Space.Roles.updatePure m screenModel }

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageSpaceRoles (Mensam.Screen.Space.Roles.MessageEffect m) ->
            case m of
                Mensam.Screen.Space.Roles.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Screen.Space.Roles.RefreshRoles ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceRoles screenModel ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageSpaceRoles <|
                                        Mensam.Screen.Space.Roles.spaceView jwt screenModel.spaceId
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Roles.ChooseRole roleId ->
                    case model.screen of
                        ScreenSpaceRoles screenModel ->
                            update (SetUrl <| RouteSpaceRole { spaceId = screenModel.spaceId, roleId = roleId }) <| MkModel model

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Space.Roles.SubmitCreateRole args ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceRoles screenModel ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageSpaceRoles <|
                                        Mensam.Screen.Space.Roles.roleCreate
                                            { jwt = jwt
                                            , space = screenModel.spaceId
                                            , name = args.name
                                            , accessibility = args.accessibility
                                            , password = args.password
                                            , permissions = args.permissions
                                            }
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

        MessageSpaceRoles (Mensam.Screen.Space.Roles.Messages ms) ->
            case model.screen of
                ScreenSpaceRoles _ ->
                    update (Messages <| List.map MessageSpaceRoles ms) <| MkModel model

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageSpaceRole (Mensam.Screen.Space.Role.MessagePure m) ->
            case model.screen of
                ScreenSpaceRole screenModel ->
                    update EmptyMessage <| MkModel { model | screen = ScreenSpaceRole <| Mensam.Screen.Space.Role.updatePure m screenModel }

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageSpaceRole (Mensam.Screen.Space.Role.MessageEffect m) ->
            case m of
                Mensam.Screen.Space.Role.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Screen.Space.Role.RefreshRole ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceRole screenModel ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageSpaceRole <|
                                        Mensam.Screen.Space.Role.spaceView jwt screenModel.space.id
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Role.SubmitEditRole ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceRole screenModel ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageSpaceRole <|
                                        Mensam.Screen.Space.Role.roleEdit
                                            { jwt = jwt
                                            , id = screenModel.role.id
                                            , name = screenModel.new.name
                                            , accessibilityAndPassword = screenModel.new.accessibilityAndPassword
                                            , permissions = screenModel.new.permissions
                                            }
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Role.SubmitDeleteRole { fallback } ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceRole screenModel ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageSpaceRole <|
                                        Mensam.Screen.Space.Role.roleDelete
                                            { jwt = jwt
                                            , id = screenModel.role.id
                                            , fallbackId = fallback
                                            }
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Role.ReturnToRoles ->
                    case model.screen of
                        ScreenSpaceRole screenModel ->
                            update (SetUrl <| RouteSpaceRoles screenModel.space.id) <| MkModel model

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

        MessageSpaceRole (Mensam.Screen.Space.Role.Messages ms) ->
            case model.screen of
                ScreenSpaceRole _ ->
                    update (Messages <| List.map MessageSpaceRole ms) <| MkModel model

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageSpaceSettings (Mensam.Screen.Space.Settings.MessagePure m) ->
            case model.screen of
                ScreenSpaceSettings screenModel ->
                    update EmptyMessage <| MkModel { model | screen = ScreenSpaceSettings <| Mensam.Screen.Space.Settings.updatePure m screenModel }

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageSpaceSettings (Mensam.Screen.Space.Settings.MessageEffect m) ->
            case m of
                Mensam.Screen.Space.Settings.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Screen.Space.Settings.RefreshOldSettings ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceSettings screenModel ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageSpaceSettings <|
                                        Mensam.Screen.Space.Settings.spaceEdit
                                            { jwt = jwt
                                            , id = screenModel.id
                                            , name = Nothing
                                            , timezone = Nothing
                                            , visibility = Nothing
                                            }
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Settings.SubmitSettings ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceSettings screenModel ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageSpaceSettings <|
                                        Mensam.Screen.Space.Settings.spaceEdit
                                            { jwt = jwt
                                            , id = screenModel.id
                                            , name = screenModel.new.name
                                            , timezone = screenModel.new.timezone
                                            , visibility = screenModel.new.visibility
                                            }
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Settings.SubmitDeleteSpace ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceSettings screenModel ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageSpaceSettings <|
                                        Mensam.Screen.Space.Settings.spaceDelete
                                            { jwt = jwt
                                            , id = screenModel.id
                                            }
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Settings.ReturnToSpace ->
                    case model.screen of
                        ScreenSpaceSettings screenModel ->
                            update (SetUrl <| RouteSpace screenModel.id) <| MkModel model

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Space.Settings.ReturnToSpaces ->
                    case model.screen of
                        ScreenSpaceSettings _ ->
                            update (SetUrl RouteSpaces) <| MkModel model

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Space.Settings.OpenPageToRoles ->
                    case model.screen of
                        ScreenSpaceSettings screenModel ->
                            update (SetUrl <| RouteSpaceRoles screenModel.id) <| MkModel model

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

        MessageSpaceSettings (Mensam.Screen.Space.Settings.Messages ms) ->
            case model.screen of
                ScreenSpaceSettings _ ->
                    update (Messages <| List.map MessageSpaceSettings ms) <| MkModel model

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageSpaceUsers (Mensam.Screen.Space.Users.MessagePure m) ->
            case model.screen of
                ScreenSpaceUsers screenModel ->
                    update EmptyMessage <| MkModel { model | screen = ScreenSpaceUsers <| Mensam.Screen.Space.Users.updatePure m screenModel }

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageSpaceUsers (Mensam.Screen.Space.Users.MessageEffect m) ->
            case m of
                Mensam.Screen.Space.Users.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Screen.Space.Users.Refresh ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceUsers screenModel ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageSpaceUsers <|
                                        Mensam.Screen.Space.Users.spaceView jwt screenModel.spaceId
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Users.GetProfile userId ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceUsers _ ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageSpaceUsers <|
                                        Mensam.Screen.Space.Users.profile jwt userId
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Users.SubmitEditUser args ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceUsers screenModel ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageSpaceUsers <|
                                        Mensam.Screen.Space.Users.editUserRole jwt screenModel.spaceId args.user args.role
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

        MessageSpaceUsers (Mensam.Screen.Space.Users.Messages ms) ->
            case model.screen of
                ScreenSpaceUsers _ ->
                    update (Messages <| List.map MessageSpaceUsers ms) <| MkModel model

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageReservations (Mensam.Screen.Reservations.MessagePure m) ->
            case model.screen of
                ScreenReservations screenModel ->
                    update EmptyMessage <| MkModel { model | screen = ScreenReservations <| Mensam.Screen.Reservations.updatePure m screenModel }

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageReservations (Mensam.Screen.Reservations.MessageEffect m) ->
            case m of
                Mensam.Screen.Reservations.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Screen.Reservations.RefreshReservations ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenReservations screenModel ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageReservations <| Mensam.Screen.Reservations.reservationList { jwt = jwt, model = screenModel }
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Reservations.SetDateRange ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenReservations screenModel ->
                                    case screenModel.popup of
                                        Just _ ->
                                            ( MkModel model
                                            , Platform.Cmd.map (\regularM -> Messages [ MessageReservations regularM, MessageReservations <| Mensam.Screen.Reservations.MessagePure Mensam.Screen.Reservations.ClosePopup ]) <|
                                                Mensam.Screen.Reservations.reservationList { jwt = jwt, model = screenModel }
                                            )

                                        _ ->
                                            update (ReportError errorScreen) <| MkModel model

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Reservations.CancelReservation reservationId ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenReservations _ ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageReservations <| Mensam.Screen.Reservations.reservationCancel { jwt = jwt, id = reservationId }
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

        MessageReservations (Mensam.Screen.Reservations.Messages ms) ->
            case model.screen of
                ScreenReservations _ ->
                    update (Messages <| List.map MessageReservations ms) <| MkModel model

                _ ->
                    update (ReportError errorScreen) <| MkModel model


headerMessage : Model -> Mensam.Element.Header.Message -> Message
headerMessage (MkModel model) message =
    case message of
        Mensam.Element.Header.EmptyMessage ->
            EmptyMessage

        Mensam.Element.Header.ClickMensam ->
            case model.authenticated of
                Mensam.Auth.SignedOut ->
                    SetUrl RouteLanding

                Mensam.Auth.SignedIn _ ->
                    SetUrl RouteSpaces

        Mensam.Element.Header.ClickHamburger ->
            if model.viewHamburgerMenu then
                HideHamburgerMenu

            else
                ViewHamburgerMenu

        Mensam.Element.Header.SignIn ->
            SetUrl <| RouteLogin Nothing

        Mensam.Element.Header.SignOut ->
            Auth Logout

        Mensam.Element.Header.YourReservations ->
            SetUrl RouteReservations

        Mensam.Element.Header.ClickErrors ->
            if model.viewErrors then
                HideErrors

            else
                ViewErrors


headerContent : Model -> Mensam.Element.Header.Content
headerContent (MkModel model) =
    { errors = model.errors
    , unfoldErrors = model.viewErrors
    , unfoldHamburgerDropDown = model.viewHamburgerMenu
    , authenticated = model.authenticated
    , title =
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
                Just <| Mensam.Space.nameToString screenModel.name

            ScreenSpaceJoin screenModel ->
                Just <| "Join " ++ Mensam.Space.nameToString screenModel.spaceName

            ScreenSpaceRoles screenModel ->
                Just <| "Roles for " ++ Mensam.Space.nameToString screenModel.spaceName

            ScreenSpaceRole screenModel ->
                Just <| "Role: " ++ Mensam.Space.Role.nameToString screenModel.role.name

            ScreenSpaceUsers screenModel ->
                Just <| "Users of " ++ Mensam.Space.nameToString screenModel.spaceName

            ScreenSpaceSettings screenModel ->
                Just <| "Settings for " ++ Mensam.Space.nameToString screenModel.old.name

            ScreenReservations _ ->
                Just "Your Reservations"
    , httpStatus = model.httpStatus
    }


footerContent : Mensam.Element.Footer.Content
footerContent =
    { sourceUrl = "https://github.com/jumper149/mensam"
    }


view : Model -> Browser.Document Message
view (MkModel model) =
    Mensam.Element.document <|
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ Element.map (headerMessage <| MkModel model) <| Mensam.Element.Header.element <| headerContent <| MkModel model
            , Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.padding 20
                ]
              <|
                case model.screen of
                    ScreenLanding screenModel ->
                        Mensam.Element.screen MessageLanding <| Mensam.Screen.Landing.element screenModel

                    ScreenLogin screenModel ->
                        Mensam.Element.screen MessageLogin <| Mensam.Screen.Login.element screenModel

                    ScreenRegister screenModel ->
                        Mensam.Element.screen MessageRegister <| Mensam.Screen.Register.element screenModel

                    ScreenSpaces screenModel ->
                        Mensam.Element.screen MessageSpaces <| Mensam.Screen.Spaces.element screenModel

                    ScreenSpace screenModel ->
                        Mensam.Element.screen MessageSpace <| Mensam.Screen.Space.element screenModel

                    ScreenSpaceJoin screenModel ->
                        Mensam.Element.screen MessageSpaceJoin <| Mensam.Screen.Space.Join.element screenModel

                    ScreenSpaceRoles screenModel ->
                        Mensam.Element.screen MessageSpaceRoles <| Mensam.Screen.Space.Roles.element screenModel

                    ScreenSpaceRole screenModel ->
                        Mensam.Element.screen MessageSpaceRole <| Mensam.Screen.Space.Role.element screenModel

                    ScreenSpaceUsers screenModel ->
                        Mensam.Element.screen MessageSpaceUsers <| Mensam.Screen.Space.Users.element screenModel

                    ScreenSpaceSettings screenModel ->
                        Mensam.Element.screen MessageSpaceSettings <| Mensam.Screen.Space.Settings.element screenModel

                    ScreenReservations screenModel ->
                        Mensam.Element.screen MessageReservations <| Mensam.Screen.Reservations.element screenModel
            , Mensam.Element.Footer.element <| footerContent
            ]


subscriptions : Model -> Sub Message
subscriptions _ =
    Platform.Sub.batch
        [ Time.every 100 SetTimeNow
        , Http.track "http" (SetHttpStatus << Http.Extra.status)
        ]


errorScreen : Mensam.Error.Error
errorScreen =
    Mensam.Error.message "Can't process a message for the wrong screen" Mensam.Error.undefined


errorNoAuth : Mensam.Error.Error
errorNoAuth =
    Mensam.Error.message "Can't make request without JWT" Mensam.Error.undefined
