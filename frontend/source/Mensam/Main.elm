module Mensam.Main exposing (..)

import Browser
import Browser.Navigation
import Element
import Element.Events.Pointer
import Element.Font
import Html.Attributes
import Http
import Http.Extra
import Json.Encode as Encode
import Mensam.Api.Login
import Mensam.Api.Logout
import Mensam.Api.Profile
import Mensam.Application
import Mensam.Auth
import Mensam.Auth.Basic
import Mensam.Auth.Bearer
import Mensam.Clipboard
import Mensam.Element
import Mensam.Element.Dropdown
import Mensam.Element.Font
import Mensam.Element.Footer
import Mensam.Element.Header
import Mensam.Error
import Mensam.Error.Incorporation
import Mensam.Flags
import Mensam.Screen.Confirm
import Mensam.Screen.Dashboard
import Mensam.Screen.Landing
import Mensam.Screen.Login
import Mensam.Screen.PrivacyPolicy
import Mensam.Screen.Profile
import Mensam.Screen.Register
import Mensam.Screen.Reservations
import Mensam.Screen.Space
import Mensam.Screen.Space.Desks
import Mensam.Screen.Space.Join
import Mensam.Screen.Space.Role
import Mensam.Screen.Space.Roles
import Mensam.Screen.Space.Settings
import Mensam.Screen.Space.Users
import Mensam.Screen.Spaces
import Mensam.Screen.TermsAndConditions
import Mensam.Screen.UserSettings
import Mensam.Space
import Mensam.Space.Role
import Mensam.Storage
import Mensam.Time
import Mensam.Tracker
import Mensam.Url
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
        , baseUrl : Mensam.Url.BaseUrl
        , screen : Screen
        , screenRequestTracker : Mensam.Tracker.State
        , authenticated : Mensam.Auth.Model
        , dialogToSignIn : Maybe Mensam.Screen.Login.Model
        , errors : Mensam.Error.Incorporation.IncorporatedErrors
        , viewErrors : Bool
        , viewHamburgerMenu : Bool
        , time :
            { now : Time.Posix
            , zone : Mensam.Time.Timezone
            }
        , httpStatus : Http.Extra.Status
        }


type Screen
    = ScreenLanding Mensam.Screen.Landing.Model
    | ScreenRegister Mensam.Screen.Register.Model
    | ScreenLogin Mensam.Screen.Login.Model
    | ScreenTermsAndConditions Mensam.Screen.TermsAndConditions.Model
    | ScreenPrivacyPolicy Mensam.Screen.PrivacyPolicy.Model
    | ScreenDashboard Mensam.Screen.Dashboard.Model
    | ScreenSpaces Mensam.Screen.Spaces.Model
    | ScreenSpace Mensam.Screen.Space.Model
    | ScreenSpaceJoin Mensam.Screen.Space.Join.Model
    | ScreenSpaceRoles Mensam.Screen.Space.Roles.Model
    | ScreenSpaceRole Mensam.Screen.Space.Role.Model
    | ScreenSpaceUsers Mensam.Screen.Space.Users.Model
    | ScreenSpaceSettings Mensam.Screen.Space.Settings.Model
    | ScreenSpaceDesks Mensam.Screen.Space.Desks.Model
    | ScreenReservations Mensam.Screen.Reservations.Model
    | ScreenProfile Mensam.Screen.Profile.Model
    | ScreenUserSettings Mensam.Screen.UserSettings.Model
    | ScreenConfirm Mensam.Screen.Confirm.Model


type Route
    = RouteLanding
    | RouteLogin (Maybe Mensam.Screen.Login.Model)
    | RouteRegister
    | RouteTermsAndConditions
    | RoutePrivacyPolicy
    | RouteDashboard
    | RouteSpaces
    | RouteSpace Mensam.Space.Identifier
    | RouteSpaceJoin { spaceId : Mensam.Space.Identifier, roleId : Maybe Mensam.Space.Role.Identifier, password : Maybe String }
    | RouteSpaceRoles Mensam.Space.Identifier
    | RouteSpaceRole { spaceId : Mensam.Space.Identifier, roleId : Mensam.Space.Role.Identifier }
    | RouteSpaceUsers Mensam.Space.Identifier
    | RouteSpaceSettings Mensam.Space.Identifier
    | RouteSpaceDesks Mensam.Space.Identifier
    | RouteReservations
    | RouteProfile Mensam.User.Identifier
    | RouteUserSettings
    | RouteConfirm Mensam.User.ConfirmationSecret


routeToUrl : Mensam.Url.BaseUrl -> Route -> String
routeToUrl baseUrl route =
    case route of
        RouteLanding ->
            Mensam.Url.absolute baseUrl [] []

        RouteLogin _ ->
            Mensam.Url.absolute baseUrl [ "login" ] []

        RouteRegister ->
            Mensam.Url.absolute baseUrl [ "register" ] []

        RouteTermsAndConditions ->
            Mensam.Url.absolute baseUrl [ "terms" ] []

        RoutePrivacyPolicy ->
            Mensam.Url.absolute baseUrl [ "privacy" ] []

        RouteDashboard ->
            Mensam.Url.absolute baseUrl [ "dashboard" ] []

        RouteSpaces ->
            Mensam.Url.absolute baseUrl [ "spaces" ] []

        RouteSpace identifier ->
            Mensam.Url.absolute baseUrl [ "space", Mensam.Space.identifierToString identifier ] []

        RouteSpaceJoin { spaceId, roleId, password } ->
            Mensam.Url.absolute baseUrl
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
            Mensam.Url.absolute baseUrl [ "roles", "space", Mensam.Space.identifierToString identifier ] []

        RouteSpaceRole { spaceId, roleId } ->
            Mensam.Url.absolute baseUrl [ "role", Mensam.Space.Role.identifierToString roleId, "space", Mensam.Space.identifierToString spaceId ] []

        RouteSpaceUsers identifier ->
            Mensam.Url.absolute baseUrl [ "users", "space", Mensam.Space.identifierToString identifier ] []

        RouteSpaceSettings identifier ->
            Mensam.Url.absolute baseUrl [ "settings", "space", Mensam.Space.identifierToString identifier ] []

        RouteSpaceDesks identifier ->
            Mensam.Url.absolute baseUrl [ "desks", "space", Mensam.Space.identifierToString identifier ] []

        RouteReservations ->
            Mensam.Url.absolute baseUrl [ "reservations" ] []

        RouteProfile identifier ->
            Mensam.Url.absolute baseUrl [ "profile", Mensam.User.identifierToString identifier ] []

        RouteUserSettings ->
            Mensam.Url.absolute baseUrl [ "user", "settings" ] []

        RouteConfirm (Mensam.User.MkConfirmationSecret secret) ->
            Mensam.Url.absolute baseUrl [ "register", "confirm", secret ] []


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

        RouteTermsAndConditions ->
            update EmptyMessage <| MkModel { model | screen = ScreenTermsAndConditions Mensam.Screen.TermsAndConditions.init }

        RoutePrivacyPolicy ->
            update EmptyMessage <| MkModel { model | screen = ScreenPrivacyPolicy Mensam.Screen.PrivacyPolicy.init }

        RouteDashboard ->
            update
                (Messages
                    [ MessageDashboard <| Mensam.Screen.Dashboard.MessageEffect Mensam.Screen.Dashboard.RefreshSpaces
                    , MessageDashboard <| Mensam.Screen.Dashboard.MessageEffect Mensam.Screen.Dashboard.RefreshReservations
                    ]
                )
            <|
                MkModel { model | screen = ScreenDashboard <| Mensam.Screen.Dashboard.init { time = { now = model.time.now, zone = model.time.zone } } }

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
                    , MessageSpaceSettings <| Mensam.Screen.Space.Settings.MessageEffect Mensam.Screen.Space.Settings.DownloadSpacePictureRequest
                    ]
                )
            <|
                MkModel { model | screen = ScreenSpaceSettings <| Mensam.Screen.Space.Settings.init model.baseUrl { id = identifier } }

        RouteSpaceDesks identifier ->
            update
                (Messages
                    [ MessageSpaceDesks <| Mensam.Screen.Space.Desks.MessageEffect Mensam.Screen.Space.Desks.RefreshSpace
                    , MessageSpaceDesks <| Mensam.Screen.Space.Desks.MessageEffect Mensam.Screen.Space.Desks.RefreshDesks
                    ]
                )
            <|
                MkModel { model | screen = ScreenSpaceDesks <| Mensam.Screen.Space.Desks.init { id = identifier } }

        RouteReservations ->
            update (MessageReservations <| Mensam.Screen.Reservations.MessageEffect Mensam.Screen.Reservations.RefreshReservations) <|
                MkModel { model | screen = ScreenReservations <| Mensam.Screen.Reservations.init { time = { now = model.time.now, zone = model.time.zone } } }

        RouteProfile identifier ->
            update
                (Messages
                    [ MessageProfile <| Mensam.Screen.Profile.MessageEffect Mensam.Screen.Profile.Refresh
                    , MessageProfile <| Mensam.Screen.Profile.MessageEffect Mensam.Screen.Profile.RefreshProfilePicture
                    ]
                )
            <|
                MkModel
                    { model
                        | screen =
                            ScreenProfile <|
                                Mensam.Screen.Profile.init model.baseUrl
                                    { id = identifier
                                    , self =
                                        case model.authenticated of
                                            Mensam.Auth.SignedOut ->
                                                False

                                            Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication auth) ->
                                                if auth.user.id == identifier then
                                                    True

                                                else
                                                    False
                                    }
                    }

        RouteUserSettings ->
            case model.authenticated of
                Mensam.Auth.SignedOut ->
                    update (SetUrl RouteLanding) <| MkModel model

                Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication authentication) ->
                    update
                        (Messages
                            [ MessageUserSettings <| Mensam.Screen.UserSettings.MessageEffect Mensam.Screen.UserSettings.Refresh
                            , MessageUserSettings <| Mensam.Screen.UserSettings.MessageEffect Mensam.Screen.UserSettings.DownloadProfilePictureRequest
                            ]
                        )
                    <|
                        MkModel { model | screen = ScreenUserSettings <| Mensam.Screen.UserSettings.init model.baseUrl { id = authentication.user.id } }

        RouteConfirm secret ->
            case model.authenticated of
                Mensam.Auth.SignedOut ->
                    update (SetUrl <| RouteLogin Nothing) <| MkModel model

                Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication _) ->
                    update (Messages []) <|
                        MkModel { model | screen = ScreenConfirm <| Mensam.Screen.Confirm.init { secret = secret } }


urlParser : Mensam.Url.BaseUrl -> Url.Parser.Parser (Route -> c) c
urlParser baseUrl =
    Url.Parser.oneOf
        [ Url.Parser.map RouteLanding <| Mensam.Url.parsePrefix baseUrl </> Url.Parser.top
        , Url.Parser.map (RouteLogin Nothing) <| Mensam.Url.parsePrefix baseUrl </> Url.Parser.s "login"
        , Url.Parser.map RouteConfirm <| Mensam.Url.parsePrefix baseUrl </> Url.Parser.s "register" </> Url.Parser.s "confirm" </> Url.Parser.map Mensam.User.MkConfirmationSecret Url.Parser.string
        , Url.Parser.map RouteRegister <| Mensam.Url.parsePrefix baseUrl </> Url.Parser.s "register"
        , Url.Parser.map RouteTermsAndConditions <| Mensam.Url.parsePrefix baseUrl </> Url.Parser.s "terms"
        , Url.Parser.map RoutePrivacyPolicy <| Mensam.Url.parsePrefix baseUrl </> Url.Parser.s "privacy"
        , Url.Parser.map RouteDashboard <| Mensam.Url.parsePrefix baseUrl </> Url.Parser.s "dashboard"
        , Url.Parser.map RouteReservations <| Mensam.Url.parsePrefix baseUrl </> Url.Parser.s "reservations"
        , Url.Parser.map RouteSpaces <| Mensam.Url.parsePrefix baseUrl </> Url.Parser.s "spaces"
        , Url.Parser.map RouteSpace <| Mensam.Url.parsePrefix baseUrl </> Url.Parser.s "space" </> Url.Parser.map Mensam.Space.MkIdentifier Url.Parser.int
        , Url.Parser.map
            (\spaceId roleId password ->
                RouteSpaceJoin
                    { spaceId = Mensam.Space.MkIdentifier spaceId
                    , roleId = Maybe.map Mensam.Space.Role.MkIdentifier roleId
                    , password = password
                    }
            )
          <|
            Mensam.Url.parsePrefix baseUrl
                </> Url.Parser.s "join"
                </> Url.Parser.s "space"
                </> Url.Parser.int
                <?> Url.Parser.Query.int "role"
                <?> Url.Parser.Query.string "password"
        , Url.Parser.map RouteSpaceRoles <| Mensam.Url.parsePrefix baseUrl </> Url.Parser.s "roles" </> Url.Parser.s "space" </> Url.Parser.map Mensam.Space.MkIdentifier Url.Parser.int
        , Url.Parser.map
            (\roleId spaceId ->
                RouteSpaceRole
                    { spaceId = spaceId
                    , roleId = roleId
                    }
            )
          <|
            Mensam.Url.parsePrefix baseUrl
                </> Url.Parser.s "role"
                </> Url.Parser.map Mensam.Space.Role.MkIdentifier Url.Parser.int
                </> Url.Parser.s "space"
                </> Url.Parser.map Mensam.Space.MkIdentifier Url.Parser.int
        , Url.Parser.map RouteSpaceUsers <| Mensam.Url.parsePrefix baseUrl </> Url.Parser.s "users" </> Url.Parser.s "space" </> Url.Parser.map Mensam.Space.MkIdentifier Url.Parser.int
        , Url.Parser.map RouteSpaceSettings <| Mensam.Url.parsePrefix baseUrl </> Url.Parser.s "settings" </> Url.Parser.s "space" </> Url.Parser.map Mensam.Space.MkIdentifier Url.Parser.int
        , Url.Parser.map RouteSpaceDesks <| Mensam.Url.parsePrefix baseUrl </> Url.Parser.s "desks" </> Url.Parser.s "space" </> Url.Parser.map Mensam.Space.MkIdentifier Url.Parser.int
        , Url.Parser.map RouteProfile <| Mensam.Url.parsePrefix baseUrl </> Url.Parser.s "profile" </> Url.Parser.map Mensam.User.MkIdentifierUnsafe Url.Parser.int
        , Url.Parser.map RouteUserSettings <| Mensam.Url.parsePrefix baseUrl </> Url.Parser.s "user" </> Url.Parser.s "settings"
        ]


init : Mensam.Flags.FlagsRaw -> Url.Url -> Browser.Navigation.Key -> ( Model, Platform.Cmd.Cmd Message )
init flagsRaw url navigationKey =
    let
        modelInit =
            MkModel
                { navigationKey = navigationKey
                , baseUrl = Mensam.Url.mockUnsafe
                , screen = ScreenLanding Mensam.Screen.Landing.init
                , screenRequestTracker = Mensam.Tracker.init
                , authenticated = Mensam.Auth.SignedOut
                , dialogToSignIn = Nothing
                , errors = Mensam.Error.Incorporation.init
                , viewErrors = False
                , viewHamburgerMenu = False
                , time =
                    { now = Time.millisToPosix 0
                    , zone = Mensam.Time.timezoneEtcUtc
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
                                , baseUrl = flags.baseUrl
                            }

                    Err error ->
                        MkModel
                            { model
                                | errors = Mensam.Error.Incorporation.incorporate model.time.now error model.errors
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
    | OpenDialogToSignIn
    | CloseDialogToSignIn
    | MessageLoginPopup Mensam.Screen.Login.Message
    | MessageLanding Mensam.Screen.Landing.Message
    | MessageRegister Mensam.Screen.Register.Message
    | MessageTermsAndConditions Mensam.Screen.TermsAndConditions.Message
    | MessagePrivacyPolicy Mensam.Screen.PrivacyPolicy.Message
    | MessageLogin Mensam.Screen.Login.Message
    | MessageDashboard Mensam.Screen.Dashboard.Message
    | MessageSpaces Mensam.Screen.Spaces.Message
    | MessageSpace Mensam.Screen.Space.Message
    | MessageSpaceJoin Mensam.Screen.Space.Join.Message
    | MessageSpaceRoles Mensam.Screen.Space.Roles.Message
    | MessageSpaceRole Mensam.Screen.Space.Role.Message
    | MessageSpaceUsers Mensam.Screen.Space.Users.Message
    | MessageSpaceSettings Mensam.Screen.Space.Settings.Message
    | MessageSpaceDesks Mensam.Screen.Space.Desks.Message
    | MessageReservations Mensam.Screen.Reservations.Message
    | MessageProfile Mensam.Screen.Profile.Message
    | MessageUserSettings Mensam.Screen.UserSettings.Message
    | MessageConfirm Mensam.Screen.Confirm.Message
    | RefreshScreen


type MessageAuth
    = SetSession { jwt : Mensam.Auth.Bearer.Jwt, expiration : Maybe Time.Posix, id : Mensam.User.Identifier }
    | UnsetSession
    | Login { username : Mensam.User.Name, password : Mensam.User.Password }
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
                    case Url.Parser.parse (urlParser model.baseUrl) url of
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
            case Url.Parser.parse (urlParser model.baseUrl) url of
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
                            case Url.Parser.parse (urlParser model.baseUrl) url of
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
                                                            RouteDashboard

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
                    [ Raw <| \m -> ( m, Browser.Navigation.pushUrl model.navigationKey <| routeToUrl model.baseUrl route )
                    ]
                )
            <|
                MkModel model

        SetModel route ->
            routeToModelUpdate route (MkModel model)

        ReportError error ->
            if
                Mensam.Error.isSubError (Mensam.Auth.Bearer.error Mensam.Auth.Bearer.ErrorIndefinite) error
                    || Mensam.Error.isSubError errorNoAuth error
            then
                update (Messages [ Auth UnsetSession, OpenDialogToSignIn ]) <| MkModel model

            else
                update EmptyMessage <| MkModel { model | errors = Mensam.Error.Incorporation.incorporate model.time.now error model.errors }

        ClearErrors ->
            update HideErrors <| MkModel { model | errors = Mensam.Error.Incorporation.init }

        ViewErrors ->
            update EmptyMessage <| MkModel { model | viewErrors = True }

        HideErrors ->
            update EmptyMessage <| MkModel { model | viewErrors = False }

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
                    , dialogToSignIn = Nothing
                }
            , Mensam.Storage.set <| Mensam.Storage.MkStorage session
            )

        Auth UnsetSession ->
            ( MkModel { model | authenticated = Mensam.Auth.SignedOut }, Mensam.Storage.unset )

        Auth (Login { username, password }) ->
            ( MkModel model
            , Mensam.Api.Login.request Nothing
                model.baseUrl
                (Mensam.Api.Login.BasicAuth <|
                    Mensam.Auth.Basic.MkCredentials
                        { username = Mensam.User.nameToString username
                        , password = Mensam.User.passwordToString password
                        }
                )
              <|
                \result ->
                    case result of
                        Ok (Mensam.Api.Login.Success value) ->
                            Messages
                                [ Auth <| SetSession value
                                , Auth <| RefreshUserInfo
                                , SetUrl RouteDashboard
                                ]

                        Ok (Mensam.Api.Login.ErrorAuth Mensam.Auth.Basic.ErrorUsername) ->
                            ReportError <|
                                Mensam.Error.message "Login failed" <|
                                    Mensam.Error.message "Authentication" <|
                                        Mensam.Error.message "Wrong username" Mensam.Error.undefined

                        Ok (Mensam.Api.Login.ErrorAuth Mensam.Auth.Basic.ErrorPassword) ->
                            ReportError <|
                                Mensam.Error.message "Login failed" <|
                                    Mensam.Error.message "Authentication" <|
                                        Mensam.Error.message "Wrong password" Mensam.Error.undefined

                        Ok (Mensam.Api.Login.ErrorAuth Mensam.Auth.Basic.ErrorIndefinite) ->
                            ReportError <|
                                Mensam.Error.message "Login failed" <|
                                    Mensam.Error.message "Authentication" <|
                                        Mensam.Error.message "Indefinite" Mensam.Error.undefined

                        Err error ->
                            ReportError <|
                                Mensam.Error.message "Login failed" <|
                                    Mensam.Error.http error
            )

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
                                    , Mensam.Api.Logout.request Nothing model.baseUrl { jwt = jwt } <|
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
            let
                f openDialog =
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            if openDialog then
                                update OpenDialogToSignIn <| MkModel model

                            else
                                update EmptyMessage <| MkModel model

                        Mensam.Auth.SignedIn authentication ->
                            if Mensam.Auth.isExpired authentication now then
                                if openDialog then
                                    update (Messages [ Auth UnsetSession, OpenDialogToSignIn ]) <| MkModel model

                                else
                                    update (Auth UnsetSession) <| MkModel model

                            else
                                update EmptyMessage <| MkModel model
            in
            case model.screen of
                ScreenLanding _ ->
                    f False

                ScreenRegister _ ->
                    f False

                ScreenLogin _ ->
                    f False

                ScreenTermsAndConditions _ ->
                    f False

                ScreenPrivacyPolicy _ ->
                    f False

                ScreenDashboard _ ->
                    f True

                ScreenSpaces _ ->
                    f True

                ScreenSpace _ ->
                    f True

                ScreenSpaceJoin _ ->
                    f True

                ScreenSpaceRoles _ ->
                    f True

                ScreenSpaceRole _ ->
                    f True

                ScreenSpaceUsers _ ->
                    f True

                ScreenSpaceSettings _ ->
                    f True

                ScreenSpaceDesks _ ->
                    f True

                ScreenReservations _ ->
                    f True

                ScreenProfile _ ->
                    f True

                ScreenUserSettings _ ->
                    f True

                ScreenConfirm _ ->
                    f True

        Auth RefreshUserInfo ->
            case model.authenticated of
                Mensam.Auth.SignedOut ->
                    update EmptyMessage <| MkModel model

                Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication authentication) ->
                    ( MkModel model
                    , Mensam.Api.Profile.request Nothing
                        model.baseUrl
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

        OpenDialogToSignIn ->
            case model.dialogToSignIn of
                Nothing ->
                    update EmptyMessage <| MkModel { model | dialogToSignIn = Just <| Mensam.Screen.Login.init }

                Just _ ->
                    update EmptyMessage <| MkModel model

        CloseDialogToSignIn ->
            update EmptyMessage <| MkModel { model | dialogToSignIn = Nothing }

        MessageLoginPopup (Mensam.Screen.Login.MessagePure m) ->
            case model.dialogToSignIn of
                Nothing ->
                    update (ReportError <| Mensam.Error.undefined) <| MkModel model

                Just signInModel ->
                    update EmptyMessage <| MkModel { model | dialogToSignIn = Just <| Mensam.Screen.Login.updatePure m signInModel }

        MessageLoginPopup (Mensam.Screen.Login.MessageEffect m) ->
            case m of
                Mensam.Screen.Login.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Screen.Login.SubmitLogin ->
                    case model.dialogToSignIn of
                        Nothing ->
                            update (ReportError <| Mensam.Error.undefined) <| MkModel model

                        Just signInModel ->
                            ( MkModel model
                            , Platform.Cmd.map MessageLoginPopup <| Mensam.Screen.Login.login Nothing model.baseUrl signInModel
                            )

                Mensam.Screen.Login.Register ->
                    update (SetUrl RouteRegister) <| MkModel model

                Mensam.Screen.Login.SetSession session ->
                    update
                        (Messages
                            [ Auth <| SetSession session
                            , Auth <| RefreshUserInfo
                            , CloseDialogToSignIn
                            , RefreshScreen
                            ]
                        )
                    <|
                        MkModel model

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

                Mensam.Screen.Register.Submit args ->
                    case model.screen of
                        ScreenRegister _ ->
                            let
                                ( trackerState, request ) =
                                    Mensam.Tracker.register model.screenRequestTracker <|
                                        \tracker -> Mensam.Screen.Register.register (Just tracker) model.baseUrl args
                            in
                            ( MkModel { model | screenRequestTracker = trackerState }
                            , Platform.Cmd.map MessageRegister request
                            )

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Register.Submitted { username, password, emailSent } ->
                    case model.screen of
                        ScreenRegister _ ->
                            update
                                (Messages
                                    [ if emailSent then
                                        EmptyMessage

                                      else
                                        ReportError <| Mensam.Error.message "Failed to send confirmation email" Mensam.Error.undefined
                                    , Auth (Login { username = username, password = password })
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
                            let
                                ( trackerState, request ) =
                                    Mensam.Tracker.register model.screenRequestTracker <|
                                        \tracker -> Mensam.Screen.Login.login (Just tracker) model.baseUrl screenModel
                            in
                            ( MkModel { model | screenRequestTracker = trackerState }
                            , Platform.Cmd.map MessageLogin request
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
                            , SetUrl RouteDashboard
                            ]
                        )
                    <|
                        MkModel model

        MessageTermsAndConditions (Mensam.Screen.TermsAndConditions.MessageEffect m) ->
            case m of
                Mensam.Screen.TermsAndConditions.AbsurdMessage absurd ->
                    never absurd

        MessagePrivacyPolicy (Mensam.Screen.PrivacyPolicy.MessageEffect m) ->
            case m of
                Mensam.Screen.PrivacyPolicy.AbsurdMessage absurd ->
                    never absurd

        MessageDashboard (Mensam.Screen.Dashboard.MessagePure m) ->
            case model.screen of
                ScreenDashboard screenModel ->
                    update EmptyMessage <|
                        MkModel
                            { model
                                | screen =
                                    ScreenDashboard <|
                                        Mensam.Screen.Dashboard.updatePure m
                                            { timezone = model.time.zone
                                            , username =
                                                case model.authenticated of
                                                    Mensam.Auth.SignedOut ->
                                                        Nothing

                                                    Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication auth) ->
                                                        Maybe.map .name auth.user.info
                                            }
                                            screenModel
                            }

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageDashboard (Mensam.Screen.Dashboard.MessageEffect m) ->
            case m of
                Mensam.Screen.Dashboard.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Screen.Dashboard.RefreshSpaces ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenDashboard _ ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Dashboard.spaceList (Just tracker) model.baseUrl jwt
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageDashboard request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Dashboard.RefreshOwnerName args ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenDashboard _ ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Dashboard.ownerGetName (Just tracker) model.baseUrl { jwt = jwt, space = args.space, owner = args.owner }
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageDashboard request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Dashboard.RefreshSpacePicture space ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenDashboard _ ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Dashboard.downloadSpacePicture (Just tracker) model.baseUrl jwt space
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageDashboard request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Dashboard.ChooseSpace identifier ->
                    update (SetUrl <| RouteSpace identifier) <| MkModel model

                Mensam.Screen.Dashboard.RefreshReservations ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenDashboard screenModel ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Dashboard.reservationList (Just tracker) model.baseUrl { jwt = jwt, model = screenModel }
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageDashboard request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Dashboard.CancelReservation reservationId ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenDashboard _ ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Dashboard.reservationCancel (Just tracker) model.baseUrl { jwt = jwt, id = reservationId }
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageDashboard request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Dashboard.OpenPageToBrowseSpaces ->
                    update (SetUrl RouteSpaces) <| MkModel model

                Mensam.Screen.Dashboard.OpenPageToViewReservations ->
                    update (SetUrl RouteReservations) <| MkModel model

                Mensam.Screen.Dashboard.SubmitCreateFirstSpace spaceInfo ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenDashboard _ ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker ->
                                                    Mensam.Screen.Dashboard.spaceCreate (Just tracker)
                                                        model.baseUrl
                                                        { jwt = jwt
                                                        , name = spaceInfo.name
                                                        , timezone = spaceInfo.timezone
                                                        , visibility =
                                                            if spaceInfo.visible then
                                                                Mensam.Space.MkVisibilityVisible

                                                            else
                                                                Mensam.Space.MkVisibilityHidden
                                                        }
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageDashboard request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

        MessageDashboard (Mensam.Screen.Dashboard.Messages ms) ->
            case model.screen of
                ScreenDashboard _ ->
                    update (Messages <| List.map MessageDashboard ms) <| MkModel model

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageSpaces (Mensam.Screen.Spaces.MessagePure m) ->
            case model.screen of
                ScreenSpaces screenModel ->
                    update EmptyMessage <| MkModel { model | screen = ScreenSpaces <| Mensam.Screen.Spaces.updatePure m { timezone = model.time.zone } screenModel }

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
                            let
                                ( trackerState, request ) =
                                    Mensam.Tracker.register model.screenRequestTracker <|
                                        \tracker -> Mensam.Screen.Spaces.spaceList (Just tracker) model.baseUrl jwt
                            in
                            ( MkModel { model | screenRequestTracker = trackerState }
                            , Platform.Cmd.map MessageSpaces request
                            )

                Mensam.Screen.Spaces.SubmitCreate formData ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            let
                                ( trackerState, request ) =
                                    Mensam.Tracker.register model.screenRequestTracker <|
                                        \tracker ->
                                            Mensam.Screen.Spaces.spaceCreate (Just tracker)
                                                model.baseUrl
                                                { jwt = jwt
                                                , timezone = formData.timezone
                                                , name = formData.name
                                                , visibility =
                                                    if formData.visible then
                                                        Mensam.Space.MkVisibilityVisible

                                                    else
                                                        Mensam.Space.MkVisibilityHidden
                                                }
                            in
                            ( MkModel { model | screenRequestTracker = trackerState }
                            , Platform.Cmd.map
                                (\msg ->
                                    Messages
                                        -- TODO: Does this make sense with cancelled requests?
                                        [ MessageSpaces msg
                                        , MessageSpaces <| Mensam.Screen.Spaces.MessageEffect Mensam.Screen.Spaces.RefreshSpaces
                                        ]
                                )
                                request
                            )

                Mensam.Screen.Spaces.ChooseSpace identifier ->
                    update (SetUrl <| RouteSpace identifier) <| MkModel model

        MessageSpaces (Mensam.Screen.Spaces.Messages ms) ->
            case model.screen of
                ScreenSpaces _ ->
                    update (Messages <| List.map MessageSpaces ms) <| MkModel model

                _ ->
                    update (ReportError errorScreen) <| MkModel model

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

                Mensam.Screen.Space.GetSelectorRegionWidth elementId ->
                    case model.screen of
                        ScreenSpace _ ->
                            ( MkModel model
                            , Cmd.map MessageSpace <| Mensam.Screen.Space.getSelectorRegionDimensionsCmd elementId
                            )

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Space.RefreshSpace ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt, user }) ->
                            case model.screen of
                                ScreenSpace screenModel ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Space.spaceView (Just tracker) model.baseUrl { jwt = jwt, yourUserId = user.id } screenModel
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpace request
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
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Space.deskList (Just tracker) model.baseUrl jwt screenModel
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpace request
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

                Mensam.Screen.Space.OpenPageToDesks ->
                    case model.screen of
                        ScreenSpace screenModel ->
                            update (SetUrl <| RouteSpaceDesks screenModel.space) <| MkModel model

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Space.SubmitLeave ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpace screenModel ->
                                    case screenModel.popup of
                                        Just Mensam.Screen.Space.PopupLeave ->
                                            let
                                                ( trackerState, request ) =
                                                    Mensam.Tracker.register model.screenRequestTracker <|
                                                        \tracker -> Mensam.Screen.Space.spaceLeave (Just tracker) model.baseUrl jwt screenModel.space
                                            in
                                            ( MkModel { model | screenRequestTracker = trackerState }
                                              -- TODO: Does this work well with cancelled requests?
                                            , Platform.Cmd.map
                                                (\msg ->
                                                    Messages
                                                        [ MessageSpace msg
                                                        , MessageSpace <| Mensam.Screen.Space.MessageEffect Mensam.Screen.Space.RefreshSpace
                                                        ]
                                                )
                                                request
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
                                            let
                                                ( trackerState, request ) =
                                                    Mensam.Tracker.register model.screenRequestTracker <|
                                                        \tracker -> Mensam.Screen.Space.reservationCreate (Just tracker) model.baseUrl jwt screenModel { desk = { id = desk.id } }
                                            in
                                            ( MkModel { model | screenRequestTracker = trackerState }
                                            , Platform.Cmd.map MessageSpace request
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
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt, user }) ->
                            case model.screen of
                                ScreenSpaceJoin screenModel ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Space.Join.spaceView (Just tracker) model.baseUrl { jwt = jwt, yourUserId = user.id } screenModel
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceJoin request
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
                                            let
                                                ( trackerState, request ) =
                                                    Mensam.Tracker.register model.screenRequestTracker <|
                                                        \tracker -> Mensam.Screen.Space.Join.spaceJoin (Just tracker) model.baseUrl jwt screenModel.spaceId justRoleId screenModel.password
                                            in
                                            ( MkModel { model | screenRequestTracker = trackerState }
                                            , Platform.Cmd.map MessageSpaceJoin request
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

        MessageSpaceJoin (Mensam.Screen.Space.Join.Messages ms) ->
            case model.screen of
                ScreenSpaceJoin _ ->
                    update (Messages <| List.map MessageSpaceJoin ms) <| MkModel model

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
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt, user }) ->
                            case model.screen of
                                ScreenSpaceRoles screenModel ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Space.Roles.spaceView (Just tracker) model.baseUrl { jwt = jwt, yourUserId = user.id } screenModel.spaceId
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceRoles request
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
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker ->
                                                    Mensam.Screen.Space.Roles.roleCreate (Just tracker)
                                                        model.baseUrl
                                                        { jwt = jwt
                                                        , space = screenModel.spaceId
                                                        , name = args.name
                                                        , accessibility = args.accessibility
                                                        , password = args.password
                                                        , permissions = args.permissions
                                                        }
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceRoles request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Roles.ReturnToSpaceSettings ->
                    case model.screen of
                        ScreenSpaceRoles screenModel ->
                            update (SetUrl <| RouteSpaceSettings screenModel.spaceId) <| MkModel model

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

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
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt, user }) ->
                            case model.screen of
                                ScreenSpaceRole screenModel ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker ->
                                                    Mensam.Screen.Space.Role.spaceView (Just tracker) model.baseUrl { jwt = jwt, yourUserId = user.id } screenModel.space.id
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceRole request
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
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker ->
                                                    Mensam.Screen.Space.Role.roleEdit (Just tracker)
                                                        model.baseUrl
                                                        { jwt = jwt
                                                        , id = screenModel.role.id
                                                        , name = screenModel.new.name
                                                        , accessibilityAndPassword = screenModel.new.accessibilityAndPassword
                                                        , permissions = screenModel.new.permissions
                                                        }
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceRole request
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
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker ->
                                                    Mensam.Screen.Space.Role.roleDelete (Just tracker)
                                                        model.baseUrl
                                                        { jwt = jwt
                                                        , id = screenModel.role.id
                                                        , fallbackId = fallback
                                                        }
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceRole request
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
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker ->
                                                    Mensam.Screen.Space.Settings.spaceEdit (Just tracker)
                                                        model.baseUrl
                                                        { jwt = jwt
                                                        , id = screenModel.id
                                                        , name = Nothing
                                                        , timezone = Nothing
                                                        , visibility = Nothing
                                                        }
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceSettings request
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
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker ->
                                                    Mensam.Screen.Space.Settings.spaceEdit (Just tracker)
                                                        model.baseUrl
                                                        { jwt = jwt
                                                        , id = screenModel.id
                                                        , name = screenModel.new.name
                                                        , timezone = Maybe.map .selected screenModel.new.timezone
                                                        , visibility = screenModel.new.visibility
                                                        }
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceSettings request
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
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker ->
                                                    Mensam.Screen.Space.Settings.spaceDelete (Just tracker)
                                                        model.baseUrl
                                                        { jwt = jwt
                                                        , id = screenModel.id
                                                        }
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceSettings request
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

                Mensam.Screen.Space.Settings.DownloadSpacePictureRequest ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceSettings screenModel ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Space.Settings.downloadSpacePicture (Just tracker) model.baseUrl jwt screenModel.id
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceSettings request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Space.Settings.UploadSpacePictureRequested ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication _) ->
                            case model.screen of
                                ScreenSpaceSettings _ ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageSpaceSettings <| Mensam.Screen.Space.Settings.selectSpacePictureToUpload
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Space.Settings.UploadSpacePictureUpload file ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceSettings screenModel ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Space.Settings.uploadSpacePicture (Just tracker) model.baseUrl jwt screenModel.id file
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceSettings request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Space.Settings.DeleteSpacePictureRequest ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceSettings screenModel ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Space.Settings.deleteSpacePicture (Just tracker) model.baseUrl jwt screenModel.id
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceSettings request
                                    )

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

        MessageSpaceDesks (Mensam.Screen.Space.Desks.MessagePure m) ->
            case model.screen of
                ScreenSpaceDesks screenModel ->
                    update EmptyMessage <| MkModel { model | screen = ScreenSpaceDesks <| Mensam.Screen.Space.Desks.updatePure m screenModel }

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageSpaceDesks (Mensam.Screen.Space.Desks.MessageEffect m) ->
            case m of
                Mensam.Screen.Space.Desks.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Screen.Space.Desks.RefreshSpace ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt, user }) ->
                            case model.screen of
                                ScreenSpaceDesks screenModel ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Space.Desks.spaceView (Just tracker) model.baseUrl { jwt = jwt, yourUserId = user.id } screenModel.spaceId
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceDesks request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Desks.RefreshDesks ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceDesks screenModel ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Space.Desks.listDesks (Just tracker) model.baseUrl jwt screenModel.spaceId
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceDesks request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Desks.SubmitCreateDesk args ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceDesks screenModel ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker ->
                                                    Mensam.Screen.Space.Desks.createDesk (Just tracker)
                                                        model.baseUrl
                                                        { jwt = jwt
                                                        , space = screenModel.spaceId
                                                        , name = args.name
                                                        , location = Nothing -- TODO: Should we set the location on creation?
                                                        }
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceDesks request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Desks.SubmitDeleteDesk args ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceDesks _ ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker ->
                                                    Mensam.Screen.Space.Desks.deleteDesk (Just tracker)
                                                        model.baseUrl
                                                        { jwt = jwt
                                                        , id = args.id
                                                        }
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceDesks request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Desks.SubmitEditDesk args ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceDesks _ ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker ->
                                                    Mensam.Screen.Space.Desks.editDesk (Just tracker)
                                                        model.baseUrl
                                                        { jwt = jwt
                                                        , id = args.id
                                                        , name = args.name
                                                        , location = args.location
                                                        }
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceDesks request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Desks.ReturnToSpace ->
                    case model.screen of
                        ScreenSpaceDesks screenModel ->
                            update (SetUrl <| RouteSpace screenModel.spaceId) <| MkModel model

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

        MessageSpaceDesks (Mensam.Screen.Space.Desks.Messages ms) ->
            case model.screen of
                ScreenSpaceDesks _ ->
                    update (Messages <| List.map MessageSpaceDesks ms) <| MkModel model

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageSpaceUsers (Mensam.Screen.Space.Users.MessagePure m) ->
            case model.screen of
                ScreenSpaceUsers screenModel ->
                    update EmptyMessage <| MkModel { model | screen = ScreenSpaceUsers <| Mensam.Screen.Space.Users.updatePure model.baseUrl m screenModel }

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageSpaceUsers (Mensam.Screen.Space.Users.MessageEffect m) ->
            case m of
                Mensam.Screen.Space.Users.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Screen.Space.Users.Refresh ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt, user }) ->
                            case model.screen of
                                ScreenSpaceUsers screenModel ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Space.Users.spaceView (Just tracker) model.baseUrl { jwt = jwt, yourUserId = user.id } screenModel.spaceId
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceUsers request
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
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Space.Users.profile (Just tracker) model.baseUrl jwt userId
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceUsers request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Users.GetProfilePicture userId ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceUsers _ ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Space.Users.profilePicture (Just tracker) model.baseUrl jwt userId
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceUsers request
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
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Space.Users.editUserRole (Just tracker) model.baseUrl jwt screenModel.spaceId args.user args.role
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceUsers request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Users.SubmitKickUser args ->
                    case model.authenticated of
                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenSpaceUsers screenModel ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Space.Users.kickUser (Just tracker) model.baseUrl jwt screenModel.spaceId args.user
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageSpaceUsers request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                Mensam.Screen.Space.Users.CopyUrlToClipboard content ->
                    case model.screen of
                        ScreenSpaceUsers _ ->
                            ( MkModel model
                            , Mensam.Clipboard.copyText content
                            )

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Space.Users.ReturnToSpace ->
                    case model.screen of
                        ScreenSpaceUsers screenModel ->
                            update (SetUrl <| RouteSpace screenModel.spaceId) <| MkModel model

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Space.Users.OpenPageToProfile userId ->
                    case model.screen of
                        ScreenSpaceUsers _ ->
                            update (SetUrl <| RouteProfile userId) <| MkModel model

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Space.Users.OpenPageSpaceRoles spaceId ->
                    case model.screen of
                        ScreenSpaceUsers _ ->
                            update (SetUrl <| RouteSpaceRoles spaceId) <| MkModel model

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Space.Users.DownloadQRCodePng input ->
                    case model.screen of
                        ScreenSpaceUsers _ ->
                            ( MkModel model
                            , Mensam.Screen.Space.Users.downloadQRCode input
                            )

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

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
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Reservations.reservationList (Just tracker) model.baseUrl { jwt = jwt, model = screenModel }
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageReservations request
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
                                            let
                                                ( trackerState, request ) =
                                                    Mensam.Tracker.register model.screenRequestTracker <|
                                                        \tracker -> Mensam.Screen.Reservations.reservationList (Just tracker) model.baseUrl { jwt = jwt, model = screenModel }
                                            in
                                            ( MkModel { model | screenRequestTracker = trackerState }
                                              -- TODO: Does this work well with cancelled requests?
                                            , Platform.Cmd.map
                                                (\regularM ->
                                                    Messages
                                                        [ MessageReservations regularM
                                                        , MessageReservations <| Mensam.Screen.Reservations.MessagePure Mensam.Screen.Reservations.ClosePopup
                                                        ]
                                                )
                                                request
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
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Reservations.reservationCancel (Just tracker) model.baseUrl { jwt = jwt, id = reservationId }
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageReservations request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

        MessageReservations (Mensam.Screen.Reservations.Messages ms) ->
            case model.screen of
                ScreenReservations _ ->
                    update (Messages <| List.map MessageReservations ms) <| MkModel model

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageProfile (Mensam.Screen.Profile.MessagePure m) ->
            case model.screen of
                ScreenProfile screenModel ->
                    update EmptyMessage <| MkModel { model | screen = ScreenProfile <| Mensam.Screen.Profile.updatePure m screenModel }

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageProfile (Mensam.Screen.Profile.MessageEffect m) ->
            case m of
                Mensam.Screen.Profile.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Screen.Profile.Refresh ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenProfile screenModel ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Profile.profile (Just tracker) model.baseUrl jwt screenModel.id
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageProfile request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Profile.RefreshProfilePicture ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenProfile screenModel ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Profile.downloadProfilePicture (Just tracker) model.baseUrl jwt screenModel.id
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageProfile request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Profile.OpenPageUserSettings ->
                    update (SetUrl RouteUserSettings) <| MkModel model

        MessageProfile (Mensam.Screen.Profile.Messages ms) ->
            case model.screen of
                ScreenProfile _ ->
                    update (Messages <| List.map MessageProfile ms) <| MkModel model

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageUserSettings (Mensam.Screen.UserSettings.MessagePure m) ->
            case model.screen of
                ScreenUserSettings screenModel ->
                    update EmptyMessage <| MkModel { model | screen = ScreenUserSettings <| Mensam.Screen.UserSettings.updatePure m screenModel }

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageUserSettings (Mensam.Screen.UserSettings.MessageEffect m) ->
            case m of
                Mensam.Screen.UserSettings.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Screen.UserSettings.Refresh ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenUserSettings screenModel ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.UserSettings.profile (Just tracker) model.baseUrl jwt screenModel.id
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageUserSettings request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.UserSettings.SubmitNewPassword submitData ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenUserSettings _ ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker ->
                                                    Mensam.Screen.UserSettings.changePassword (Just tracker)
                                                        model.baseUrl
                                                        { jwt = jwt
                                                        , newPassword = submitData.newPassword
                                                        }
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageUserSettings request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.UserSettings.SubmitConfirmationRequest ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenUserSettings _ ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.UserSettings.confirmationRequest (Just tracker) model.baseUrl { jwt = jwt }
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageUserSettings request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.UserSettings.RefreshNotificationPreferences ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenUserSettings _ ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.UserSettings.setNotificationPreferences (Just tracker) model.baseUrl { jwt = jwt, receiveEmailNotifications = Nothing }
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageUserSettings request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.UserSettings.SubmitNotificationPreferences notificationPreferences ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenUserSettings _ ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.UserSettings.setNotificationPreferences (Just tracker) model.baseUrl { jwt = jwt, receiveEmailNotifications = Just notificationPreferences.receiveEmailNotifications }
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageUserSettings request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.UserSettings.DownloadProfilePictureRequest ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenUserSettings screenModel ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.UserSettings.downloadProfilePicture (Just tracker) model.baseUrl jwt screenModel.id
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageUserSettings request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.UserSettings.UploadProfilePictureRequested ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication _) ->
                            case model.screen of
                                ScreenUserSettings _ ->
                                    ( MkModel model
                                    , Platform.Cmd.map MessageUserSettings <| Mensam.Screen.UserSettings.selectProfilePictureToUpload
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.UserSettings.UploadProfilePictureUpload file ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenUserSettings _ ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.UserSettings.uploadProfilePicture (Just tracker) model.baseUrl jwt file
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageUserSettings request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.UserSettings.DeleteProfilePictureRequest ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenUserSettings _ ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.UserSettings.deleteProfilePicture (Just tracker) model.baseUrl jwt
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageUserSettings request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.UserSettings.OpenPageUserProfile user ->
                    update (SetUrl <| RouteProfile user.id) <| MkModel model

        MessageUserSettings (Mensam.Screen.UserSettings.Messages ms) ->
            case model.screen of
                ScreenUserSettings _ ->
                    update (Messages <| List.map MessageUserSettings ms) <| MkModel model

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageConfirm (Mensam.Screen.Confirm.MessagePure m) ->
            case model.screen of
                ScreenConfirm screenModel ->
                    update EmptyMessage <| MkModel { model | screen = ScreenConfirm <| Mensam.Screen.Confirm.updatePure m screenModel }

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        MessageConfirm (Mensam.Screen.Confirm.MessageEffect m) ->
            case m of
                Mensam.Screen.Confirm.ReportError err ->
                    update (ReportError err) <| MkModel model

                Mensam.Screen.Confirm.SubmitConfirm secret ->
                    case model.authenticated of
                        Mensam.Auth.SignedOut ->
                            update (ReportError errorNoAuth) <| MkModel model

                        Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication { jwt }) ->
                            case model.screen of
                                ScreenConfirm _ ->
                                    let
                                        ( trackerState, request ) =
                                            Mensam.Tracker.register model.screenRequestTracker <|
                                                \tracker -> Mensam.Screen.Confirm.confirm (Just tracker) model.baseUrl jwt secret
                                    in
                                    ( MkModel { model | screenRequestTracker = trackerState }
                                    , Platform.Cmd.map MessageConfirm request
                                    )

                                _ ->
                                    update (ReportError errorScreen) <| MkModel model

                Mensam.Screen.Confirm.LeaveConfirmationPage ->
                    case model.screen of
                        ScreenConfirm _ ->
                            update (SetUrl RouteDashboard) <| MkModel model

                        _ ->
                            update (ReportError errorScreen) <| MkModel model

        MessageConfirm (Mensam.Screen.Confirm.Messages ms) ->
            case model.screen of
                ScreenConfirm _ ->
                    update (Messages <| List.map MessageConfirm ms) <| MkModel model

                _ ->
                    update (ReportError errorScreen) <| MkModel model

        RefreshScreen ->
            case model.screen of
                ScreenLanding _ ->
                    routeToModelUpdate RouteLanding <| MkModel model

                ScreenLogin _ ->
                    routeToModelUpdate (RouteLogin Nothing) <| MkModel model

                ScreenRegister _ ->
                    routeToModelUpdate RouteRegister <| MkModel model

                ScreenTermsAndConditions _ ->
                    routeToModelUpdate RouteTermsAndConditions <| MkModel model

                ScreenPrivacyPolicy _ ->
                    routeToModelUpdate RoutePrivacyPolicy <| MkModel model

                ScreenDashboard _ ->
                    routeToModelUpdate RouteDashboard <| MkModel model

                ScreenSpaces _ ->
                    routeToModelUpdate RouteSpaces <| MkModel model

                ScreenSpace m ->
                    routeToModelUpdate (RouteSpace m.space) <| MkModel model

                ScreenSpaceJoin m ->
                    routeToModelUpdate (RouteSpaceJoin { spaceId = m.spaceId, roleId = m.roleIdSelected, password = m.password }) <| MkModel model

                ScreenSpaceRoles m ->
                    routeToModelUpdate (RouteSpaceRoles m.spaceId) <| MkModel model

                ScreenSpaceRole m ->
                    routeToModelUpdate (RouteSpaceRole { spaceId = m.space.id, roleId = m.role.id }) <| MkModel model

                ScreenSpaceUsers m ->
                    routeToModelUpdate (RouteSpaceUsers m.spaceId) <| MkModel model

                ScreenSpaceSettings m ->
                    routeToModelUpdate (RouteSpaceSettings m.id) <| MkModel model

                ScreenSpaceDesks m ->
                    routeToModelUpdate (RouteSpaceDesks m.spaceId) <| MkModel model

                ScreenReservations _ ->
                    routeToModelUpdate RouteReservations <| MkModel model

                ScreenProfile m ->
                    routeToModelUpdate (RouteProfile m.id) <| MkModel model

                ScreenUserSettings _ ->
                    routeToModelUpdate RouteUserSettings <| MkModel model

                ScreenConfirm m ->
                    routeToModelUpdate (RouteConfirm m.secret) <| MkModel model


headerMessage : Model -> Mensam.Element.Header.Message -> Message
headerMessage (MkModel model) message =
    case message of
        Mensam.Element.Header.ClickMensam ->
            case model.authenticated of
                Mensam.Auth.SignedOut ->
                    SetUrl RouteLanding

                Mensam.Auth.SignedIn _ ->
                    SetUrl RouteDashboard

        Mensam.Element.Header.ClickHamburger ->
            if model.viewHamburgerMenu then
                HideHamburgerMenu

            else
                ViewHamburgerMenu

        Mensam.Element.Header.ClickOutsideOfHamburger ->
            HideHamburgerMenu

        Mensam.Element.Header.SignIn ->
            SetUrl <| RouteLogin Nothing

        Mensam.Element.Header.ClickErrors ->
            if model.viewErrors then
                HideErrors

            else
                ViewErrors

        Mensam.Element.Header.ClearErrors ->
            ClearErrors


headerContent : Model -> Mensam.Element.Header.Content
headerContent (MkModel model) =
    { errors =
        let
            calculateTime err =
                { error = err.error
                , time = Mensam.Time.fromPosix model.time.zone err.time
                }
        in
        List.map calculateTime <| Mensam.Error.Incorporation.select Nothing model.errors
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

            ScreenTermsAndConditions _ ->
                Just "Terms and Conditions"

            ScreenPrivacyPolicy _ ->
                Just "Privacy Policy"

            ScreenDashboard _ ->
                Just "Your Dashboard"

            ScreenSpaces _ ->
                Just "Spaces"

            ScreenSpace screenModel ->
                Just <| Mensam.Space.nameToString screenModel.name

            ScreenSpaceJoin screenModel ->
                Just <| Mensam.Space.nameToString screenModel.spaceName

            ScreenSpaceRoles screenModel ->
                Just <| Mensam.Space.nameToString screenModel.spaceName

            ScreenSpaceRole screenModel ->
                Just <| Mensam.Space.Role.nameToString screenModel.role.name

            ScreenSpaceUsers screenModel ->
                Just <| Mensam.Space.nameToString screenModel.spaceName

            ScreenSpaceSettings screenModel ->
                Just <| Mensam.Space.nameToString screenModel.old.name

            ScreenSpaceDesks screenModel ->
                Just <| Mensam.Space.nameToString screenModel.spaceName

            ScreenReservations _ ->
                Just "Your Reservations"

            ScreenProfile screenModel ->
                Just <| Mensam.User.nameToString screenModel.name

            ScreenUserSettings _ ->
                Just "Your Settings"

            ScreenConfirm _ ->
                Just "Confirm Email"
    , httpStatus = model.httpStatus
    }


footerMessage : Model -> Mensam.Element.Footer.Message -> Message
footerMessage (MkModel _) message =
    case message of
        Mensam.Element.Footer.ClickedSomewhere ->
            HideHamburgerMenu


dropdownMessage : Model -> Mensam.Element.Dropdown.Message -> Message
dropdownMessage (MkModel _) message =
    case message of
        Mensam.Element.Dropdown.CloseDropdown ->
            HideHamburgerMenu

        Mensam.Element.Dropdown.YourProfile id ->
            Messages
                [ HideHamburgerMenu
                , SetUrl <| RouteProfile id
                ]

        Mensam.Element.Dropdown.YourDashboard ->
            Messages
                [ HideHamburgerMenu
                , SetUrl RouteDashboard
                ]

        Mensam.Element.Dropdown.YourReservations ->
            Messages
                [ HideHamburgerMenu
                , SetUrl RouteReservations
                ]

        Mensam.Element.Dropdown.UserSettings ->
            Messages
                [ HideHamburgerMenu
                , SetUrl RouteUserSettings
                ]

        Mensam.Element.Dropdown.SignOut ->
            Messages
                [ HideHamburgerMenu
                , Auth Logout
                ]


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
                , Element.padding 12
                , Element.inFront <|
                    Element.map (dropdownMessage <| MkModel model) <|
                        Mensam.Element.Dropdown.element
                            { unfoldDropdownMenu = model.viewHamburgerMenu
                            , authenticated = model.authenticated
                            }
                ]
              <|
                let
                    maybeSignInPopup =
                        case model.screen of
                            ScreenLanding _ ->
                                Nothing

                            ScreenLogin _ ->
                                Nothing

                            ScreenRegister _ ->
                                Nothing

                            ScreenTermsAndConditions _ ->
                                Nothing

                            ScreenPrivacyPolicy _ ->
                                Nothing

                            ScreenDashboard _ ->
                                Maybe.map (Element.map MessageLoginPopup << Mensam.Screen.Login.element) model.dialogToSignIn

                            ScreenSpaces _ ->
                                Maybe.map (Element.map MessageLoginPopup << Mensam.Screen.Login.element) model.dialogToSignIn

                            ScreenSpace _ ->
                                Maybe.map (Element.map MessageLoginPopup << Mensam.Screen.Login.element) model.dialogToSignIn

                            ScreenSpaceJoin _ ->
                                Maybe.map (Element.map MessageLoginPopup << Mensam.Screen.Login.element) model.dialogToSignIn

                            ScreenSpaceRoles _ ->
                                Maybe.map (Element.map MessageLoginPopup << Mensam.Screen.Login.element) model.dialogToSignIn

                            ScreenSpaceRole _ ->
                                Maybe.map (Element.map MessageLoginPopup << Mensam.Screen.Login.element) model.dialogToSignIn

                            ScreenSpaceUsers _ ->
                                Maybe.map (Element.map MessageLoginPopup << Mensam.Screen.Login.element) model.dialogToSignIn

                            ScreenSpaceSettings _ ->
                                Maybe.map (Element.map MessageLoginPopup << Mensam.Screen.Login.element) model.dialogToSignIn

                            ScreenSpaceDesks _ ->
                                Maybe.map (Element.map MessageLoginPopup << Mensam.Screen.Login.element) model.dialogToSignIn

                            ScreenReservations _ ->
                                Maybe.map (Element.map MessageLoginPopup << Mensam.Screen.Login.element) model.dialogToSignIn

                            ScreenProfile _ ->
                                Maybe.map (Element.map MessageLoginPopup << Mensam.Screen.Login.element) model.dialogToSignIn

                            ScreenUserSettings _ ->
                                Maybe.map (Element.map MessageLoginPopup << Mensam.Screen.Login.element) model.dialogToSignIn

                            ScreenConfirm _ ->
                                Maybe.map (Element.map MessageLoginPopup << Mensam.Screen.Login.element) model.dialogToSignIn
                in
                Element.el
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.Font.size 16
                    , Element.Font.family [ Mensam.Element.Font.sansSerif ]
                    , Element.inFront <|
                        case maybeSignInPopup of
                            Nothing ->
                                Element.none

                            Just el ->
                                Element.column
                                    [ Element.width Element.fill
                                    , Element.height Element.fill
                                    , Element.padding 0
                                    , Element.spacing 0
                                    ]
                                    [ Element.row
                                        [ Element.width Element.fill
                                        , Element.height <| Element.px 170
                                        , Element.alignTop
                                        , Element.Events.Pointer.onClick <| \_ -> EmptyMessage
                                        ]
                                        []
                                    , Element.row
                                        [ Element.width Element.fill
                                        , Element.padding 0
                                        , Element.spacing 0
                                        ]
                                        [ Element.column
                                            [ Element.width Element.fill
                                            , Element.height Element.fill
                                            , Element.alignLeft
                                            , Element.Events.Pointer.onClick <| \_ -> EmptyMessage
                                            ]
                                            []
                                        , el
                                        , Element.column
                                            [ Element.width Element.fill
                                            , Element.height Element.fill
                                            , Element.alignRight
                                            , Element.Events.Pointer.onClick <| \_ -> EmptyMessage
                                            ]
                                            []
                                        ]
                                    , Element.row
                                        [ Element.width Element.fill
                                        , Element.height Element.fill
                                        , Element.alignBottom
                                        , Element.Events.Pointer.onClick <| \_ -> EmptyMessage
                                        ]
                                        []
                                    ]
                    ]
                <|
                    Element.el
                        ([ Element.width Element.fill
                         , Element.height Element.fill
                         ]
                            ++ (case maybeSignInPopup of
                                    Nothing ->
                                        []

                                    Just _ ->
                                        [ Element.htmlAttribute <| Html.Attributes.style "filter" "blur(2px)"
                                        ]
                               )
                        )
                    <|
                        case model.screen of
                            ScreenLanding screenModel ->
                                Mensam.Element.screen MessageLanding <| Mensam.Screen.Landing.element screenModel

                            ScreenLogin screenModel ->
                                Mensam.Element.screen MessageLogin <| Mensam.Screen.Login.element screenModel

                            ScreenRegister screenModel ->
                                Mensam.Element.screen MessageRegister <| Mensam.Screen.Register.element screenModel

                            ScreenTermsAndConditions screenModel ->
                                Mensam.Element.screen MessageTermsAndConditions <| Mensam.Screen.TermsAndConditions.element screenModel

                            ScreenPrivacyPolicy screenModel ->
                                Mensam.Element.screen MessagePrivacyPolicy <| Mensam.Screen.PrivacyPolicy.element screenModel

                            ScreenDashboard screenModel ->
                                Mensam.Element.screen MessageDashboard <| Mensam.Screen.Dashboard.element model.baseUrl screenModel

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
                                Mensam.Element.screen MessageSpaceUsers <| Mensam.Screen.Space.Users.element model.baseUrl screenModel

                            ScreenSpaceSettings screenModel ->
                                Mensam.Element.screen MessageSpaceSettings <| Mensam.Screen.Space.Settings.element screenModel

                            ScreenSpaceDesks screenModel ->
                                Mensam.Element.screen MessageSpaceDesks <| Mensam.Screen.Space.Desks.element screenModel

                            ScreenReservations screenModel ->
                                Mensam.Element.screen MessageReservations <| Mensam.Screen.Reservations.element screenModel

                            ScreenProfile screenModel ->
                                Mensam.Element.screen MessageProfile <| Mensam.Screen.Profile.element screenModel

                            ScreenUserSettings screenModel ->
                                Mensam.Element.screen MessageUserSettings <| Mensam.Screen.UserSettings.element screenModel

                            ScreenConfirm screenModel ->
                                Mensam.Element.screen MessageConfirm <| Mensam.Screen.Confirm.element screenModel
            , Element.map (footerMessage <| MkModel model) <| Mensam.Element.Footer.element
            ]


subscriptions : Model -> Sub Message
subscriptions _ =
    Platform.Sub.batch
        [ Time.every 100 <| \time -> Messages [ SetTimeNow time, Auth <| CheckExpirationExplicit time ]
        , Http.track "http" (SetHttpStatus << Http.Extra.status)
        ]


errorScreen : Mensam.Error.Error
errorScreen =
    Mensam.Error.message "Can't process a message for the wrong screen" Mensam.Error.undefined


errorNoAuth : Mensam.Error.Error
errorNoAuth =
    Mensam.Error.message "Can't make request without JWT" Mensam.Error.undefined
