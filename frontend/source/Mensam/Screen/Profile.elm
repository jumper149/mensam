module Mensam.Screen.Profile exposing (..)

import Element
import Element.Border
import Element.Font
import Mensam.Api.PictureDownload
import Mensam.Api.Profile
import Mensam.Auth.Bearer
import Mensam.Element.Button
import Mensam.Element.Screen
import Mensam.Error
import Mensam.Http.Tracker
import Mensam.Url
import Mensam.User


type alias Model =
    { self : Bool
    , id : Mensam.User.Identifier
    , name : Mensam.User.Name
    , profilePictureUrl : String
    , email : Maybe Mensam.User.Email
    , emailVerified : Bool
    , popup : ()
    }


init : Mensam.Url.BaseUrl -> { self : Bool, id : Mensam.User.Identifier } -> Model
init baseUrl value =
    { self = value.self
    , id = value.id
    , name = Mensam.User.MkNameUnsafe ""
    , profilePictureUrl =
        Mensam.Url.absolute baseUrl
            [ "static"
            , "default-profile-picture.jpeg"
            ]
            []
    , email = Nothing
    , emailVerified = False
    , popup = ()
    }


element : Model -> Element.Element Message
element model =
    Mensam.Element.Screen.element
        { main =
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing 10
                ]
                [ Element.column
                    [ Element.width Element.fill
                    , Element.height <| Element.fillPortion 1
                    ]
                    [ Element.row
                        [ Element.width Element.fill
                        , Element.height <| Element.px 45
                        , Element.padding 10
                        , Element.spacing 30
                        ]
                        [ Element.el
                            [ Element.Font.size 22
                            , Element.Font.hairline
                            , Element.alignBottom
                            , Element.alignLeft
                            ]
                          <|
                            Element.text "User Profile"
                        , if model.self then
                            Mensam.Element.Button.button <|
                                Mensam.Element.Button.MkButton
                                    { attributes = [ Element.alignRight ]
                                    , color = Mensam.Element.Button.Gray
                                    , enabled = True
                                    , label = Element.text "Settings"
                                    , message = Just <| MessageEffect OpenPageUserSettings
                                    , size = Mensam.Element.Button.Small
                                    }

                          else
                            Element.none
                        ]
                    , Element.el [ Element.padding 10 ] <|
                        Element.image
                            [ Element.width <| Element.px 180
                            , Element.height <| Element.px 180
                            , Element.Border.rounded 30
                            , Element.clip
                            ]
                            { src = model.profilePictureUrl
                            , description = "Profile picture."
                            }
                    , Element.row
                        [ Element.width Element.fill
                        , Element.height <| Element.px 45
                        , Element.padding 10
                        , Element.spacing 20
                        ]
                        [ Element.el
                            [ Element.alignLeft
                            ]
                          <|
                            Element.text "User:"
                        , Element.el [] <| Element.text <| Mensam.User.nameToString model.name
                        ]
                    , Element.row
                        [ Element.width Element.fill
                        , Element.height <| Element.px 45
                        , Element.padding 10
                        , Element.spacing 20
                        ]
                        [ Element.el
                            [ Element.alignLeft
                            ]
                          <|
                            Element.text "Email:"
                        , Element.el [] <|
                            Element.text <|
                                let
                                    verifiedText =
                                        if model.emailVerified then
                                            " (verified)"

                                        else
                                            ""
                                in
                                case model.email of
                                    Nothing ->
                                        "hidden" ++ verifiedText

                                    Just email ->
                                        Mensam.User.emailToString email ++ verifiedText
                        ]
                    ]
                ]
        , popup = Nothing
        , closePopup = MessagePure ClosePopup
        }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect
    | Messages (List Message)


type MessagePure
    = SetName Mensam.User.Name
    | SetProfilePicture { url : String }
    | SetEmail (Maybe Mensam.User.Email)
    | SetEmailVerified Bool
    | ClosePopup


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetName name ->
            { model | name = name }

        SetProfilePicture picture ->
            { model | profilePictureUrl = picture.url }

        SetEmail email ->
            { model | email = email }

        SetEmailVerified verified ->
            { model | emailVerified = verified }

        ClosePopup ->
            { model | popup = () }


type MessageEffect
    = ReportError Mensam.Error.Error
    | Refresh
    | RefreshProfilePicture
    | OpenPageUserSettings


profile : Maybe Mensam.Http.Tracker.Tracker -> Mensam.Url.BaseUrl -> Mensam.Auth.Bearer.Jwt -> Mensam.User.Identifier -> Cmd Message
profile tracker baseUrl jwt userId =
    Mensam.Api.Profile.request tracker
        baseUrl
        { jwt = jwt
        , id = userId
        }
    <|
        \response ->
            case response of
                Ok (Mensam.Api.Profile.Success body) ->
                    Messages
                        [ MessagePure <| SetName body.name
                        , MessagePure <| SetEmail body.email
                        , MessagePure <| SetEmailVerified body.emailVerified
                        ]

                Ok Mensam.Api.Profile.ErrorUnknownUser ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Unknown user while requesting information" <|
                                Mensam.Error.undefined

                Ok (Mensam.Api.Profile.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to request profile" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.Profile.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to request profile" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to request profile" <|
                                Mensam.Error.http error


downloadProfilePicture : Maybe Mensam.Http.Tracker.Tracker -> Mensam.Url.BaseUrl -> Mensam.Auth.Bearer.Jwt -> Mensam.User.Identifier -> Cmd Message
downloadProfilePicture tracker baseUrl jwt user =
    Mensam.Api.PictureDownload.request tracker
        baseUrl
        { jwt = jwt
        , user = user
        }
    <|
        \response ->
            case response of
                Ok (Mensam.Api.PictureDownload.Success picture) ->
                    MessagePure <| SetProfilePicture picture

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to download profile picture" <|
                                Mensam.Error.http error
