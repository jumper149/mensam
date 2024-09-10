module Mensam.Screen.Confirm exposing (..)

import Element
import Element.Font
import Html.Events
import Json.Decode as Decode
import Mensam.Api.Confirm
import Mensam.Auth.Bearer
import Mensam.Element.Button
import Mensam.Element.Screen
import Mensam.Error
import Mensam.User


type alias Model =
    { secret : Mensam.User.ConfirmationSecret
    , popup : ()
    }


init : { secret : Mensam.User.ConfirmationSecret } -> Model
init value =
    { secret = value.secret
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
                            Element.text "Confirm Email"
                        ]
                    , Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = []
                            , color = Mensam.Element.Button.Yellow
                            , label = Element.text "Submit Confirmation"
                            , message = Just <| MessageEffect <| SubmitConfirm model.secret
                            , size = Mensam.Element.Button.Medium
                            }
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
    = ClosePopup


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        ClosePopup ->
            { model | popup = () }


type MessageEffect
    = ReportError Mensam.Error.Error
    | SubmitConfirm Mensam.User.ConfirmationSecret
    | LeaveConfirmationPage


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


confirm : Mensam.Auth.Bearer.Jwt -> Mensam.User.ConfirmationSecret -> Cmd Message
confirm jwt secret =
    Mensam.Api.Confirm.request
        { jwt = jwt
        , secret = secret
        }
    <|
        \response ->
            case response of
                Ok Mensam.Api.Confirm.Success ->
                    Messages
                        [ MessageEffect LeaveConfirmationPage
                        ]

                Ok Mensam.Api.Confirm.ErrorTimedOut ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to confirm email" <|
                                Mensam.Error.message "Timed out" <|
                                    Mensam.Error.message "Try to send a new confirmation email and click the link again" <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.Confirm.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to confirm email" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.Confirm.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to confirm email" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to confirm email" <|
                                Mensam.Error.http error
