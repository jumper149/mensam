module Mensam.Element.Header exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Element.Input
import Html.Attributes
import Http.Extra
import Mensam.Auth
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Error
import Mensam.User
import Svg
import Svg.Attributes


type alias Content =
    { errors : List Mensam.Error.Error
    , unfoldErrors : Bool
    , unfoldHamburgerDropDown : Bool
    , authenticated : Mensam.Auth.Model
    , title : Maybe String
    , httpStatus : Http.Extra.Status
    }


type Message
    = EmptyMessage
    | ClickMensam
    | ClickHamburger
    | SignIn
    | SignOut
    | YourReservations
    | ClickErrors


element : Content -> Element.Element Message
element content =
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px 60
        , Element.Background.color <| Element.rgba 1 1 1 0.1
        , Element.Font.family [ Mensam.Element.Font.condensed ]
        , Element.Font.light
        , Element.Font.size 17
        , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
        ]
    <|
        Element.row
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.behindContent <| elementTitle content.title
            ]
            [ elementMensam
            , elementStatus content.httpStatus
            , elementErrors content.errors content.unfoldErrors
            , elementHamburger content.unfoldHamburgerDropDown content.authenticated
            ]


elementMensam : Element.Element Message
elementMensam =
    Element.el
        [ Element.height Element.fill
        , Element.paddingXY 20 0
        , Element.alignLeft
        , Element.Font.family [ Mensam.Element.Font.sansSerif ]
        , Element.Font.size 25
        , Element.Font.extraLight
        , Element.Font.italic
        , Element.Font.color Mensam.Element.Color.bright.yellow
        , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
        , Element.mouseOver [ Element.Background.color <| Element.rgba 1 1 1 0.1 ]
        , Element.Events.onClick ClickMensam
        ]
    <|
        Element.el
            [ Element.centerX
            , Element.centerY
            ]
        <|
            Element.text "Mensam"


elementHamburger : Bool -> Mensam.Auth.Model -> Element.Element Message
elementHamburger unfoldDropDownMenu authenticated =
    Element.el
        [ Element.height Element.fill
        , Element.alignRight
        , Element.Background.color <|
            if unfoldDropDownMenu then
                Mensam.Element.Color.bright.black

            else
                Mensam.Element.Color.transparent
        , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
        , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
        , Element.mouseOver [ Element.Background.color <| Element.rgba 1 1 1 0.1 ]
        ]
    <|
        case authenticated of
            Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication authentication) ->
                Element.Input.button
                    [ Element.height Element.fill
                    , Element.paddingXY 20 0
                    , Element.below <|
                        if unfoldDropDownMenu then
                            Element.column
                                [ Element.width <| Element.px 200
                                , Element.htmlAttribute <| Html.Attributes.style "cursor" "default"
                                , Element.htmlAttribute <| Html.Attributes.style "overflow-x" "hidden"
                                , Element.Events.onClick <| EmptyMessage
                                , Element.alignRight
                                , Element.paddingEach
                                    { top = 19
                                    , right = 12
                                    , bottom = 12
                                    , left = 12
                                    }
                                , Element.spacing 10
                                , Element.Background.color Mensam.Element.Color.bright.black
                                , Element.Font.color Mensam.Element.Color.bright.white
                                ]
                            <|
                                [ Element.el
                                    [ Element.paddingEach { top = 0, bottom = 20, left = 15, right = 15 }
                                    , Element.centerX
                                    , Element.centerY
                                    , Element.htmlAttribute <| Html.Attributes.style "text-transform" "none"
                                    ]
                                  <|
                                    Element.text <|
                                        case authentication.user.info of
                                            Nothing ->
                                                ""

                                            Just { name } ->
                                                Mensam.User.nameToString name
                                , Element.el
                                    [ Element.paddingXY 10 10
                                    , Element.centerX
                                    , Element.centerY
                                    , Element.mouseOver [ Element.Background.color <| Element.rgba 1 1 1 0.1 ]
                                    , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                    , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                    , Element.Events.onClick <| YourReservations
                                    ]
                                  <|
                                    Element.text "Your Reservations"
                                , Element.el
                                    [ Element.paddingXY 10 10
                                    , Element.centerX
                                    , Element.centerY
                                    , Element.mouseOver [ Element.Background.color <| Element.rgba 1 1 1 0.1 ]
                                    , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                    , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                    , Element.Events.onClick <| SignOut
                                    ]
                                  <|
                                    Element.text "Sign out"
                                ]

                        else
                            Element.none
                    ]
                    { onPress = Just ClickHamburger
                    , label =
                        Element.column
                            [ Element.centerX
                            , Element.centerY
                            , Element.width <| Element.px 14
                            , Element.spacing 4
                            ]
                            [ Element.el
                                [ Element.width Element.fill
                                , Element.height <| Element.px 1
                                , Element.Background.color Mensam.Element.Color.bright.white
                                ]
                                Element.none
                            , Element.el
                                [ Element.width Element.fill
                                , Element.height <| Element.px 1
                                , Element.Background.color Mensam.Element.Color.bright.white
                                ]
                                Element.none
                            , Element.el
                                [ Element.width Element.fill
                                , Element.height <| Element.px 1
                                , Element.Background.color Mensam.Element.Color.bright.white
                                ]
                                Element.none
                            ]
                    }

            Mensam.Auth.SignedOut ->
                Element.Input.button
                    [ Element.height Element.fill
                    , Element.paddingXY 20 0
                    ]
                    { onPress = Just SignIn
                    , label =
                        Element.el
                            [ Element.centerX
                            , Element.centerY
                            ]
                        <|
                            Element.text "Sign in"
                    }


elementErrors : List Mensam.Error.Error -> Bool -> Element.Element Message
elementErrors errors unfoldErrors =
    case errors of
        [] ->
            Element.none

        _ ->
            Element.el
                [ Element.height Element.fill
                , Element.paddingXY 20 0
                , Element.alignRight
                , Element.Background.color <|
                    if unfoldErrors then
                        Mensam.Element.Color.bright.black

                    else
                        Element.rgba 1 1 1 0
                , Element.Font.color <|
                    if unfoldErrors then
                        Mensam.Element.Color.dark.red

                    else
                        Mensam.Element.Color.bright.red
                , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                , Element.mouseOver
                    [ Element.Background.color <|
                        if unfoldErrors then
                            Mensam.Element.Color.bright.white

                        else
                            Element.rgba 1 1 1 0.1
                    ]
                , Element.Events.onClick ClickErrors
                , Element.below <|
                    if unfoldErrors then
                        Element.column
                            [ Element.width <| Element.px 200
                            , Element.htmlAttribute <| Html.Attributes.style "overflow-x" "hidden"
                            , Element.alignRight
                            , Element.paddingEach
                                { top = 19
                                , right = 12
                                , bottom = 12
                                , left = 12
                                }
                            , Element.spacing 10
                            , Element.Background.color Mensam.Element.Color.bright.black
                            , Element.Font.color Mensam.Element.Color.bright.white
                            , Element.mouseOver
                                [ Element.Background.color <| Mensam.Element.Color.bright.white
                                , Element.Font.color <| Mensam.Element.Color.dark.black
                                ]
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
                    Element.text "!"


elementTitle : Maybe String -> Element.Element msg
elementTitle maybeTitle =
    case maybeTitle of
        Nothing ->
            Element.none

        Just text ->
            Element.el
                [ Element.width <| Element.px 140
                , Element.height Element.fill
                , Element.centerX
                , Element.Font.family [ Mensam.Element.Font.sansSerif ]
                , Element.Font.size 17
                , Element.Font.extraLight
                , Element.Font.italic
                , Element.Font.color Mensam.Element.Color.bright.white
                , Element.clip
                ]
            <|
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                <|
                    Element.text text


elementStatus : Http.Extra.Status -> Element.Element msg
elementStatus status =
    case status of
        Http.Extra.Done ->
            Element.none

        Http.Extra.Loading ->
            Element.el
                [ Element.width <| Element.px 30
                , Element.spacingXY 5 0
                ]
            <|
                Element.html <|
                    Svg.svg
                        [ Svg.Attributes.viewBox "0 0 30 20"
                        , Svg.Attributes.width "100%"
                        ]
                    <|
                        [ Svg.circle
                            [ Svg.Attributes.cy "10"
                            , Svg.Attributes.r "2"
                            , Svg.Attributes.fill "rgba(255,255,255,0.4)"
                            ]
                            [ Svg.animate
                                [ Svg.Attributes.attributeName "cx"
                                , Svg.Attributes.values "10;20;10"
                                , Svg.Attributes.dur "1s"
                                , Svg.Attributes.repeatCount "indefinite"
                                ]
                                []
                            ]
                        ]
