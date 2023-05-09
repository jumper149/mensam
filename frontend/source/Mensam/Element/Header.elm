module Mensam.Element.Header exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Html.Attributes
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Error


type alias Content =
    { errors : List Mensam.Error.Error
    , unfoldErrors : Bool
    , authenticated : Bool
    , title : Maybe String
    }


type Message
    = ClickMensam
    | SignIn
    | SignOut
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
            , elementErrors content.errors content.unfoldErrors
            , elementSignInOut content.authenticated
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


elementSignInOut : Bool -> Element.Element Message
elementSignInOut authenticated =
    Element.el
        [ Element.height Element.fill
        , Element.alignRight
        , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
        , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
        , Element.mouseOver [ Element.Background.color <| Element.rgba 1 1 1 0.1 ]
        ]
    <|
        if authenticated then
            Element.el
                [ Element.height Element.fill
                , Element.paddingXY 20 0
                , Element.Events.onClick <| SignOut
                ]
            <|
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                <|
                    Element.text "Sign out"

        else
            Element.el
                [ Element.height Element.fill
                , Element.paddingXY 20 0
                , Element.Events.onClick <| SignIn
                ]
            <|
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                <|
                    Element.text "Sign in"


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
                [ Element.height Element.fill
                , Element.centerX
                , Element.Font.family [ Mensam.Element.Font.sansSerif ]
                , Element.Font.size 17
                , Element.Font.extraLight
                , Element.Font.italic
                , Element.Font.color Mensam.Element.Color.bright.white
                ]
            <|
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                <|
                    Element.text text
