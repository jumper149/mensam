module Mensam.Element.Dropdown exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Element.Input
import Mensam.Auth
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.User


type alias Content =
    { unfoldDropdownMenu : Bool
    , authenticated : Mensam.Auth.Model
    }


type Message
    = CloseDropdown
    | YourProfile Mensam.User.Identifier
    | YourDashboard
    | YourReservations
    | SignOut


element : Content -> Element.Element Message
element content =
    dropdownWrapper content <|
        case content.authenticated of
            Mensam.Auth.SignedIn (Mensam.Auth.MkAuthentication authentication) ->
                Element.column
                    ([ Element.width Element.fill
                     , Element.height Element.fill
                     , Element.Font.size 18
                     ]
                        ++ (Mensam.Element.Font.font <|
                                Mensam.Element.Font.Condensed
                                    { weight = Mensam.Element.Font.Light300
                                    , italic = False
                                    }
                           )
                    )
                    [ Element.Input.button
                        [ Element.paddingXY 0 20
                        , Element.width Element.fill
                        , Element.alignTop
                        , Element.Font.italic
                        , Element.mouseOver
                            [ Element.Font.color <| Mensam.Element.Color.bright.yellow
                            , Element.Font.shadow
                                { offset = ( 0, 0 )
                                , blur = 50
                                , color = Mensam.Element.Color.bright.yellow
                                }
                            ]
                        ]
                        { onPress = Just <| YourProfile authentication.user.id
                        , label =
                            Element.el
                                [ Element.centerX
                                ]
                            <|
                                Element.text <|
                                    case authentication.user.info of
                                        Nothing ->
                                            ""

                                        Just { name } ->
                                            Mensam.User.nameToString name
                        }
                    , Element.column
                        [ Element.height Element.fill
                        , Element.width Element.fill
                        , Element.Font.size 16
                        ]
                        [ dropdownEntry
                            { attributes = []
                            , text = "Your Dashboard"
                            , message = YourDashboard
                            }
                        , dropdownEntry
                            { attributes = []
                            , text = "Your Reservations"
                            , message = YourReservations
                            }
                        , dropdownEntry
                            { attributes = [ Element.alignBottom ]
                            , text = "Sign out"
                            , message = SignOut
                            }
                        ]
                    ]

            Mensam.Auth.SignedOut ->
                Element.none


{-| Close dropdown when there is a click outside.
-}
dropdownWrapper : Content -> Element.Element Message -> Element.Element Message
dropdownWrapper content insideElement =
    if content.unfoldDropdownMenu then
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ Element.row
                [ Element.width Element.fill
                , Element.height <| Element.px 200
                , Element.alignTop
                ]
                [ Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.alignLeft
                    , Element.Events.onClick CloseDropdown
                    ]
                    []
                , Element.column
                    [ Element.Background.color Mensam.Element.Color.bright.black
                    , Element.width <| Element.px 200
                    , Element.height Element.fill
                    , Element.alignRight
                    , Element.clip
                    ]
                    [ insideElement ]
                ]
            , Element.row
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.alignBottom
                , Element.Events.onClick CloseDropdown
                ]
                []
            ]

    else
        Element.none


dropdownEntry :
    { attributes : List (Element.Attribute Message)
    , text : String
    , message : Message
    }
    -> Element.Element Message
dropdownEntry entry =
    Element.Input.button
        ([ Element.width Element.fill
         , Element.padding 10
         , Element.mouseOver [ Element.Background.color <| Element.rgba 1 1 1 0.1 ]
         ]
            ++ entry.attributes
        )
        { onPress = Just entry.message
        , label =
            Element.el
                [ Element.centerX
                , Element.centerY
                ]
            <|
                Element.text entry.text
        }
