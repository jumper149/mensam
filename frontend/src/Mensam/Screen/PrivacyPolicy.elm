module Mensam.Screen.PrivacyPolicy exposing (..)

import Element
import Element.Font
import Mensam.Element.Color


type alias Model =
    ()


init : Model
init =
    ()


element : Model -> Element.Element Message
element () =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 6
        ]
    <|
        Element.column
            [ Element.width Element.fill
            , Element.padding 2
            , Element.spacing 6
            ]
            [ section
                [ h1 "Privacy Policy"
                , p
                    [ "Mensam (\"we\", \"our\", or \"us\") is committed to protecting your privacy."
                    , "This Privacy Policy explains how we collect, use, disclose, and safeguard your information when you visit our website or use our service."
                    , "By accessing or using the Service, you agree to the collection and use of information in accordance with this Privacy Policy."
                    ]
                ]
            , section
                [ h2 "Information we collect"
                , p
                    [ "We collect and save several types of information from users of our Service. This includes:"
                    ]
                , list
                    [ "IP address"
                    , "Email address"
                    , "Username"
                    , "Date and time of access"
                    , "Your timezone"
                    , "Reservations (date, time and location)"
                    , "Space metadata (whatever you enter)"
                    , "Relationships between spaces and users"
                    ]
                , p
                    [ "This collected information can mostly be edited directly by yourself."
                    , "Data related to reservations cannot be deleted without affecting other users."
                    , "When you make a reservation you give the control of that data to the administrator of that particular space."
                    ]
                ]
            , section
                [ h2 "Information on your Machine"
                , p
                    [ "We store login related data in your browsers \"localStorage\"."
                    ]
                ]
            , section
                [ h2 "How we use your Information"
                , p
                    [ "We use the information we collect for various purposes, including:"
                    ]
                , list
                    [ "To manage your account."
                    , "To make your space and desks available."
                    , "To process your reservations."
                    , "To monitor and analyze usage and trends to improve user experience."
                    , "To detect, prevent, and address technical issues or fraud."
                    , "To comply with legal obligations and enforce our terms and conditions."
                    ]
                ]
            , section
                [ h2 "Legal Basis for Processing your Data"
                , p
                    [ "We process your personal data based on the following legal grounds:"
                    ]
                , list
                    [ "Consent: Where you have given us clear consent to process your personal data for specific purposes."
                    , "Contractual Necessity: To provide the Service and fulfill our obligations in any contracts with you."
                    , "Legitimate Interests: For the purposes of improving our Service, monitoring usage, and maintaining security."
                    , "Legal Obligations: To comply with legal requirements or respond to lawful requests from authorities."
                    ]
                ]
            , section
                [ h2 "Sharing and Disclosure of Information"
                , p
                    [ "We do not sell, trade, or rent your personal information to third parties. However, we may share your information in the following situations."
                    ]
                , list
                    [ "We may share your information with third-party vendors, service providers, and business partners who perform services on our behalf, such as payment processing, data hosting, or customer support."
                    , "We may disclose your information if required by law or in response to valid requests by public authorities (e.g., a court or government agency)."
                    , "In the event of a merger, acquisition, or asset."
                    ]
                ]
            ]


section : List (Element.Element Message) -> Element.Element Message
section =
    Element.textColumn
        [ Element.width Element.fill
        , Element.padding 5
        , Element.spacing 5
        , Element.Font.color <| Mensam.Element.Color.bright.white Mensam.Element.Color.Opaque100
        , Element.Font.size 15
        , Element.Font.extraLight
        ]


h1 : String -> Element.Element Message
h1 x =
    Element.paragraph [ Element.width Element.fill, Element.Font.alignLeft, Element.Font.size 30 ] [ Element.text x ]


h2 : String -> Element.Element Message
h2 x =
    Element.paragraph [ Element.width Element.fill, Element.Font.alignLeft, Element.Font.size 22 ] [ Element.text x ]


p : List String -> Element.Element Message
p =
    Element.paragraph [ Element.width Element.fill, Element.Font.justify ] << List.map Element.text << List.intersperse " "


list : List String -> Element.Element Message
list =
    Element.textColumn
        [ Element.width Element.fill
        , Element.spacing 5
        , Element.paddingEach { top = 0, right = 0, bottom = 0, left = 10 }
        ]
        << List.map
            (\text ->
                Element.row [ Element.width Element.fill ]
                    [ Element.el [ Element.alignLeft, Element.alignTop ] <| Element.text "- "
                    , Element.paragraph
                        [ Element.width Element.fill
                        , Element.alignLeft
                        , Element.alignTop
                        , Element.Font.justify
                        ]
                      <|
                        List.singleton <|
                            Element.text
                                text
                    ]
            )


type Message
    = MessageEffect MessageEffect


type MessageEffect
    = AbsurdMessage Never
