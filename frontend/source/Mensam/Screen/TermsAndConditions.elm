module Mensam.Screen.TermsAndConditions exposing (..)

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
                [ h1 "Terms and Conditions"
                , p
                    [ "Welcome to Mensam (Desk-Booking Web Application)."
                    , "These Terms and Conditions (\"Terms\") govern your use of this web application."
                    , "Please read these Terms carefully before using this Service."
                    , "By accessing or using our Service, you agree to be bound by these Terms."
                    ]
                , Element.paragraph [ Element.Font.italic ]
                    [ Element.text
                        "Unless you agree to all Terms, please do not use the Service."
                    ]
                ]
            , section
                [ h2 "Account Registration"
                , p
                    [ "To use the Service, you must create an account."
                    , "You are responsible for maintaining the confidentiality of your login information."
                    , "You must not misuse your account for purposes that do not align with this service and might worsen the experience of other users."
                    ]
                , p
                    [ "We do not actively censor data or care about underage users."
                    , "The internet is a dangerous place and we are not the superheroes that suddenly change that fact."
                    , "You take the responsibility to interact with other users of this Service."
                    ]
                ]
            , section
                [ h2 "Usage"
                , p
                    [ "Spaces are governed by their respective administrators."
                    , "You can create your own spaces, where you make the rules."
                    ]
                , p
                    [ "You can use your profile to identify yourself."
                    , "Consider that your user profile is generally public."
                    , "Some privacy features are implemented, but to not disappoint you, you should think of all the information that you entered as public information."
                    ]
                , p
                    [ "Reservations can be made by you in any place that you are a member of."
                    , "Consider that reservations contain sensitiv information that cannot be deleted anymore, as this would affect the user experience of other users."
                    ]
                ]
            , section
                [ h2 "Termination"
                , p
                    [ "We may suspend or terminate your access to the Service at any time, with or without cause, including but not limited to violation of these Terms."
                    , "Upon termination, your right to use this Service will immediately cease, and any outstanding obligations will remain due."
                    ]
                ]
            , section
                [ h2 "Privacy Policy"
                , Element.paragraph [ Element.Font.justify ]
                    [ Element.text "Your use of the Service is also governed by our Privacy Policy, which outlines how we collect, use, and protect your personal information."
                    , Element.text " "
                    , Element.text "Please review our "
                    , Element.newTabLink []
                        { url = "./privacy"
                        , label =
                            Element.el
                                [ Element.Font.color <| Mensam.Element.Color.bright.blue Mensam.Element.Color.Opaque100
                                , Element.mouseOver
                                    [ Element.Font.color <| Mensam.Element.Color.bright.cyan Mensam.Element.Color.Opaque100
                                    ]
                                ]
                            <|
                                Element.text "privacy policy"
                        }
                    , Element.text "."
                    , Element.text " "
                    , Element.text "By using the Service, you consent to the collection and use of your data as described in the Privacy Policy."
                    ]
                ]
            , section
                [ h2 "Limitation of Liability"
                , p
                    [ "The Service is provided on an \"as-is\" and \"as-available\" basis."
                    , "We make no warranties, whether express or implied, regarding the availability, suitability, or fitness of the Service for any purpose."
                    ]
                , p
                    [ "Mensam shall not be liable for any direct, indirect, incidental, consequential, or punitive damages resulting from your use of, or inability to use, the Service, including but not limited to loss of data, revenue, or business."
                    , "We are not responsible for any damages or losses incurred as a result of your use of third-party workspaces."
                    ]
                ]
            , section
                [ h2 "Indemnification"
                , p
                    [ "You agree to indemnify and hold harmless this provider, its affiliates, the contributors and everyone else helping to provide this service from any claims, losses, damages, liabilities, or expenses arising from your use of the Service, including but not limited to any violation of these Terms."
                    ]
                ]

            -- TODO: Section about "Governing Law and Dispute Resolution"
            , section
                [ h2 "Modifications to Service and Terms"
                , p
                    [ "We reserve the right to modify or discontinue the Service (or any part thereof) at any time with or without notice."
                    , "We may update these Terms from time to time. Any changes to the Terms will be posted on this page, and your continued use of the Service constitutes acceptance of the updated Terms."
                    ]
                ]
            , section
                [ h2 "Contact Information"
                , p
                    [ "If you have any questions or concerns about these Terms, please contact us."
                    ]

                -- TODO: Add contact information.
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
    Element.paragraph [ Element.Font.alignLeft, Element.Font.size 30 ] [ Element.text x ]


h2 : String -> Element.Element Message
h2 x =
    Element.paragraph [ Element.Font.alignLeft, Element.Font.size 22 ] [ Element.text x ]


p : List String -> Element.Element Message
p =
    Element.paragraph [ Element.Font.justify ] << List.map Element.text << List.intersperse " "


type Message
    = MessageEffect MessageEffect


type MessageEffect
    = AbsurdMessage Never
