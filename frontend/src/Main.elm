module Main exposing (..)

import Browser
import Debug
import Html
import Html.Events
import Html.Attributes

main : Program () Model Message
main =
  Browser.sandbox { init = init, update = update, view = view }

type Model = MkModel {username : String, password : String, output : String}

init : Model
init = MkModel {username = "", password = "", output = ""}

type Message
  = EnterUsername String
  | EnterPassword String
  | SubmitLogin

update : Message -> Model -> Model
update msg (MkModel model) =
  case msg of
    EnterUsername x ->
      MkModel { model | username = x }
    EnterPassword x ->
      MkModel { model | password = x }
    SubmitLogin ->
      MkModel { model | output = Debug.toString model }

view : Model -> Html.Html Message
view (MkModel model) =
  Html.div []
    [ Html.form [ Html.Events.onSubmit SubmitLogin ]
          [ Html.fieldset [ ]
              [ Html.input
                  [ Html.Events.onInput EnterUsername
                  , Html.Attributes.type_ "text"
                  , Html.Attributes.placeholder "Username"
                  ]
                  []
              ]
          , Html.fieldset [ ]
              [ Html.input
                  [ Html.Events.onInput EnterPassword
                  , Html.Attributes.type_ "password"
                  , Html.Attributes.placeholder "Password"
                  ]
                  []
              ]
          , Html.button []
              [ Html.text "Login" ]
          ]
    ,  Html.text (model.output)
    ]
