module Mensam.Spaces exposing (..)

import Element
import Mensam.Api.SpaceList
import Mensam.Jwt


type alias Model =
    { spaces : List { id : Int, name : String } }


init : Model
init =
    { spaces = [] }


element : Model -> Element.Element Message
element model =
    Element.el
        [ Element.width Element.fill
        ]
    <|
        Element.table
            []
            { data = model.spaces
            , columns =
                [ { header = Element.text "ID"
                  , width = Element.px 100
                  , view = \x -> Element.text <| Debug.toString x.id
                  }
                , { header = Element.text "Name"
                  , width = Element.fill
                  , view = \x -> Element.text <| Debug.toString x.name
                  }
                ]
            }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect


type MessagePure
    = SetSpaces (List { id : Int, name : String })


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetSpaces spaces ->
            { model | spaces = spaces }


type MessageEffect
    = ReportError String
    | RefreshSpaces


spaceList : Mensam.Jwt.Jwt -> Cmd Message
spaceList jwt =
    Mensam.Api.SpaceList.request { jwt = jwt, order = [] } <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceList.Success value) ->
                    MessagePure <| SetSpaces value.spaces

                Ok (Mensam.Api.SpaceList.ErrorBody error) ->
                    MessageEffect <| ReportError <| Debug.toString error

                Ok (Mensam.Api.SpaceList.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Debug.toString error

                Err err ->
                    MessageEffect <| ReportError <| Debug.toString err
