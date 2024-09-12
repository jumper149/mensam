module Element.Window exposing (..)

import Element
import Element.Events.Pointer
import Html as H
import Html.Attributes as HA


init : Config -> { position : Coordinate } -> Model
init config { position } =
    { config = config
    , state =
        State
            { position = position |> toViewportCoordinates config
            , draggingPointerPosition = Nothing
            }
    }


type alias Model =
    { state : State
    , config : Config
    }


type Message
    = MessageDown PointerPosition
    | MessageMove PointerPosition
    | MessageUp


update : Message -> Model -> Model
update mouseEvent model =
    let
        (State state) =
            model.state
    in
    case mouseEvent of
        MessageDown pos ->
            { model
                | state = setDraggingPointerPosition (Just pos) model.state
            }

        MessageMove pos ->
            case state.draggingPointerPosition of
                Nothing ->
                    model

                Just oldPos ->
                    { model
                        | state =
                            movePositionByDelta (pointerPositionToTranslation { old = oldPos, new = pos }) <|
                                setDraggingPointerPosition (Just pos) model.state
                    }

        MessageUp ->
            { model | state = model.state |> setDraggingPointerPosition Nothing }


view : Model -> { viewportAttributes : List (Element.Attribute msg), contentAttributes : List (Element.Attribute msg) } -> Element.Element msg -> Element.Element msg
view model { viewportAttributes, contentAttributes } content =
    let
        (State state) =
            model.state

        transform : Coordinate -> H.Attribute msg
        transform { x, y } =
            HA.style "transform" <|
                String.join " " <|
                    -- Center the content box
                    "translate(-50%, -50%)"
                        :: listWhen (x /= 0 || y /= 0)
                            -- Move center point to position
                            [ "translate(" ++ String.fromFloat x ++ "px ," ++ String.fromFloat y ++ "px)" ]
    in
    Element.el
        ([ Element.htmlAttribute <| HA.style "overflow" "hidden"
         , Element.htmlAttribute <| HA.style "box-sizing" "border-box"
         ]
            ++ viewportAttributes
        )
    <|
        Element.el
            ([ Element.htmlAttribute <| transform state.position
             , Element.htmlAttribute <| HA.style "box-sizing" "border-box"
             ]
                ++ contentAttributes
            )
            content



-- Internal state


{-| Opaque internal state.

Values inside can be accessed with [getters](#getters) e.g. [`getScale`](PanZoom#getScale).

-}
type State
    = State
        { position : Coordinate -- Position in viewport coordinate system
        , draggingPointerPosition : Maybe PointerPosition
        }


movePositionByDelta : Coordinate -> State -> State
movePositionByDelta delta (State state) =
    State { state | position = coordinatePlus state.position delta }


setDraggingPointerPosition : Maybe PointerPosition -> State -> State
setDraggingPointerPosition mMousePosition (State state) =
    State { state | draggingPointerPosition = mMousePosition }



-- Utilities


toViewportCoordinates : Config -> Coordinate -> Coordinate
toViewportCoordinates { viewportOffset } position =
    { x = position.x - viewportOffset.x
    , y = position.y - viewportOffset.y
    }


fromViewportCoordinates : Config -> Coordinate -> Coordinate
fromViewportCoordinates { viewportOffset } position =
    { x = position.x + viewportOffset.x
    , y = position.y + viewportOffset.y
    }


type alias Coordinate =
    { x : Float
    , y : Float
    }


type PointerPosition
    = MkPointerPosition Coordinate


listWhen : Bool -> List a -> List a
listWhen cond l =
    if cond then
        l

    else
        []


coordinatePlus : Coordinate -> Coordinate -> Coordinate
coordinatePlus a b =
    { x = a.x + b.x, y = a.y + b.y }


coordinateMinus : Coordinate -> Coordinate -> Coordinate
coordinateMinus a b =
    { x = a.x - b.x, y = a.y - b.y }


pointerPositionToTranslation : { old : PointerPosition, new : PointerPosition } -> Coordinate
pointerPositionToTranslation positions =
    case ( positions.old, positions.new ) of
        ( MkPointerPosition old, MkPointerPosition new ) ->
            coordinateMinus new old


type alias Config =
    { viewportOffset : { x : Float, y : Float }
    }


defaultConfig : Config
defaultConfig =
    { viewportOffset = { x = 0, y = 0 } }


onDown : (Message -> msg) -> Element.Attribute msg
onDown handler =
    Element.Events.Pointer.onDown <|
        \event ->
            handler <| MessageDown <| MkPointerPosition event.pagePos


onMove : (Message -> msg) -> Element.Attribute msg
onMove handler =
    Element.Events.Pointer.onMove <|
        \event ->
            handler <| MessageMove <| MkPointerPosition event.pagePos


onUp : (Message -> msg) -> Element.Attribute msg
onUp handler =
    Element.Events.Pointer.onUp <|
        \_ ->
            handler MessageUp
