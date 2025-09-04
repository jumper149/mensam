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
            , zoom = MkZoomLevel config.viewportInitialZoom
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
    | MessageZoomIn
    | MessageZoomOut


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

        MessageZoomIn ->
            { model | state = zoom { minZoom = MkZoomLevel <| 1 / model.config.viewportZoomMaximumMagnification, maxZoom = MkZoomLevel model.config.viewportZoomMaximumMagnification, step = model.config.viewportZoomStep } model.state }

        MessageZoomOut ->
            { model | state = zoom { minZoom = MkZoomLevel <| 1 / model.config.viewportZoomMaximumMagnification, maxZoom = MkZoomLevel model.config.viewportZoomMaximumMagnification, step = 1 / model.config.viewportZoomStep } model.state }


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
                        :: (listWhen (x /= 0 || y /= 0)
                                -- Move center point to position
                                [ "translate(" ++ String.fromFloat x ++ "px ," ++ String.fromFloat y ++ "px)" ]
                                ++ -- Set the zoom level
                                   [ "scale(" ++ String.fromFloat (zoomFactor state.zoom) ++ ")" ]
                           )
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


type State
    = State
        { position : Coordinate -- Position in viewport coordinate system
        , zoom : ZoomLevel
        , draggingPointerPosition : Maybe PointerPosition
        }


movePositionByDelta : Coordinate -> State -> State
movePositionByDelta delta (State state) =
    State { state | position = coordinatePlus state.position delta }


setDraggingPointerPosition : Maybe PointerPosition -> State -> State
setDraggingPointerPosition mMousePosition (State state) =
    State { state | draggingPointerPosition = mMousePosition }


zoom : { minZoom : ZoomLevel, maxZoom : ZoomLevel, step : Float } -> State -> State
zoom { minZoom, maxZoom, step } (State state) =
    State
        { state
            | zoom =
                let
                    newZoom =
                        zoomFactor state.zoom * step
                in
                if newZoom < zoomFactor minZoom then
                    minZoom

                else if newZoom > zoomFactor maxZoom then
                    maxZoom

                else
                    MkZoomLevel newZoom
        }



-- Utilities


toViewportCoordinates : Config -> Coordinate -> Coordinate
toViewportCoordinates { viewportOffset } position =
    { x = position.x - viewportOffset.x
    , y = position.y - viewportOffset.y
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


type ZoomLevel
    = MkZoomLevel Float


zoomFactor : ZoomLevel -> Float
zoomFactor (MkZoomLevel zoomLevel) =
    zoomLevel


type alias Config =
    { viewportOffset : { x : Float, y : Float }
    , viewportInitialZoom : Float
    , viewportZoomStep : Float -- A factor that is applied to the zoom level.
    , viewportZoomMaximumMagnification : Float
    }


defaultConfig : Config
defaultConfig =
    { viewportOffset = { x = 0, y = 0 }, viewportInitialZoom = 1, viewportZoomStep = 1.1, viewportZoomMaximumMagnification = 4 }


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


onLeave : (Message -> msg) -> Element.Attribute msg
onLeave handler =
    Element.Events.Pointer.onLeave <|
        \_ ->
            handler MessageUp
