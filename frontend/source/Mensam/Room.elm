module Mensam.Room exposing (..)

import Element
import Mensam.Svg.Color
import Svg
import Svg.Attributes


type Room
    = MkRoom
        { dimensions : { minX : Int, minY : Int, maxX : Int, maxY : Int }
        , drawingInstructions : List DrawingInstructions
        }


type DrawingInstructions
    = DrawTable { position : Position, direction : Direction }


{-| A point on a 2D grid.
-}
type Position
    = MkPosition { x : Int, y : Int }


getX : Position -> Int
getX (MkPosition position) =
    position.x


getY : Position -> Int
getY (MkPosition position) =
    position.y


{-| A direction on a 2D grid.
The value `degrees` should satisfy `0 <= degrees < 360`.

Here are some example values to get an idea:

  - North: 0
  - East: 90
  - South: 180
  - West: 270

-}
type Direction
    = MkDirection { degrees : Int }


drawRoom : Room -> Element.Element msg
drawRoom (MkRoom room) =
    Element.html <|
        Svg.svg
            [ Svg.Attributes.width "100"
            , Svg.Attributes.height "100"
            , Svg.Attributes.viewBox <|
                String.concat
                    [ String.fromInt room.dimensions.minX
                    , " "
                    , String.fromInt room.dimensions.minY
                    , " "
                    , String.fromInt <| room.dimensions.maxX - room.dimensions.minX
                    , " "
                    , String.fromInt <| room.dimensions.maxY - room.dimensions.minY
                    ]
            ]
        <|
            List.concatMap drawInstruction room.drawingInstructions


drawInstruction : DrawingInstructions -> List (Svg.Svg msg)
drawInstruction instruction =
    case instruction of
        DrawTable { position, direction } ->
            [ Svg.rect
                [ Svg.Attributes.x <| String.fromInt <| getX position
                , Svg.Attributes.y <| String.fromInt <| getY position
                , Svg.Attributes.width "10"
                , Svg.Attributes.height "5"
                , Svg.Attributes.fill Mensam.Svg.Color.bright.white
                ]
                []
            ]


example : Room
example =
    MkRoom
        { dimensions = { minX = -50, minY = -50, maxX = 50, maxY = 50 }
        , drawingInstructions =
            [ DrawTable { position = MkPosition { x = 0, y = 0 }, direction = MkDirection { degrees = 0 } }
            , DrawTable { position = MkPosition { x = 20, y = 0 }, direction = MkDirection { degrees = 90 } }
            , DrawTable { position = MkPosition { x = 20, y = -20 }, direction = MkDirection { degrees = 90 } }
            ]
        }
