module Mensam.Application exposing (..)

import Platform.Cmd


updates : List (model -> ( model, Platform.Cmd.Cmd message )) -> model -> ( model, Platform.Cmd.Cmd message )
updates messages model =
    case messages of
        [] ->
            ( model, Platform.Cmd.none )

        updateNow :: otherMessages ->
            let
                ( modelUpdated, cmdUpdated ) =
                    updateNow model

                ( modelFinal, cmdFinal ) =
                    updates otherMessages modelUpdated
            in
            ( modelFinal, Platform.Cmd.batch [ cmdUpdated, cmdFinal ] )
